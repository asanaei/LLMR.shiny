# Token usage that a reply does not report is unknown, not zero: the sums
# cover the replies that report usage, and `unknown` counts the others.

calls <- function() {
  data.frame(response_id = paste0("r", 1:4),
             sent_tokens = c(100L, 110L, 120L, 130L),
             rec_tokens = c(20L, 21L, 22L, 23L),
             total_tokens = c(120L, 131L, 142L, 153L))
}

test_that("fully reported usage and known zeros are summed", {
  expect_identical(
    extract_token_counts(calls()),
    list(calls = 4L, sent = 460L, received = 86L, total = 546L, unknown = 0L)
  )
  z <- calls()
  z[c("sent_tokens", "rec_tokens", "total_tokens")] <- 0L
  expect_identical(extract_token_counts(z)[c("sent", "total", "unknown")],
                   list(sent = 0L, total = 0L, unknown = 0L))
})

test_that("absent or all-missing token columns are unknown, not zero", {
  a <- calls()
  a[c("sent_tokens", "rec_tokens", "total_tokens")] <- NA_integer_
  expect_identical(
    extract_token_counts(a)[c("sent", "received", "total", "unknown")],
    list(sent = NA_integer_, received = NA_integer_, total = NA_integer_,
         unknown = 4L)
  )
  expect_identical(extract_token_counts(calls()["response_id"])$unknown, 4L)
  lg <- calls()["response_id"]
  lg$sent_tokens <- NA
  expect_identical(extract_token_counts(lg)$unknown, 4L)
})

test_that("mixed reporting sums the known rows and counts the rest", {
  m <- calls()
  m[2, c("sent_tokens", "rec_tokens", "total_tokens")] <- NA
  expect_identical(
    extract_token_counts(m)[c("sent", "received", "total", "unknown")],
    list(sent = 350L, received = 65L, total = 415L, unknown = 1L)
  )
  f <- m
  f$success <- c(TRUE, FALSE, TRUE, TRUE)
  f$response_id[2] <- NA
  failed <- extract_token_counts(f)
  expect_identical(failed$unknown, 0L)
  expect_identical(failed$calls, 4L)
})

test_that("with no result frame, the fallback calls have unknown usage", {
  expect_identical(
    extract_token_counts(NULL, fallback_calls = 3),
    list(calls = 3L, sent = NA_integer_, received = NA_integer_,
         total = NA_integer_, unknown = 3L)
  )
  expect_identical(extract_token_counts(NULL)[c("calls", "sent", "unknown")],
                   list(calls = 0L, sent = 0L, unknown = 0L))
  expect_error(extract_token_counts(NULL, fallback_calls = NA),
               "nonnegative whole")
})

test_that("a summary listed first does not stand in for the call table", {
  summaries <- list(data.frame(n_calls = 4L, success_rate = 1),
                    data.frame(sent_tokens = 460L, rec_tokens = 86L))
  for (summ in summaries) {
    out <- extract_token_counts(list(summary = summ, calls = calls()))
    expect_identical(out$calls, 4L)
    expect_identical(out$total, 546L)
  }
})

test_that("incomplete identifiers count result rows, with a warning", {
  p <- calls()
  p$response_id[c(2, 4)] <- NA
  expect_warning(out <- extract_token_counts(p),
                 "2 of 4 rows have no `response_id`", fixed = TRUE)
  expect_identical(out$result_rows, 4L)
  expect_null(out$calls)
  c2 <- calls()
  c2$call_id <- c("x", NA, NA, NA)
  expect_identical(extract_token_counts(c2)$calls, 4L)
})

test_that("retries sharing a request ID count as one request", {
  r <- data.frame(request_id = c("q1", "q1", "q2"),
                  sent_tokens = c(5L, 5L, 6L), rec_tokens = c(1L, 2L, 3L))
  out <- extract_token_counts(r)
  expect_identical(out$calls, 2L)
  expect_identical(out$total, 22L)
})

test_that("non-numeric token columns are not read as counts", {
  f <- calls()
  f$sent_tokens <- factor(f$sent_tokens)
  expect_warning(out <- extract_token_counts(f), "is factor, not numeric")
  expect_identical(out$sent, NA_integer_)
  expect_identical(out$unknown, 4L)
})

test_that("demonstration rows add result rows but no tokens", {
  d <- demo_runner()(data.frame(text = c("hello there", "general kenobi")))
  expect_identical(extract_token_counts(d)[c("result_rows", "sent", "unknown")],
                   list(result_rows = 2L, sent = 0L, unknown = 0L))
  keep <- c("response_id", "sent_tokens", "rec_tokens", "total_tokens",
            "run_mode")
  live <- data.frame(response_id = "live-1", sent_tokens = 50L,
                     rec_tokens = 10L, total_tokens = 60L, run_mode = "live")
  out <- extract_token_counts(rbind(as.data.frame(d)[keep], live))
  expect_identical(out[c("calls", "result_rows", "sent", "total")],
                   list(calls = 1L, result_rows = 2L, sent = 50L, total = 60L))
})

test_that("usage_add keeps unknown usage out of the sums and counts it", {
  s <- usage_add(usage_empty(), list(calls = 27L, sent = 2700L, received = 540L))
  expect_identical(s$total, 3240L)
  expect_identical(s$unknown, 0L)
  s <- usage_add(s, list(calls = 2L))
  expect_identical(s[c("calls", "sent", "total", "unknown")],
                   list(calls = 29L, sent = 2700L, total = 3240L, unknown = 2L))
  p <- usage_add(usage_empty(), list(calls = 1L, sent = NA, received = 5L))
  expect_identical(p[c("sent", "received", "total", "unknown")],
                   list(sent = 0L, received = 5L, total = 0L, unknown = 1L))
  e <- usage_add(usage_empty(), extract_token_counts(NULL, fallback_calls = 4))
  expect_identical(e$unknown, 4L)
  old <- usage_empty()
  old$unknown <- NULL
  expect_identical(
    usage_add(old, list(calls = 1, sent = 3, received = 4))$unknown, 0L
  )
  expect_warning(usage_add(usage_empty(), list(calls = 1, sent = "3")),
                 "not a single number")
})

test_that("the usage tile never shows unknown usage as zero", {
  tile <- function(s) paste(as.character(usage_tile(s)), collapse = " ")
  expect_match(tile(usage_empty()), "Sent 0 / Received 0 tokens", fixed = TRUE)
  none <- usage_add(usage_empty(), list(calls = 4L))
  expect_match(tile(none), "Token usage unknown", fixed = TRUE)
  expect_match(tile(none), "Usage was not reported for 4 replies.", fixed = TRUE)
  some <- usage_add(usage_empty(), list(calls = 2, sent = 120, received = 40))
  some <- usage_add(some, list(calls = 1))
  expect_match(tile(some), "Sent at least 120 / Received at least 40 tokens",
               fixed = TRUE)
  part <- usage_add(usage_empty(), list(calls = 1L, sent = NA, received = 5L))
  expect_match(tile(part), "Sent unknown / Received at least 5 tokens",
               fixed = TRUE)
  expect_no_match(tile(part), "Sent 0", fixed = TRUE)
})
