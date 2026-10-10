usage_calls <- function() {
  data.frame(response_id = paste0("r", 1:4),
             sent_tokens = c(100, 110, 120, 130),
             rec_tokens = c(20, 21, 22, 23),
             total_tokens = c(120, 131, 142, 153))
}

token_values <- function(x) x[c("sent", "received", "total")]
usage_html <- function(x) paste(as.character(usage_tile(x)), collapse = " ")

test_that("reported counts and known zeros have double subtotals", {
  expect_identical(extract_token_counts(usage_calls()),
                   list(calls = 4L, sent = 460, received = 86, total = 546,
                        unknown = 0L, sent_unknown = 0L, received_unknown = 0L))
  z <- usage_calls()
  z[c("sent_tokens", "rec_tokens", "total_tokens")] <- 0L
  expect_identical(token_values(extract_token_counts(z)),
                   list(sent = 0, received = 0, total = 0))
})

test_that("absent and all-missing columns retain all three unknown values", {
  missing <- usage_calls()
  missing[c("sent_tokens", "rec_tokens", "total_tokens")] <- NA_integer_
  logical_na <- usage_calls()["response_id"]
  logical_na$sent_tokens <- NA
  for (d in list(missing, usage_calls()["response_id"], logical_na)) {
    out <- extract_token_counts(d)
    expect_identical(token_values(out),
                     list(sent = NA_real_, received = NA_real_, total = NA_real_))
    expect_identical(out$unknown, 4L)
    expect_identical(out$sent_unknown, 4L)
    expect_identical(out$received_unknown, 4L)
  }
})

test_that("failed calls retain reported usage and count missing usage", {
  d <- usage_calls()
  d[2, c("sent_tokens", "rec_tokens", "total_tokens")] <- NA
  d$success <- c(TRUE, FALSE, FALSE, TRUE)
  d$response_id[2] <- NA
  out <- extract_token_counts(d)
  expect_identical(token_values(out), list(sent = 350, received = 65, total = 415))
  expect_identical(out$unknown, 1L)
  expect_identical(out$calls, 4L)
  failed <- extract_token_counts(d[2, ])
  expect_identical(token_values(failed),
                   list(sent = NA_real_, received = NA_real_, total = NA_real_))
  expect_identical(failed$unknown, 1L)
  expect_identical(failed$calls, 1L)
})

test_that("fallback counts and empty frames distinguish no calls from unknown usage", {
  expect_identical(extract_token_counts(NULL, 3),
                   list(calls = 3L, sent = NA_real_, received = NA_real_,
                        total = NA_real_, unknown = 3L,
                        sent_unknown = 3L, received_unknown = 3L))
  for (x in list(NULL, usage_calls()[FALSE, ], data.frame())) {
    out <- extract_token_counts(x)
    expect_identical(token_values(out), list(sent = 0, received = 0, total = 0))
    expect_identical(out$unknown, 0L)
    expect_equal(out$calls %||% out$result_rows, 0L)
  }
  expect_warning(out <- extract_token_counts(NULL, NA), "recorded as unknown")
  expect_identical(out$calls, NA_integer_)
  expect_identical(token_values(out),
                   list(sent = NA_real_, received = NA_real_, total = NA_real_))
  for (n in c(-1, 0.5, Inf, -Inf, NaN, .Machine$integer.max + 1)) {
    expect_error(extract_token_counts(NULL, n), "nonnegative whole")
  }
})

test_that("identifier frames outrank token summaries and ties warn", {
  summary <- data.frame(sent_tokens = 460, rec_tokens = 86)
  out <- extract_token_counts(list(summary = summary, calls = usage_calls()["response_id"]))
  expect_identical(out$calls, 4L)
  expect_identical(token_values(out),
                   list(sent = NA_real_, received = NA_real_, total = NA_real_))
  expect_identical(out$unknown, 4L)
  expect_identical(extract_token_counts(list(summary, usage_calls()))$total, 546)
  expect_warning(out <- extract_token_counts(list(first = usage_calls(), second = usage_calls()[1, ])),
                 "first, second; using first", fixed = TRUE)
  expect_identical(out$calls, 4L)
  expect_warning(out <- extract_token_counts(list(NULL, usage_calls(), usage_calls()[1, ])),
                 "2, 3; using 2", fixed = TRUE)
  expect_identical(out$calls, 4L)
})

test_that("incomplete identifiers include empty list entries and all-missing columns", {
  d <- usage_calls()
  d$response_id[c(2, 4)] <- NA
  expect_warning(out <- extract_token_counts(d), "2 of 4 rows have no `response_id`", fixed = TRUE)
  expect_null(out$calls)
  expect_identical(out$result_rows, 4L)
  d$response_id <- NA_character_
  expect_warning(out <- extract_token_counts(d), "4 of 4 rows")
  expect_identical(out$result_rows, 4L)
  d$success <- FALSE
  expect_no_warning(out <- extract_token_counts(d))
  expect_identical(out$calls, 4L)
  d <- usage_calls()[1:2, ]
  for (id in list(c("a", NA), c("a", ""), character(), list("a", NULL))) {
    d$response_id <- list(id, "b")
    expect_warning(out <- extract_token_counts(d), "1 of 2 rows")
    expect_identical(out$result_rows, 2L)
    d$success <- c(FALSE, TRUE)
    expect_no_warning(out <- extract_token_counts(d))
    expect_identical(out$calls, 2L)
    d$success <- NULL
  }
  d$call_id <- c(NA, "x")
  d$response_id <- c("a", "b")
  expect_no_warning(out <- extract_token_counts(d))
  expect_identical(out$calls, 2L)
  d$call_id <- NA_character_
  d$response_id <- c("shared", "shared")
  d$success <- FALSE
  expect_identical(extract_token_counts(d)$calls, 1L)
})

test_that("shared identifiers count requests and retain reported retry tokens", {
  d <- data.frame(request_id = c("q1", "q1", "q2"),
                  sent_tokens = c(5, 5, 6), rec_tokens = c(1, 2, 3))
  expect_identical(extract_token_counts(d)$calls, 2L)
  expect_identical(extract_token_counts(d)$total, 22)
})

test_that("packed missing values require the usage-bearing row in the same frame", {
  d <- data.frame(response_id = c("r1", "r1"), rowpack_id = c("p1", "p1"),
                  rowpack_row = 1:2, sent_tokens = c(10, NA), rec_tokens = c(2, NA))
  full <- extract_token_counts(d)
  expect_identical(full$calls, 1L)
  expect_identical(token_values(full), list(sent = 10, received = 2, total = 12))
  expect_identical(full[c("unknown", "sent_unknown", "received_unknown")],
                   list(unknown = 0L, sent_unknown = 0L, received_unknown = 0L))
  follow <- extract_token_counts(d[2, ])
  expect_identical(follow$calls, 1L)
  expect_identical(token_values(follow),
                   list(sent = NA_real_, received = NA_real_, total = NA_real_))
  expect_identical(follow$unknown, 1L)
  d$rowpack_id[1] <- "other"
  expect_identical(extract_token_counts(d)$unknown, 1L)
  d$rowpack_id <- NULL
  expect_identical(extract_token_counts(d)$unknown, 1L)
})

test_that("invalid values warn once per column and remain unknown", {
  d <- usage_calls()
  d$sent_tokens <- factor(d$sent_tokens)
  expect_warning(out <- extract_token_counts(d), "is factor, not numeric")
  expect_identical(out$sent, NA_real_)
  expect_identical(out$sent_unknown, 4L)
  d$sent_tokens <- c(-1, 1.5, Inf, NaN)
  d$total_tokens <- NULL
  expect_warning(out <- extract_token_counts(d), "`sent_tokens` contains invalid counts", fixed = TRUE)
  expect_identical(token_values(out), list(sent = NA_real_, received = 86, total = NA_real_))
  expect_identical(out$unknown, 4L)
  for (value in list(-1, 1.5, Inf, -Inf, NaN, "10", c(1, 2))) {
    expect_warning(s <- usage_add(NULL, list(calls = 1L, sent = value, received = 2)),
                   "tokens$sent", fixed = TRUE)
    expect_identical(s$sent, 0)
    expect_identical(s$sent_unknown, 1L)
  }
})

test_that("large token sums remain doubles and render without scientific notation", {
  d <- data.frame(response_id = paste0("r", seq_len(3000)),
                  sent_tokens = 1e6, rec_tokens = 1)
  out <- extract_token_counts(d)
  expect_identical(token_values(out), list(sent = 3e9, received = 3000, total = 3000003000))
  s <- usage_add(NULL, out)
  s <- usage_add(s, out)
  expect_identical(token_values(s), list(sent = 6e9, received = 6000, total = 6000006000))
  expect_identical(s$unknown, 0L)
  expect_match(usage_html(s), "Sent 6000000000 / Received 6000 tokens", fixed = TRUE)
})

test_that("demonstration rows contribute result rows and no tokens", {
  demo <- annotate_demo_result(usage_calls()[1:2, ])
  live <- usage_calls()[3:4, ]
  live$run_mode <- "live"
  live$demo_notice <- NA_character_
  out <- extract_token_counts(rbind(demo, live))
  expect_identical(out$calls, 2L)
  expect_identical(out$result_rows, 2L)
  expect_identical(token_values(out), list(sent = 250, received = 45, total = 295))
  expect_identical(token_values(extract_token_counts(demo)), list(sent = 0, received = 0, total = 0))
  live$response_id[1] <- NA
  expect_warning(out <- extract_token_counts(rbind(demo, live)), "1 of 2 rows")
  expect_null(out$calls)
  expect_identical(out$result_rows, 4L)
})

test_that("usage_add derives only absent totals with complete supplied usage", {
  expect_identical(usage_add(NULL, list(calls = 1L, sent = 10, received = 2))$total, 12)
  explicit <- usage_add(NULL, list(calls = 1L, sent = 10, received = 2, total = NA_real_))
  expect_identical(explicit$total, 0)
  expect_identical(explicit$unknown, 1L)
  incomplete <- usage_add(NULL, list(calls = 2L, sent = 10, received = 2, unknown = 2L))
  expect_identical(incomplete$total, 0)
  d <- data.frame(response_id = c("a", "b"), sent_tokens = c(10, NA), rec_tokens = c(NA, 2))
  together <- usage_add(NULL, extract_token_counts(d))
  apart <- usage_add(usage_add(NULL, extract_token_counts(d[1, ])), extract_token_counts(d[2, ]))
  expect_identical(together, apart)
  expect_identical(token_values(together), list(sent = 10, received = 2, total = 0))
})

test_that("a reported total with missing parts is retained and qualified", {
  out <- extract_token_counts(data.frame(response_id = "a", total_tokens = 20))
  expect_identical(token_values(out), list(sent = NA_real_, received = NA_real_, total = 20))
  expect_identical(out$unknown, 1L)
  s <- usage_add(NULL, out)
  expect_identical(token_values(s), list(sent = 0, received = 0, total = 20))
  expect_match(usage_html(s), "Token usage unknown", fixed = TRUE)
})

test_that("each displayed component uses its own missing count", {
  d <- data.frame(response_id = c("a", "b"), sent_tokens = c(0, 0), rec_tokens = c(5, NA))
  s <- usage_add(NULL, extract_token_counts(d))
  expect_identical(s$sent_unknown, 0L)
  expect_identical(s$received_unknown, 1L)
  expect_match(usage_html(s), "Sent 0 / Received at least 5 tokens", fixed = TRUE)
  expect_match(usage_html(s), "Usage was not reported for 1 reply.", fixed = TRUE)
  d$sent_tokens <- c(0, NA)
  expect_match(usage_html(usage_add(NULL, extract_token_counts(d))),
               "Sent unknown / Received at least 5 tokens", fixed = TRUE)
  part <- usage_add(NULL, list(calls = 1L, sent = NA, received = 5))
  expect_match(usage_html(part), "Sent unknown / Received 5 tokens", fixed = TRUE)
  expect_match(usage_html(usage_add(NULL, list(calls = 4L))), "Token usage unknown", fixed = TRUE)
  expect_match(usage_html(usage_empty()), "Sent 0 / Received 0 tokens", fixed = TRUE)
  old <- usage_empty()
  old$sent <- NA_real_
  old$sent_unknown <- old$received_unknown <- NULL
  expect_match(usage_html(old), "Sent unknown / Received 0 tokens", fixed = TRUE)
  old <- usage_add(old, list(calls = 1L, sent = 0, received = 0))
  expect_identical(old$sent_unknown, 0L)
  expect_identical(old$received_unknown, 0L)
})

test_that("unknown counts stay integers and invalid counts are rejected", {
  for (field in c("calls", "result_rows")) {
    expect_warning(s <- usage_add(NULL, setNames(list(NA_integer_), field)), "recorded as unknown")
    expect_identical(s[[field]], NA_integer_)
    expect_match(usage_html(s), if (field == "calls") "Unknown number of calls" else "unknown result rows", fixed = TRUE)
    for (value in c(-1, 0.5, Inf, NaN)) {
      expect_error(usage_add(NULL, setNames(list(value), field)), "nonnegative whole")
    }
  }
  planned <- usage_set_plan(usage_empty(), 2L, "Tuning")
  s <- usage_add(planned, list(calls = 2L))
  expect_identical(s$planned_calls, 0L)
  expect_identical(s$planned_label, "No pending run")
})
