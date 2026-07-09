test_that("key_state reads env vars and never exposes the key value", {
  old_api <- Sys.getenv("GROQ_API_KEY", unset = NA_character_)
  old_key <- Sys.getenv("GROQ_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old_api)) Sys.unsetenv("GROQ_API_KEY") else Sys.setenv(GROQ_API_KEY = old_api)
    if (is.na(old_key)) Sys.unsetenv("GROQ_KEY") else Sys.setenv(GROQ_KEY = old_key)
  }, add = TRUE)

  Sys.unsetenv("GROQ_API_KEY"); Sys.unsetenv("GROQ_KEY")
  ks <- key_state("groq")
  expect_false(ks$found)
  expect_equal(ks$env_vars, c("GROQ_API_KEY", "GROQ_KEY"))

  Sys.setenv(GROQ_API_KEY = "super-secret")
  ks2 <- key_state("groq")
  expect_true(ks2$found)
  expect_equal(ks2$env_var, "GROQ_API_KEY")
  # the tile must not echo the value
  txt <- paste(as.character(key_state_tile(ks2)), collapse = " ")
  expect_false(grepl("super-secret", txt, fixed = TRUE))
})

test_that("provider registry resolves defaults", {
  expect_equal(provider_default_model("groq"), "llama-3.3-70b-versatile")
  expect_equal(provider_display_name("openai"), "OpenAI")
  ch <- provider_choices()
  expect_true("groq" %in% ch)
})

test_that("demo runner returns LLMR-shaped columns and marks the result", {
  r <- demo_runner(function(t) if (grepl("policy", t)) "policy" else "other")
  out <- r(data.frame(text = c("a policy item", "something else")))
  expect_equal(nrow(out), 2)
  expect_true(all(c("response_text", "success", "sent_tokens", "rec_tokens",
                    "total_tokens", "response_id") %in% names(out)))
  expect_equal(out$response_text, c("policy", "other"))
  expect_true(is_demo_result(out))
  expect_true(all(out$success))
})

test_that("build_runner gives a demo runner or NULL for live", {
  expect_true(is.function(build_runner("demo")))
  expect_null(build_runner("live"))
})

test_that("column mapping validates and maps, one row per input row", {
  df <- data.frame(raw = c("a", "b", "c"), lab = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  m <- map_columns(df, "raw", "lab", keep_original = FALSE)
  expect_equal(nrow(m), 3)
  expect_equal(m$text, c("a", "b", "c"))
  expect_equal(m$labels, c("x", "y", "z"))
  expect_error(validate_column_mapping(df, "missing"), "text column")
})

test_that("cost accounting accumulates usage and plans", {
  s <- cost_empty()
  s <- cost_set_plan(s, 10, "tuning")
  expect_equal(s$planned_calls, 10L)
  s <- cost_add_usage(s, list(calls = 3, sent = 100, received = 50, total = 150))
  expect_equal(s$calls, 3L)
  expect_equal(s$total, 150L)
  expect_equal(s$planned_calls, 0L)  # reset after a run
})

test_that("token counts are extracted from a result frame", {
  df <- data.frame(response_text = c("a", "b"),
                   sent_tokens = c(5, 6), rec_tokens = c(2, 3),
                   total_tokens = c(7, 9))
  tc <- extract_token_counts(df)
  expect_equal(tc$calls, 2L)
  expect_equal(tc$total, 16L)
})

test_that("safe_llmr_call captures errors as a banner, not a crash", {
  ok <- safe_llmr_call(1 + 1)
  expect_true(ok$ok)
  expect_equal(ok$value, 2)

  bad <- safe_llmr_call(stop("boom"))
  expect_false(bad$ok)
  expect_s3_class(bad$ui, "shiny.tag")
})

test_that("install guidance names the github remote", {
  ui <- install_guidance_ui("LLMRpanel")
  txt <- paste(as.character(ui), collapse = " ")
  expect_match(txt, "asanaei/LLMRpanel")
  expect_match(txt, "install_github")
})

test_that("an unknown provider falls back to empty string / provider id, not NA", {
  expect_identical(provider_default_model("no_such_provider"), "")
  expect_identical(provider_display_name("no_such_provider"), "no_such_provider")
})

test_that("build_runner rejects an unknown mode instead of going live silently", {
  expect_error(build_runner("liv"), "must be")
  expect_error(build_runner("Demo"), "must be")
  expect_true(is.function(build_runner("demo")))
  expect_null(build_runner("live"))
})

test_that("validate_column_mapping gives a clear message for no selection", {
  df <- data.frame(a = 1, b = 2)
  expect_error(validate_column_mapping(df, NULL), "No text column")
  expect_error(validate_column_mapping(df, character(0)), "No text column")
  expect_error(validate_column_mapping(df, ""), "No text column")
  expect_true(validate_column_mapping(df, "a"))
})

test_that("LLMR classed conditions are recognized by class, not a field", {
  # constructed exactly as LLMR's .llmr_error() classes them; offline
  cond_of <- function(category) {
    structure(
      class = c(paste0("llmr_api_", category, "_error"),
                "llmr_api_error", "error", "condition"),
      list(message = "boom", call = NULL)
    )
  }
  expect_identical(condition_category(cond_of("auth")), "auth")
  expect_identical(condition_category(cond_of("rate_limit")), "rate_limit")
  expect_identical(condition_category(cond_of("param")), "param")
  expect_true(is_auth_error(cond_of("auth")))
  expect_false(is_auth_error(cond_of("server")))
  # a plain condition still resolves to NA / not-auth
  plain <- simpleError("boom")
  expect_identical(condition_category(plain), NA_character_)
  expect_false(is_auth_error(plain))
  # the field fallback still works for foreign conditions
  foreign <- simpleError("boom"); foreign$category <- "auth"
  expect_true(is_auth_error(foreign))
})

test_that("an auth error becomes the key banner through safe_llmr_call", {
  auth_cond <- structure(
    class = c("llmr_api_auth_error", "llmr_api_error", "error", "condition"),
    list(message = "401 unauthorized", call = NULL)
  )
  res <- safe_llmr_call(stop(auth_cond), provider = "groq")
  expect_false(res$ok)
  txt <- paste(as.character(res$ui), collapse = " ")
  expect_match(txt, "GROQ_API_KEY")
  expect_match(txt, "No API key")
  # a non-auth error shows its message, not the key banner
  res2 <- safe_llmr_call(stop("plain failure"), provider = "groq")
  txt2 <- paste(as.character(res2$ui), collapse = " ")
  expect_match(txt2, "plain failure")
  expect_no_match(txt2, "GROQ_API_KEY")
})

test_that("map_columns(keep_original = TRUE) preserves pre-existing columns", {
  df <- data.frame(text = c("orig A", "orig B"),
                   labels = c("L1", "L2"),
                   body = c("body A", "body B"),
                   lab2 = c("x", "y"),
                   stringsAsFactors = FALSE)
  m <- map_columns(df, "body", "lab2", keep_original = TRUE)
  expect_equal(m$text, c("body A", "body B"))
  expect_equal(m$labels, c("x", "y"))
  expect_equal(m$text.original, c("orig A", "orig B"))
  expect_equal(m$labels.original, c("L1", "L2"))
  expect_equal(nrow(m), 2)
  # mapping a column onto itself adds no suffix column
  m2 <- map_columns(df, "text", keep_original = TRUE)
  expect_false("text.original" %in% names(m2))
  expect_equal(m2$text, c("orig A", "orig B"))
})

test_that("demo runner survives a zero-row frame and NA input text", {
  r <- demo_runner()
  z <- r(data.frame(text = character(0)))
  expect_equal(nrow(z), 0)
  expect_true(all(c("response_text", "success", "sent_tokens",
                    "rec_tokens", "total_tokens", "response_id") %in% names(z)))
  expect_length(z$response_id, 0)

  out <- r(data.frame(text = c("hello there", NA)))
  expect_false(anyNA(out$sent_tokens))
  expect_false(anyNA(out$total_tokens))
  expect_equal(out$total_tokens, out$sent_tokens + out$rec_tokens)
})

test_that("cost tile shows the planned line only for a pending run", {
  s <- cost_set_plan(cost_empty(), 10, "tuning")
  txt <- paste(as.character(cost_tile(s)), collapse = " ")
  expect_match(txt, "tuning: 10 calls")

  s <- cost_add_usage(s, list(calls = 10, sent = 40, received = 20, total = 60))
  txt2 <- paste(as.character(cost_tile(s)), collapse = " ")
  expect_no_match(txt2, "No pending run: 0 calls", fixed = TRUE)
  expect_match(txt2, "No pending run")
})

test_that("extract_token_counts falls back row-wise for NA totals", {
  df <- data.frame(response_text = c("a", "b"),
                   sent_tokens = c(5, 6), rec_tokens = c(2, 3),
                   total_tokens = c(7, NA))
  tc <- extract_token_counts(df)
  expect_equal(tc$total, 16L)
  expect_equal(tc$sent, 11L)
  expect_equal(tc$received, 5L)
})
