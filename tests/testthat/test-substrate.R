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
