test_that("text_block_output provides a bounded scrollable output", {
  ui <- text_block_output("summary", height = "20rem")
  html <- paste(as.character(ui), collapse = " ")

  expect_s3_class(ui, "shiny.tag")
  expect_match(html, "llmr-text-block", fixed = TRUE)
  expect_match(html, "border rounded", fixed = TRUE)
  expect_match(html, "height: 20rem", fixed = TRUE)
  expect_match(html, "overflow: auto", fixed = TRUE)
  expect_match(html, 'id="summary"', fixed = TRUE)
  expect_match(html, "shiny-text-output", fixed = TRUE)
})

test_that("help_tip is focusable and exposes its text accessibly", {
  ui <- help_tip("How the next speaker is chosen.", placement = "left")
  html <- paste(as.character(ui), collapse = " ")

  expect_match(html, "<bslib-tooltip", fixed = TRUE)
  expect_match(html, 'placement="left"', fixed = TRUE)
  expect_match(html, "fa-circle-info", fixed = TRUE)
  expect_match(
    html,
    'aria-label="How the next speaker is chosen."',
    fixed = TRUE
  )
  expect_match(
    html,
    'title="How the next speaker is chosen."',
    fixed = TRUE
  )
  expect_match(html, 'tabindex="0"', fixed = TRUE)
  expect_no_match(html, 'role="presentation"', fixed = TRUE)
})

test_that("the shared theme keeps validation messages in normal flow", {
  theme <- llmr_theme("content")
  rules <- paste(
    unlist(lapply(theme$layers, function(layer) layer$rules)),
    collapse = "\n"
  )

  expect_match(
    rules,
    ".shiny-output-error-validation",
    fixed = TRUE
  )
  expect_match(rules, "position: static !important", fixed = TRUE)
  expect_match(rules, "height: auto !important", fixed = TRUE)
  expect_match(rules, "min-height: 1.5em", fixed = TRUE)
})

test_that("shell_context exposes generation settings as reactives", {
  context <- NULL
  shiny::testServer(function(input, output, session) {
    context <<- shell_context(input, output, session)
  }, {
    session$setInputs(
      run_mode = "live",
      provider = "groq",
      model = "openai/gpt-oss-20b",
      temperature = 0.4,
      max_tokens = 300,
      reasoning_effort = "medium"
    )


    expect_equal(context$temperature(), 0.4)
    expect_equal(context$max_tokens(), 300)
    expect_identical(context$reasoning_effort(), "medium")
  })
})
