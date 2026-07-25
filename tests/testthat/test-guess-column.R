test_that("guess_column recognizes mapping roles by name", {
  cols <- c("id", "text", "label")
  text_col <- guess_column(cols, "text")
  label_col <- guess_column(cols, "label", exclude = text_col)

  expect_identical(text_col, "text")
  expect_identical(label_col, "label")
  expect_identical(guess_column(cols, "id"), "id")
})

test_that("guess_column ignores case and common separators", {
  cols <- c("Unit.ID", "BODY", "Category")

  expect_identical(guess_column(cols, "text"), "BODY")
  expect_identical(guess_column(cols, "label"), "Category")
  expect_identical(guess_column(cols, "id"), "Unit.ID")
})

test_that("guess_column recognizes each documented common variant", {
  variants <- list(
    text = c("text", "body", "content", "response", "message", "utterance"),
    label = c("label", "labels", "code", "category", "class"),
    id = c("id", "doc_id", "unit_id")
  )

  for (kind in names(variants)) {
    for (variant in variants[[kind]]) {
      expect_identical(
        guess_column(c("fallback", variant), kind),
        variant
      )
    }
  }
})

test_that("guess_column excludes assigned columns and falls back in order", {
  cols <- c("text", "body", "other")

  expect_identical(
    guess_column(cols, "text", exclude = "text"),
    "body"
  )
  expect_identical(
    guess_column(c("record", "answer"), "label", exclude = "record"),
    "answer"
  )
  expect_null(guess_column(cols, "text", exclude = cols))
  expect_null(guess_column(character(), "text"))
})
