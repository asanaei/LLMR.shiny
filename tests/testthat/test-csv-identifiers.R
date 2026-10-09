# Uploaded identifiers can be kept exactly as written, and a mapping refuses
# column names it cannot resolve unambiguously.

write_ids <- function() {
  f <- tempfile(fileext = ".csv")
  writeLines(c("id,text,score", "001,NA,1.5", "01,hello,2"), f)
  f
}

test_that("default reading is unchanged", {
  f <- write_ids(); on.exit(unlink(f), add = TRUE)
  d <- read_csv_path(f)
  expect_identical(d, utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE))
  expect_identical(d$id, c(1L, 1L))
  expect_true(is.na(d$text[1]))
})

test_that("identifiers and text can be read exactly as written", {
  f <- write_ids(); on.exit(unlink(f), add = TRUE)
  d <- read_csv_path(f, col_classes = "character", na_strings = character(0))
  expect_identical(d$id, c("001", "01"))
  expect_identical(d$text, c("NA", "hello"))
  others <- setdiff(names(d), c("id", "text"))
  d[others] <- lapply(d[others], utils::type.convert, as.is = TRUE, na.strings = "NA")
  expect_identical(d$score, read_csv_path(f)$score)
})

test_that("read_csv_upload honors the same options", {
  f <- write_ids(); on.exit(unlink(f), add = TRUE)
  d <- read_csv_upload(list(datapath = f), col_classes = "character",
                       na_strings = character(0))
  expect_identical(d$id, c("001", "01"))
})

test_that("duplicate or empty column names are refused before mapping", {
  df <- data.frame(a = c("x", "y"), b = c("u", "v"))
  names(df) <- c("text", "text")
  expect_error(validate_column_mapping(df, "text"), "duplicate column names")
  expect_error(map_columns(df, "text"), "duplicate column names")
  df2 <- data.frame(a = 1, y = "t")
  names(df2) <- c("", "y")
  expect_error(validate_column_mapping(df2, "y"), "have no name")
})

test_that("the label selector is validated like the text selector", {
  df <- data.frame(text = "a", lab = "b")
  expect_error(validate_column_mapping(df, "text", c("lab", "text")), "one label column")
  expect_error(validate_column_mapping(df, "text", NA_character_), "one label column")
  expect_true(validate_column_mapping(df, "text", ""))
  expect_true(validate_column_mapping(df, "text", "lab"))
})
