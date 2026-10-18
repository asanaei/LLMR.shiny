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

# A file that holds the cases default type inference alters: identifiers longer
# than a double's precision, T/F, the text "NA", Persian text, a quoted field
# spanning two lines, and pre-existing `text` and `labels` columns.
write_hard_cases <- function() {
  f <- tempfile(fileext = ".csv")
  persian <- "سلام دنیا"
  lines <- c(
    "id,body,code,text,labels,flag",
    "12345678901234567890,\"first line\nsecond line\",001,NA,T,T",
    paste0("12345678901234567891,", persian, ",01,kept,F,F")
  )
  writeLines(enc2utf8(lines), f, useBytes = TRUE)
  f
}

read_as_written <- function(f) {
  read_csv_path(f, col_classes = "character", na_strings = character(0))
}

test_that("long integer-like identifiers stay distinct and exact", {
  f <- write_hard_cases(); on.exit(unlink(f), add = TRUE)
  ids <- c("12345678901234567890", "12345678901234567891")
  expect_identical(read_as_written(f)$id, ids)
  default <- read_csv_path(f)
  expect_type(default$id, "double")
  expect_false(identical(format(default$id, scientific = FALSE), ids))
})

test_that("T and F stay text when read as written", {
  f <- write_hard_cases(); on.exit(unlink(f), add = TRUE)
  expect_identical(read_as_written(f)$flag, c("T", "F"))
  expect_identical(read_csv_path(f)$flag, c(TRUE, FALSE))
  d <- read_as_written(f)
  d$flag <- utils::type.convert(d$flag, as.is = TRUE, na.strings = "NA")
  expect_identical(d$flag, read_csv_path(f)$flag)
})

test_that("UTF-8 Persian text is read unchanged", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "the session locale is not UTF-8")
  f <- write_hard_cases(); on.exit(unlink(f), add = TRUE)
  persian <- "سلام دنیا"
  d <- read_as_written(f)
  expect_identical(enc2utf8(d$body[2]), persian)
  expect_identical(nchar(d$body[2]), 9L)
  expect_identical(map_columns(d, "body")$text[2], d$body[2])
})

test_that("a quoted field spanning lines stays one field in one row", {
  f <- write_hard_cases(); on.exit(unlink(f), add = TRUE)
  d <- read_as_written(f)
  expect_identical(nrow(d), 2L)
  expect_identical(d$body[1], "first line\nsecond line")
  expect_identical(nrow(map_columns(d, "body", "code")), 2L)
})

test_that("existing text and labels columns are kept aside, as written", {
  f <- write_hard_cases(); on.exit(unlink(f), add = TRUE)
  d <- read_as_written(f)
  m <- map_columns(d, "body", "code")
  expect_identical(
    names(m),
    c("id", "body", "code", "text.original", "labels.original", "flag",
      "text", "labels")
  )
  expect_identical(m$labels, c("001", "01"))
  expect_identical(m$text.original, c("NA", "kept"))
  expect_identical(m$labels.original, c("T", "F"))
  expect_identical(m$text, d$body)
})

test_that("a file with a repeated heading is refused after reading", {
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("id,text,text", "001,a,b"), f)
  d <- read_as_written(f)
  expect_identical(names(d), c("id", "text", "text"))
  expect_error(map_columns(d, "text"), "duplicate column names \\(text\\)")
})
