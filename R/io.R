# io.R -------------------------------------------------------------------------
# Input/output: read uploaded CSVs, map user columns to the names a package API
# expects, and coerce arbitrary results to a display table.

#' Read an uploaded CSV (a Shiny fileInput value)
#'
#' @param file A `fileInput` value (a list with `datapath`).
#' @return A data frame.
#' @export
read_csv_upload <- function(file) {
  if (is.null(file) || is.null(file$datapath)) {
    stop("No CSV file was provided.", call. = FALSE)
  }
  utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read a CSV from a path
#'
#' @param path File path.
#' @return A data frame.
#' @export
read_csv_path <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Column names of a data frame, for a mapping select input
#'
#' @param data A data frame.
#' @return A character vector of column names.
#' @export
column_names_for_mapping <- function(data) {
  names(as.data.frame(data))
}

#' Validate a column mapping
#'
#' @param data A data frame.
#' @param text_col Required text column name.
#' @param label_col Optional label column name.
#' @return `TRUE`, or an error.
#' @export
validate_column_mapping <- function(data, text_col, label_col = NULL) {
  cols <- column_names_for_mapping(data)
  # A NULL/length-0 text_col is the common "nothing selected yet" Shiny state;
  # give a clear message rather than a base length-zero condition error.
  if (is.null(text_col) || length(text_col) != 1L || is.na(text_col) ||
      !nzchar(text_col)) {
    stop("No text column is selected.", call. = FALSE)
  }
  if (!text_col %in% cols) {
    stop("The selected text column is not in the data.", call. = FALSE)
  }
  if (!is.null(label_col) && nzchar(label_col) && !label_col %in% cols) {
    stop("The selected label column is not in the data.", call. = FALSE)
  }
  TRUE
}

#' Map user columns to `text` (and optionally `labels`)
#'
#' @param data A data frame.
#' @param text_col Name of the column to become `text`.
#' @param label_col Optional name of the column to become `labels`.
#' @param keep_original Keep the original columns alongside the mapped ones.
#'   A pre-existing `text` (or `labels`) column that is not itself the mapped
#'   source is preserved under a `.original` suffix rather than overwritten.
#' @return A data frame with a `text` column (and `labels` when requested),
#'   always with one row per input row.
#' @export
map_columns <- function(data, text_col, label_col = NULL, keep_original = TRUE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  validate_column_mapping(data, text_col, label_col)

  want_labels <- !is.null(label_col) && nzchar(label_col)
  out <- if (keep_original) data else data.frame(row.names = seq_len(nrow(data)))
  if (keep_original) {
    # The mapped names must not clobber a user's pre-existing column of the
    # same name; move such a column aside under a ".original" suffix.
    sources <- c(text = text_col, if (want_labels) c(labels = label_col))
    for (target in names(sources)) {
      if (target %in% names(out) && !identical(sources[[target]], target)) {
        names(out)[names(out) == target] <-
          make.unique(c(names(out), paste0(target, ".original")))[ncol(out) + 1L]
      }
    }
  }
  out$text <- as.character(data[[text_col]])
  if (want_labels) {
    out$labels <- as.character(data[[label_col]])
  }
  out
}

#' Coerce an arbitrary result to a display table
#'
#' Data frames and matrices pass through (head-limited); a list yields its first
#' data frame; anything else becomes a one-column capture of its structure.
#'
#' @param x Any object.
#' @param max_rows Row cap for the display.
#' @return A data frame.
#' @export
as_display_table <- function(x, max_rows = 500L) {
  if (is.data.frame(x)) return(utils::head(as.data.frame(x), max_rows))
  if (is.matrix(x)) return(utils::head(as.data.frame(x), max_rows))
  if (is.list(x)) {
    dfs <- Filter(is.data.frame, x)
    if (length(dfs) > 0) return(utils::head(as.data.frame(dfs[[1]]), max_rows))
  }
  data.frame(
    output = paste(utils::capture.output(utils::str(x, max.level = 2)), collapse = "\n"),
    stringsAsFactors = FALSE
  )
}
