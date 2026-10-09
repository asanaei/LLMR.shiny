# io.R -------------------------------------------------------------------------
# Input/output: read uploaded CSVs, map user columns to the names a package API
# expects, and coerce arbitrary results to a display table.

#' Read an uploaded CSV (a Shiny fileInput value)
#'
#' By default every column's type is inferred, as [utils::read.csv()] does, so
#' an identifier such as `001` becomes the number 1 and two identifiers `001`
#' and `01` become the same value. To keep identifiers and text exactly as
#' written, read with `col_classes = "character"` and `na_strings =
#' character(0)`, and convert the other columns afterwards with
#' `utils::type.convert(x, as.is = TRUE, na.strings = "NA")`, which reproduces
#' the default inference.
#'
#' @param file A `fileInput` value (a list with `datapath`).
#' @param col_classes Passed to [utils::read.csv()] as `colClasses`. The
#'   default, `NA`, infers each column's type.
#' @param na_strings Passed to [utils::read.csv()] as `na.strings`. The default
#'   reads the text `"NA"` as missing.
#' @return A data frame.
#' @export
read_csv_upload <- function(file, col_classes = NA, na_strings = "NA") {
  if (is.null(file) || is.null(file$datapath)) {
    stop("No CSV file was provided.", call. = FALSE)
  }
  utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = col_classes, na.strings = na_strings)
}

#' Read a CSV from a path
#'
#' @param path File path.
#' @inheritParams read_csv_upload
#' @return A data frame.
#' @export
read_csv_path <- function(path, col_classes = NA, na_strings = "NA") {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = col_classes, na.strings = na_strings)
}

#' Column names of a data frame, for a mapping select input
#'
#' @param data A data frame.
#' @return A character vector of column names.
#' @export
column_names_for_mapping <- function(data) {
  names(as.data.frame(data))
}

#' Guess a column for a data-mapping role
#'
#' Column names are matched without regard to case or common separators.
#' When no recognized name remains, the first available column is returned.
#' Text names include `text`, `body`, `content`, `response`, `message`, and
#' `utterance`; label names include `label`, `labels`, `code`, `category`, and
#' `class`; identifier names include `id`, `doc_id`, and `unit_id`.
#'
#' @param cols A character vector of column names.
#' @param kind The mapping role to identify: `"text"`, `"label"`, or `"id"`.
#' @param exclude Column names that are already assigned to another role.
#' @return One column name as a character vector of length one, or `NULL` when
#'   no column remains.
#' @export
#' @examples
#' cols <- c("id", "text", "label")
#' text_col <- guess_column(cols, "text")
#' guess_column(cols, "label", exclude = text_col)
guess_column <- function(cols, kind = c("text", "label", "id"),
                         exclude = character()) {
  kind <- match.arg(kind)
  cols <- as.character(cols)
  remaining <- cols[!is.na(cols) & !cols %in% as.character(exclude)]
  if (!length(remaining)) {
    return(NULL)
  }

  variants <- list(
    text = c("text", "body", "content", "response", "message", "utterance"),
    label = c("label", "labels", "code", "category", "class"),
    id = c("id", "doc_id", "unit_id")
  )
  normalize <- function(x) {
    gsub("[^[:alnum:]]", "", tolower(trimws(x)))
  }
  matched <- match(normalize(variants[[kind]]), normalize(remaining), nomatch = 0L)
  matched <- matched[matched > 0L]
  if (length(matched)) {
    return(remaining[[matched[[1L]]]])
  }

  remaining[[1L]]
}

#' Validate a column mapping
#'
#' Columns are selected by name, so the data must have unique, non-empty column
#' names.
#'
#' @param data A data frame.
#' @param text_col Required text column name.
#' @param label_col Optional label column name; `NULL` or `""` for none.
#' @return `TRUE`, or an error.
#' @export
validate_column_mapping <- function(data, text_col, label_col = NULL) {
  cols <- column_names_for_mapping(data)
  bad <- is.na(cols) | !nzchar(trimws(cols))
  if (any(bad)) {
    stop(sprintf("Column(s) %s have no name; name every column before mapping.",
                 paste(which(bad), collapse = ", ")), call. = FALSE)
  }
  dup <- unique(cols[duplicated(cols)])
  if (length(dup)) {
    stop(sprintf("The data have duplicate column names (%s); rename them before mapping.",
                 paste(dup, collapse = ", ")), call. = FALSE)
  }
  # A NULL/length-0 text_col is the common "nothing selected yet" Shiny state;
  # give a clear message rather than a base length-zero condition error.
  if (is.null(text_col) || length(text_col) != 1L || is.na(text_col) ||
      !nzchar(text_col)) {
    stop("No text column is selected.", call. = FALSE)
  }
  if (!text_col %in% cols) {
    stop("The selected text column is not in the data.", call. = FALSE)
  }
  if (!is.null(label_col)) {
    if (length(label_col) != 1L || is.na(label_col)) {
      stop("Select one label column, or none.", call. = FALSE)
    }
    if (nzchar(label_col) && !label_col %in% cols) {
      stop("The selected label column is not in the data.", call. = FALSE)
    }
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
#' Data frames and matrices pass through (head-limited). For a list, name the
#' component to display. Anything else becomes a one-column capture of its
#' structure.
#'
#' @param x Any object.
#' @param max_rows Row cap for the display.
#' @param component For a list, the name of the component to display.
#' @param digits Number of significant digits used for ordinary double columns.
#'   Integer, identifier, and nonnumeric columns are not changed.
#' @return A data frame.
#' @export
#' @examples
#' x <- data.frame(
#'   item_id = c(1000001, 1000002),
#'   share = c(0.123456, 0.987654),
#'   count = c(3L, 7L)
#' )
#' as_display_table(x)
as_display_table <- function(x, max_rows = 500L, component = NULL, digits = 3L) {
  if (length(digits) != 1L || !is.numeric(digits) || is.na(digits) ||
      !is.finite(digits) || digits < 1 || digits != as.integer(digits)) {
    stop("`digits` must be one positive whole number.", call. = FALSE)
  }

  prepare <- function(value) {
    out <- utils::head(as.data.frame(value), max_rows)
    column_names <- names(out)
    is_id <- grepl(
      "(^ids?$|(^|[._])(ids?|uuids?|guids?)$|identifier)",
      column_names,
      ignore.case = TRUE
    ) | grepl("(Id|ID|Uuid|UUID|Guid|GUID)s?$", column_names)
    # Counts are often stored as doubles; rounding them to significant digits
    # would misreport them (123456 words as 123000), so only columns that
    # actually carry fractions are rounded.
    roundable <- vapply(
      out,
      function(column) {
        is.double(column) && is.null(attr(column, "class")) &&
          any(is.finite(column) & column != round(column))
      },
      logical(1)
    ) & !is_id
    out[roundable] <- lapply(out[roundable], signif, digits = as.integer(digits))
    out
  }

  if (is.data.frame(x)) return(prepare(x))
  if (is.matrix(x)) return(prepare(x))
  if (is.list(x)) {
    if (length(component) != 1L || !is.character(component) ||
        !component %in% names(x)) {
      stop("`component` must name one component of a list result.", call. = FALSE)
    }
    return(as_display_table(
      x[[component]],
      max_rows = max_rows,
      digits = digits
    ))
  }
  data.frame(
    output = paste(utils::capture.output(utils::str(x, max.level = 2)), collapse = "\n"),
    stringsAsFactors = FALSE
  )
}
