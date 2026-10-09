# usage.R ----------------------------------------------------------------------
# Session usage accounting shared across GUIs. The record distinguishes calls
# supported by provenance from result rows whose call count is unknown, and
# known token counts from replies whose usage was not reported.

#' An empty usage record
#'
#' @return A list with zeroed counters: `calls`, `result_rows`, `sent`,
#'   `received`, `total`, `unknown` (replies whose usage was not reported),
#'   `planned_calls`, and `planned_label`.
#' @export
usage_empty <- function() {
  list(calls = 0L, result_rows = 0L, sent = 0L, received = 0L, total = 0L,
       unknown = 0L, planned_calls = 0L, planned_label = "No pending run")
}

#' Record a planned run on a usage record
#'
#' @param state A usage record.
#' @param calls Number of calls the next run will make.
#' @param label A short label for the planned run.
#' @return The updated usage record.
#' @export
usage_set_plan <- function(state, calls, label = "Next run") {
  state$planned_calls <- as.integer(calls %||% 0)
  state$planned_label <- label
  state
}

#' Add realized usage to a usage record
#'
#' Token counts that are missing or `NA` are unknown, not zero: the record's
#' token sums cover the known counts, and its `unknown` field counts the
#' replies they leave out. A missing total is taken as sent plus received
#' tokens when both are known. When `tokens` has no `unknown` field and any of
#' its three counts is unknown, each of its `calls` counts as unknown. A call
#' or row count given as `NA` is recorded as unknown, with a warning.
#'
#' @param state A usage record.
#' @param tokens A token-count list (see [extract_token_counts()]) with any of
#'   `calls`, `result_rows`, `sent`, `received`, `total`, and `unknown`.
#' @return The updated usage record.
#' @examples
#' state <- usage_add(usage_empty(), list(calls = 2, sent = 120, received = 40))
#' state <- usage_add(state, list(calls = 1))
#' state[c("calls", "sent", "received", "total", "unknown")]
#' @export
usage_add <- function(state, tokens) {
  state <- state %||% usage_empty()
  calls <- count_field(tokens[["calls"]], "tokens$calls")
  rows <- count_field(tokens[["result_rows"]], "tokens$result_rows")
  sent <- token_field(tokens[["sent"]], "sent")
  received <- token_field(tokens[["received"]], "received")
  total <- token_field(tokens[["total"]], "total")
  if (is.na(total) && !is.na(sent) && !is.na(received)) total <- sent + received
  unknown <- if (is.null(tokens[["unknown"]])) {
    if (anyNA(c(sent, received, total))) calls else 0L
  } else {
    count_field(tokens[["unknown"]], "tokens$unknown")
  }
  add_count <- function(old, new) as.integer((old %||% 0L) + new)
  add_tokens <- function(old, new) {
    as.integer((old %||% 0L) + if (is.na(new)) 0L else new)
  }
  state$calls <- add_count(state$calls, calls)
  state$result_rows <- add_count(state$result_rows, rows)
  state$sent <- add_tokens(state$sent, sent)
  state$received <- add_tokens(state$received, received)
  state$total <- add_tokens(state$total, total)
  state$unknown <- add_count(state$unknown, unknown)
  state$planned_calls <- 0L
  state$planned_label <- "No pending run"
  state
}

# One token count: absent or NA is unknown; anything other than a single
# number is not a count, and is treated as unknown with a warning.
token_field <- function(x, name) {
  if (!length(x)) return(NA_real_)
  if (length(x) == 1L && (is.numeric(x) || (is.logical(x) && is.na(x)))) {
    return(as.numeric(x))
  }
  warning(sprintf("`tokens$%s` is not a single number; it is treated as unknown.",
                  name), call. = FALSE)
  NA_real_
}

#' A value box reporting session usage
#'
#' Token counts are shown as lower bounds ("at least") when some replies did
#' not report usage, and as unknown when no reply reported them.
#'
#' @param state A usage record.
#' @return A `bslib::value_box`.
#' @export
usage_tile <- function(state) {
  state <- state %||% usage_empty()
  calls <- as.integer(state$calls %||% 0L)
  result_rows <- as.integer(state$result_rows %||% 0L)
  unknown <- as.integer(state$unknown %||% 0L)
  some_unknown <- is.na(unknown) || unknown > 0L
  sent <- state$sent %||% 0L
  received <- state$received %||% 0L
  bound <- function(v) {
    if (!some_unknown) return(as.character(v))
    if (identical(as.numeric(v), 0)) "unknown" else paste("at least", v)
  }
  token_line <- if (some_unknown && identical(as.numeric(sent), 0) &&
                    identical(as.numeric(received), 0)) {
    "Token usage unknown"
  } else {
    paste0("Sent ", bound(sent), " / Received ", bound(received), " tokens")
  }
  unknown_line <- if (some_unknown) {
    shiny::tags$p(
      class = "small text-body-secondary",
      if (is.na(unknown)) {
        "Usage was not reported for some replies."
      } else {
        paste0("Usage was not reported for ", unknown,
               if (identical(unknown, 1L)) " reply." else " replies.")
      }
    )
  }
  realized <- c(
    if (is.na(calls)) {
      "Unknown number of calls"
    } else {
      paste(calls, if (identical(calls, 1L)) "call" else "calls")
    },
    if (is.na(result_rows)) {
      "unknown result rows"
    } else if (result_rows > 0L) {
      paste(
        result_rows,
        if (identical(result_rows, 1L)) "result row" else "result rows"
      )
    }
  )

  planned <- as.integer(state$planned_calls %||% 0L)
  planned_line <- if (!is.na(planned) && planned > 0L) {
    label <- sub("[.]+$", "", state$planned_label %||% "Next run")
    shiny::tags$p(
      paste0(
        planned, " planned ",
        if (identical(planned, 1L)) "call" else "calls",
        ". ", label, "."
      )
    )
  } else {
    shiny::tags$p("No pending run")
  }
  bslib::value_box(
    title = "Session usage",
    value = paste(realized, collapse = " | "),
    showcase = shiny::tags$span("Usage"),
    shiny::tags$p(token_line),
    unknown_line,
    shiny::tags$p(class = "small text-body-secondary", "Calls are API requests."),
    planned_line
  )
}
