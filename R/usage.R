# usage.R ----------------------------------------------------------------------
# Session usage accounting shared across GUIs. The record distinguishes calls
# supported by provenance from result rows whose call count is unknown.

#' An empty usage record
#'
#' @return A list with zeroed counters.
#' @export
usage_empty <- function() {
  list(calls = 0L, result_rows = 0L, sent = 0L, received = 0L, total = 0L,
       planned_calls = 0L, planned_label = "No pending run")
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
#' @param state A usage record.
#' @param tokens A token-count list (see [extract_token_counts()]).
#' @return The updated usage record.
#' @export
usage_add <- function(state, tokens) {
  state$calls <- state$calls + as.integer(tokens$calls %||% 0)
  state$result_rows <- state$result_rows +
    as.integer(tokens$result_rows %||% 0)
  state$sent <- state$sent + as.integer(tokens$sent %||% 0)
  state$received <- state$received + as.integer(tokens$received %||% 0)
  state$total <- state$total + as.integer(tokens$total %||% 0)
  state$planned_calls <- 0L
  state$planned_label <- "No pending run"
  state
}

#' A value box reporting session usage
#'
#' @param state A usage record.
#' @return A `bslib::value_box`.
#' @export
usage_tile <- function(state) {
  state <- state %||% usage_empty()
  calls <- as.integer(state$calls %||% 0L)
  result_rows <- as.integer(state$result_rows %||% 0L)
  realized <- c(
    paste(calls, if (identical(calls, 1L)) "call" else "calls"),
    if (result_rows > 0L) {
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
    shiny::tags$p(
      paste0("Sent ", state$sent, " / Received ", state$received, " tokens")
    ),
    shiny::tags$p(class = "small text-body-secondary", "Calls are API requests."),
    planned_line
  )
}
