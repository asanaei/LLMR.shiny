# cost.R -----------------------------------------------------------------------
# Session cost accounting, shared across GUIs. A small mutable record of calls
# and tokens, plus a planned-run estimate, rendered as a value box.

#' An empty cost-accounting record
#'
#' @return A list with zeroed counters.
#' @export
cost_empty <- function() {
  list(calls = 0L, sent = 0L, received = 0L, total = 0L,
       planned_calls = 0L, planned_label = "No pending run")
}

#' Record a planned run on a cost record
#'
#' @param state A cost record.
#' @param calls Number of calls the next run will make.
#' @param label A short label for the planned run.
#' @return The updated cost record.
#' @export
cost_set_plan <- function(state, calls, label = "Next run") {
  state$planned_calls <- as.integer(calls %||% 0)
  state$planned_label <- label
  state
}

#' Add realized usage to a cost record
#'
#' @param state A cost record.
#' @param tokens A token-count list (see [extract_token_counts()]).
#' @return The updated cost record.
#' @export
cost_add_usage <- function(state, tokens) {
  state$calls <- state$calls + as.integer(tokens$calls %||% 0)
  state$sent <- state$sent + as.integer(tokens$sent %||% 0)
  state$received <- state$received + as.integer(tokens$received %||% 0)
  state$total <- state$total + as.integer(tokens$total %||% 0)
  state$planned_calls <- 0L
  state$planned_label <- "No pending run"
  state
}

#' A value box reporting session usage
#'
#' @param state A cost record.
#' @return A `bslib::value_box`.
#' @export
cost_tile <- function(state) {
  state <- state %||% cost_empty()
  planned <- as.integer(state$planned_calls %||% 0L)
  planned_line <- if (!is.na(planned) && planned > 0L) {
    shiny::tags$p(paste0(state$planned_label %||% "Planned next run", ": ",
                         planned, " calls"))
  } else {
    shiny::tags$p("No pending run")
  }
  bslib::value_box(
    title = "Session usage",
    value = paste0(state$calls, " calls"),
    showcase = shiny::tags$span("Tokens"),
    shiny::tags$p(paste0("Sent: ", state$sent, " | Received: ", state$received)),
    planned_line
  )
}
