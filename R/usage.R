# usage.R ----------------------------------------------------------------------
# Session usage accounting shared across GUIs. The record distinguishes calls
# supported by provenance from result rows whose call count is unknown, and
# known token counts from replies whose usage was not reported.

#' Create an empty session usage record
#'
#' Initializes the counters used by [usage_add()] and [usage_tile()].
#'
#' @return A named list with zero numeric counters `calls`, `result_rows`,
#'   `sent`, `received`, `total`, `unknown`, `sent_unknown`,
#'   `received_unknown`, and `planned_calls`. Token subtotals are doubles;
#'   the other counters are integers. `planned_label` is initialized to
#'   `"No pending run"`.
#' @export
usage_empty <- function() {
  list(calls = 0L, result_rows = 0L, sent = 0, received = 0, total = 0,
       unknown = 0L, sent_unknown = 0L, received_unknown = 0L,
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

#' Add usage to a session record
#'
#' Adds call counts and token subtotals to a usage record and clears its
#' pending run.
#'
#' @details
#' Missing or `NA` token values contribute nothing to the stored subtotals.
#' A total absent from `tokens` is computed from sent plus received when
#' both are known and `unknown` is absent or zero. An explicit `NA` total
#' remains unknown. Token values must be finite nonnegative whole numbers;
#' an invalid value is treated as unknown with one warning naming the field.
#'
#' A supplied `unknown` count is added to the record. If absent, it defaults
#' to the call count when any token component is unknown, and zero otherwise.
#' The same rule applies separately to `sent_unknown` and `received_unknown`
#' using their respective components. Result rows alone do not supply these
#' unknown counts.
#'
#' Missing call and result-row counts contribute zero. A scalar `NA` count
#' is retained with a warning. Other counts must be finite nonnegative whole
#' numbers that fit in an R integer. Fields absent from an older record
#' start at zero; missingness not recorded there cannot be recovered.
#'
#' @param state A usage record, usually from [usage_empty()], or `NULL`.
#' @param tokens A named list, such as the result of [extract_token_counts()].
#'   It may contain `calls`, `result_rows`, `sent`, `received`, `total`,
#'   `unknown`, `sent_unknown`, and `received_unknown`.
#' @return The updated record with double token subtotals and integer counts.
#'   `planned_calls` is zero and `planned_label` is `"No pending run"`.
#'   Other fields are retained.
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
  if (is.null(tokens[["total"]]) &&
      (is.null(tokens[["unknown"]]) || isTRUE(tokens[["unknown"]] == 0)) &&
      !is.na(sent) && !is.na(received)) total <- sent + received
  unknown <- if (is.null(tokens[["unknown"]])) {
    if (anyNA(c(sent, received, total))) calls else 0L
  } else {
    count_field(tokens[["unknown"]], "tokens$unknown")
  }
  component_unknown <- function(name, value) {
    if (is.null(tokens[[name]])) {
      if (is.na(value)) calls else 0L
    } else {
      count_field(tokens[[name]], paste0("tokens$", name))
    }
  }
  sent_unknown <- component_unknown("sent_unknown", sent)
  received_unknown <- component_unknown("received_unknown", received)
  add_count <- function(old, new) {
    as.integer(as.double(old %||% 0L) + new)
  }
  add_tokens <- function(old, new) {
    as.double(old %||% 0) + if (is.na(new)) 0 else new
  }
  state$calls <- add_count(state$calls, calls)
  state$result_rows <- add_count(state$result_rows, rows)
  state$sent <- add_tokens(state$sent, sent)
  state$received <- add_tokens(state$received, received)
  state$total <- add_tokens(state$total, total)
  state$unknown <- add_count(state$unknown, unknown)
  state$sent_unknown <- add_count(state$sent_unknown, sent_unknown)
  state$received_unknown <- add_count(state$received_unknown, received_unknown)
  state$planned_calls <- 0L
  state$planned_label <- "No pending run"
  state
}

# One token count, stored as a double. Missing values remain unknown;
# invalid values produce a warning before being treated as missing.
token_field <- function(x, name) {
  if (!length(x)) return(NA_real_)
  if (length(x) == 1L && is.na(x) && !is.nan(x)) return(NA_real_)
  if (length(x) == 1L && is.numeric(x) && is.finite(x) &&
      x >= 0 && x == trunc(x)) {
    return(as.numeric(x))
  }
  warning(sprintf("`tokens$%s` must be one finite nonnegative whole number; it is treated as unknown.",
                  name), call. = FALSE)
  NA_real_
}

#' Display session usage
#'
#' Creates a value box showing recorded calls, result rows, and sent and
#' received token subtotals. A pending run is shown when its planned call
#' count is positive.
#'
#' @details
#' Each component uses its own unknown count. A zero unknown count gives the
#' exact subtotal. Otherwise a zero subtotal is labeled "unknown" and a
#' positive subtotal is prefixed by "at least". An `NA` subtotal is labeled
#' "unknown". If both components are unknown, the box displays
#' "Token usage unknown". The `unknown` field gives the number of replies
#' with incomplete usage in a separate line.
#'
#' Unknown counts absent from older records are treated as zero. An `NA`
#' call or result-row count is displayed as unknown.
#'
#' @param state A usage record, usually updated by [usage_add()], or `NULL`.
#' @return A value box created by [bslib::value_box()].
#' @export
usage_tile <- function(state) {
  state <- state %||% usage_empty()
  calls <- as.integer(state$calls %||% 0L)
  result_rows <- as.integer(state$result_rows %||% 0L)
  unknown <- as.integer(state$unknown %||% 0L)
  some_unknown <- is.na(unknown) || unknown > 0L
  sent <- state$sent %||% 0L
  received <- state$received %||% 0L
  bound <- function(v, missing) {
    if (is.na(v)) return("unknown")
    value <- format(v, scientific = FALSE, trim = TRUE)
    if (isTRUE(missing == 0L)) return(value)
    if (v == 0) "unknown" else paste("at least", value)
  }
  sent <- bound(sent, state$sent_unknown %||% 0L)
  received <- bound(received, state$received_unknown %||% 0L)
  token_line <- if (identical(sent, "unknown") && identical(received, "unknown")) {
    "Token usage unknown"
  } else {
    paste0("Sent ", sent, " / Received ", received, " tokens")
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
