# runners.R --------------------------------------------------------------------
# Demonstration mode returns reproducible offline responses marked as
# demonstrations. Live mode delegates request rows to LLMR::call_llm_par
# through a replaceable function for live execution.

#' Demo-result notice string
#'
#' @return The marker text stamped on every demo result.
#' @export
demo_notice <- function() {
  "DEMO RESULT -- offline stub, not a model, not a finding"
}

#' Build an LLM config
#'
#' @param provider,model Provider and model ids.
#' @param ... Other arguments passed to [LLMR::llm_config()].
#' @param temperature Sampling temperature, or `NULL` to use the model default.
#' @param max_tokens Maximum output tokens, or `NULL`, `NA`, or `""` to use the
#'   model default.
#' @param reasoning_effort Reasoning effort (`"low"`, `"medium"`, or `"high"`),
#'   or `NULL` or `""` to use the model default.
#' @return An `LLMR::llm_config`.
#' @export
build_llm_config <- function(provider, model, ..., temperature = NULL,
                             max_tokens = NULL, reasoning_effort = NULL) {
  if (!pkg_available("LLMR")) {
    stop(
      "Package 'LLMR' is required to build a live config. Install it with install.packages(\"LLMR\").",
      call. = FALSE
    )
  }
  model_params <- list(...)
  if (length(temperature) == 1L && !is.na(temperature) &&
      (!is.character(temperature) || nzchar(trimws(temperature)))) {
    model_params$temperature <- temperature
  }
  if (length(max_tokens) == 1L && !is.na(max_tokens) &&
      (!is.character(max_tokens) || nzchar(trimws(max_tokens)))) {
    model_params$max_tokens <- max_tokens
  }
  if (length(reasoning_effort) == 1L && !is.na(reasoning_effort) &&
      nzchar(trimws(reasoning_effort))) {
    model_params$reasoning_effort <- reasoning_effort
  }
  do.call(
    LLMR::llm_config,
    c(list(provider = provider, model = model), model_params)
  )
}

#' An offline demonstration response function
#'
#' Returns a function accepted by `.runner` arguments. It adds LLMR response
#' columns to a request data frame. The per-row response text is decided by
#' `responder`, a function `(text) -> character`. Results are marked as
#' demonstrations.
#'
#' @param responder A function mapping a single input text to a response string.
#'   Defaults to echoing a short stub.
#' @param text_cols Candidate column names to read the input text from.
#' @return A response function of class `llmrshiny_demo_runner`.
#' @export
demo_runner <- function(responder = NULL,
                        text_cols = c("text", "unit", "document", "prompt", "input")) {
  responder <- responder %||% function(t) "demo response"

  runner <- function(experiments, ...) {
    df <- as.data.frame(experiments, stringsAsFactors = FALSE)
    n <- NROW(df)
    text_col <- intersect(text_cols, names(df))[1]
    texts <- if (!is.na(text_col)) as.character(df[[text_col]]) else {
      # fall back to the rendered user message when present
      if ("messages" %in% names(df)) {
        vapply(df$messages, function(m) paste(unlist(m), collapse = " "), character(1))
      } else rep("", n)
    }
    # an NA input text behaves as empty: the responder sees "", and the token
    # stub stays a number, so session totals remain consistent
    texts[is.na(texts)] <- ""

    response <- vapply(texts, function(t) as.character(responder(t %||% "")),
                       character(1), USE.NAMES = FALSE)

    df$response_text <- response
    # rep(), not a scalar, so a zero-row experiments frame passes through
    df$success <- rep(TRUE, n)
    df$sent_tokens <- pmax(1L, nchar(texts) %/% 4L)
    df$rec_tokens <- pmax(1L, nchar(response) %/% 4L)
    df$total_tokens <- df$sent_tokens + df$rec_tokens
    # paste0() treats a zero-length argument as "", yielding a length-1 result;
    # keep the ids zero-length for a zero-row frame
    df$response_id <- if (n > 0) paste0("demo-", seq_len(n)) else character(0)
    annotate_demo_result(df)
  }
  structure(runner, class = c("llmrshiny_demo_runner", "function"))
}

#' Build an execution function for a mode
#'
#' @param mode `"demo"` or `"live"`.
#' @param responder Optional demonstration responder (see [demo_runner()]).
#' @return A callable function that accepts a request data frame.
#' @export
build_runner <- function(mode, responder = NULL) {
  # An unrecognized mode (a typo) must not silently fall through to the live,
  # key-consuming path; require an explicit known mode.
  if (!identical(mode, "demo") && !identical(mode, "live")) {
    stop("`mode` must be \"demo\" or \"live\", not ",
         deparse(mode), ".", call. = FALSE)
  }
  if (identical(mode, "demo")) return(demo_runner(responder))

  function(experiments, ...) {
    if (!pkg_available("LLMR")) {
      stop(
        "Package 'LLMR' is required for live runs. Install it with install.packages(\"LLMR\").",
        call. = FALSE
      )
    }
    valid <- is.data.frame(experiments) &&
      all(c("config", "messages") %in% names(experiments)) &&
      is.list(experiments$config) && is.list(experiments$messages) &&
      all(vapply(experiments$config, inherits, logical(1), "llm_config"))
    if (!valid) {
      stop(
        "`experiments` must contain `config` and `messages` list-columns, with an `llm_config` in each row.",
        call. = FALSE
      )
    }
    LLMR::call_llm_par(experiments, ...)
  }
}

#' Mark a result frame as demonstration output
#'
#' Marks a result produced without a live model: a `run_mode` column set to
#' `"demo"`, a `demo_notice` column, and the `llmrshiny_demo_result` class that
#' [is_demo_result()] tests and [demo_banner_ui()] announces. [demo_runner()]
#' applies it automatically; call it directly for demonstration results built
#' some other way.
#'
#' @param x A data frame of results.
#' @return `x` with the two source columns and the demonstration class added.
#' @examples
#' annotate_demo_result(data.frame(response_text = "stub"))
#' @export
annotate_demo_result <- function(x) {
  n <- NROW(x)
  x$run_mode <- rep("demo", n)
  x$demo_notice <- rep(demo_notice(), n)
  class(x) <- unique(c("llmrshiny_demo_result", class(x)))
  x
}

#' Is a result a demonstration result?
#' @param x A result object.
#' @return `TRUE` when the result has the demonstration class or source fields.
#' @export
is_demo_result <- function(x) {
  inherits(x, "llmrshiny_demo_result") ||
    (is.list(x) && "run_mode" %in% names(x) && any(x$run_mode %in% "demo"))
}

#' @param x A demonstration result.
#' @param ... Passed to the next print method.
#' @rdname demo_runner
#' @export
print.llmrshiny_demo_result <- function(x, ...) {
  cat(demo_notice(), "\n", sep = "")
  shown <- x
  shown$demo_notice <- NULL
  class(shown) <- setdiff(class(shown), "llmrshiny_demo_result")
  print(shown, ...)
  invisible(x)
}

#' A banner announcing a demonstration result
#' @return A warning card.
#' @export
demo_banner_ui <- function() {
  bslib::card(
    class = "border-warning",
    bslib::card_body(shiny::tags$strong(demo_notice()))
  )
}

#' Summarize call counts and token usage
#'
#' Reads token counts from a result data frame for use by [usage_add()].
#'
#' @details
#' A row's total is its reported `total_tokens` value. If it is missing,
#' the total is computed from `sent_tokens` and `rec_tokens` when both are
#' known. Each component is summed over its reported values, including
#' failed calls. An absent column supplies missing values. Nonnumeric,
#' negative, fractional, and non-finite values are treated as missing with
#' one warning per column. A logical column containing only `NA` is accepted
#' as missing without a warning.
#'
#' The integer `unknown` count includes live rows missing any count after
#' computing the total. `sent_unknown` and `received_unknown` count rows
#' missing the respective component. A row with `rowpack_row > 1` is excluded
#' from these counts only when a live row with the same `rowpack_id` and
#' `rowpack_row == 1` is present.
#'
#' Call identifiers are considered in the order `call_id`, `request_id`,
#' then `response_id`. The first column with complete identifiers on at
#' least one live row and on every nonfailed live row is used. Distinct
#' identifiers count as calls. A list-column may contain several identifiers
#' per row; missing or empty entries make that row incomplete. Failed rows
#' (`success = FALSE`) with incomplete identifiers add one call each. If an identifier column exists
#' but every live row is failed and unidentified, each row counts as one call.
#'
#' If no identifier column qualifies, live rows are counted as result rows.
#' Incomplete identifiers on nonfailed rows produce a warning. Rows with
#' `run_mode = "demo"` add result rows and no tokens. If `run_mode` is absent,
#' the `llmrshiny_demo_result` class marks the whole frame as demonstration
#' output.
#'
#' @param x A result data frame, or a list containing data frames. A data
#'   frame supplied directly is used as given. List components are ranked
#'   by their columns in the following order.
#'   \enumerate{
#'     \item Both call identifiers and tokens.
#'     \item Call identifiers only.
#'     \item Tokens only.
#'     \item Neither.
#'   }
#'   Ties use the first frame and produce a warning naming the candidates.
#' @param fallback_calls A nonnegative whole-number call count used when
#'   `x` contains no data frame. A positive count has unknown usage.
#'   `NA` is retained with a warning. Counts must fit in an R integer.
#' @return A named list with double token subtotals `sent`, `received`, and
#'   `total`, and integer counts `unknown`, `sent_unknown`, and
#'   `received_unknown`. A subtotal is `NA` when no value is reported and
#'   `unknown` is positive; otherwise an empty subtotal is zero.
#'
#'   The list contains `calls` when an identifier column qualifies or the
#'   fallback is used. It also contains `calls` when an identifier column
#'   exists and every live row is failed and unidentified. Otherwise it
#'   contains `result_rows`. Demonstration rows add `result_rows`, including
#'   when identifiable live calls are present.
#' @examples
#' calls <- data.frame(response_id = c("r1", "r2", "r3"),
#'                     sent_tokens = c(100L, 110L, NA),
#'                     rec_tokens = c(20L, 21L, NA))
#' extract_token_counts(calls)
#' @export
extract_token_counts <- function(x, fallback_calls = 0L) {
  df <- find_call_frame(x)
  if (is.null(df)) {
    n <- count_field(fallback_calls, "fallback_calls")
    known <- if (identical(n, 0L)) 0 else NA_real_
    return(list(calls = n, sent = known, received = known, total = known,
                unknown = n, sent_unknown = n, received_unknown = n))
  }

  n <- NROW(df)
  demo <- if ("run_mode" %in% names(df)) {
    df[["run_mode"]] %in% "demo"
  } else {
    rep(inherits(df, "llmrshiny_demo_result"), n)
  }
  live <- !demo
  sent <- token_column(df, "sent_tokens")[live]
  received <- token_column(df, "rec_tokens")[live]
  total <- token_column(df, "total_tokens")[live]
  total <- ifelse(is.na(total) & !is.na(sent) & !is.na(received),
                  sent + received, total)
  failed <- if ("success" %in% names(df)) {
    df[["success"]] %in% FALSE
  } else {
    rep(FALSE, n)
  }
  pack <- df[["rowpack_row"]]
  pack_id <- df[["rowpack_id"]]
  follow_on <- rep(FALSE, n)
  if (is.numeric(pack) && !is.null(pack_id)) {
    first <- pack_id[live & !is.na(pack) & pack == 1 & !is.na(pack_id)]
    follow_on <- !is.na(pack) & pack > 1 & !is.na(pack_id) & pack_id %in% first
  }
  unknown <- sum((is.na(sent) | is.na(received) | is.na(total)) &
                   !follow_on[live])
  sent_unknown <- sum(is.na(sent) & !follow_on[live])
  received_unknown <- sum(is.na(received) & !follow_on[live])
  observed <- function(v) {
    if (any(!is.na(v)) || unknown == 0L) {
      as.double(sum(v, na.rm = TRUE))
    } else {
      NA_real_
    }
  }

  calls <- count_calls(df, live, failed)
  count <- list()
  if (!is.null(calls)) count$calls <- calls
  if (is.null(calls) || any(demo)) {
    count$result_rows <- as.integer(sum(demo) + if (is.null(calls)) sum(live) else 0L)
  }
  c(count, list(sent = observed(sent), received = observed(received),
                total = observed(total), unknown = as.integer(unknown),
                sent_unknown = as.integer(sent_unknown),
                received_unknown = as.integer(received_unknown)))
}

# The data frame in `x` that holds call-level usage: `x` itself, or the first
# frame in a list ranked by identifiers, then token columns. Tied candidates
# use list order and produce a warning so the selection is visible.
find_call_frame <- function(x) {
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) return(NULL)
  positions <- which(vapply(x, is.data.frame, logical(1)))
  frames <- x[positions]
  if (!length(frames)) return(NULL)
  has_any <- function(cols) {
    vapply(frames, function(d) any(cols %in% names(d)), logical(1))
  }
  tokens <- has_any(c("sent_tokens", "rec_tokens", "total_tokens"))
  ids <- has_any(c("call_id", "request_id", "response_id"))
  rank <- ifelse(tokens & ids, 1L, ifelse(ids, 2L, ifelse(tokens, 3L, 4L)))
  best <- which(rank == min(rank))
  if (length(best) > 1L) {
    labels <- names(frames)
    if (is.null(labels)) labels <- rep("", length(frames))
    unnamed <- is.na(labels) | !nzchar(labels)
    labels[unnamed] <- as.character(positions[unnamed])
    warning("Several call frames have the same rank: ",
            paste(labels[best], collapse = ", "), "; using ", labels[best[1]],
            ".", call. = FALSE)
  }
  frames[[best[1]]]
}

# A token column as numbers, NA where a row reports nothing. An absent or
# entirely missing column reports nothing; any other non-numeric column is not
# read, since factor codes or text are not counts.
token_column <- function(df, column) {
  v <- df[[column]]
  n <- NROW(df)
  if (is.null(v) || (is.logical(v) && all(is.na(v)))) return(rep(NA_real_, n))
  if (!is.numeric(v)) {
    warning(sprintf(
      "Token column `%s` is %s, not numeric; its counts are treated as unknown.",
      column, class(v)[[1]]
    ), call. = FALSE)
    return(rep(NA_real_, n))
  }
  invalid <- is.nan(v) | (!is.na(v) & (!is.finite(v) | v < 0 | v != trunc(v)))
  if (any(invalid)) {
    warning(sprintf(
      "Token column `%s` contains invalid counts; they are treated as unknown.",
      column), call. = FALSE)
    v[invalid] <- NA_real_
  }
  as.numeric(v)
}

# The number of distinct calls among the `rows` selected, from the first
# identifier column that covers them all; NULL when no column does. Failed
# rows need no identifier and count as one call each.
count_calls <- function(df, rows, failed) {
  if (!any(rows)) return(NULL)
  complete <- function(id) {
    if (is.list(id)) return(length(id) > 0L && all(vapply(id, complete, logical(1))))
    id <- as.character(id)
    length(id) > 0L && all(!is.na(id) & nzchar(id))
  }
  partial <- NULL
  unidentified_failures <- FALSE
  for (column in intersect(c("call_id", "request_id", "response_id"), names(df))) {
    v <- df[[column]]
    ids <- lapply(v[rows], function(id) as.character(unlist(id, use.names = FALSE)))
    has <- vapply(v[rows], complete, logical(1))
    if (!any(has) && all(failed[rows])) {
      unidentified_failures <- TRUE
      next
    }
    if (all(has | failed[rows])) {
      return(as.integer(length(unique(unlist(ids[has]))) + sum(!has)))
    }
    if (is.null(partial)) partial <- c(column, sum(!(has | failed[rows])))
  }
  if (unidentified_failures) return(as.integer(sum(rows)))
  if (!is.null(partial)) {
    warning(sprintf(
      "%s of %d rows have no `%s`, so calls cannot be counted; counting result rows instead.",
      partial[[2]], sum(rows), partial[[1]]
    ), call. = FALSE)
  }
  NULL
}

# A count field: absent counts as 0, and NA is an unknown count, kept as NA
# with a warning, since usage accounting runs after a finished run and must
# not end the session. Anything else must be one nonnegative whole number.
count_field <- function(x, name) {
  if (!length(x)) return(0L)
  if (length(x) == 1L && is.na(x) && !is.nan(x)) {
    warning(sprintf("`%s` is NA; the count is recorded as unknown.", name),
            call. = FALSE)
    return(NA_integer_)
  }
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
      x < 0 || x != round(x) || x > .Machine$integer.max) {
    stop(sprintf("`%s` must be one nonnegative whole number.", name),
         call. = FALSE)
  }
  as.integer(x)
}
