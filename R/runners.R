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
#' @param ... Passed to [LLMR::llm_config()] (e.g. `temperature`).
#' @return An `LLMR::llm_config`.
#' @export
build_llm_config <- function(provider, model, ...) {
  if (!pkg_available("LLMR")) {
    stop(
      "Package 'LLMR' is required to build a live config. Install it with install.packages(\"LLMR\").",
      call. = FALSE
    )
  }
  do.call(
    LLMR::llm_config,
    c(list(provider = provider, model = model), list(...))
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

#' Extract call/token counts from a result frame
#'
#' @param x A result data frame (or list containing one).
#' @param fallback_calls Call count to use when `x` has no result frame.
#' @return A list with token totals and either `calls`, when explicit call
#'   provenance is available, or `result_rows`.
#' @export
extract_token_counts <- function(x, fallback_calls = 0L) {
  df <- if (is.data.frame(x)) x else NULL
  if (is.null(df) && is.list(x)) {
    dfs <- Filter(is.data.frame, x)
    if (length(dfs) > 0) df <- dfs[[1]]
  }
  if (is.null(df)) {
    return(list(calls = fallback_calls, sent = 0L, received = 0L, total = 0L))
  }

  sent_rows <- as.numeric(df$sent_tokens %||% 0)
  rec_rows <- as.numeric(df$rec_tokens %||% 0)
  sent <- sum(sent_rows, na.rm = TRUE)
  received <- sum(rec_rows, na.rm = TRUE)
  # Prefer the per-row total column, but where a row's total is NA fall back to
  # that row's sent + received, so the total stays consistent with the parts.
  total <- if (!is.null(df$total_tokens)) {
    total_rows <- as.numeric(df$total_tokens)
    fallback_rows <- ifelse(is.na(sent_rows), 0, sent_rows) +
      ifelse(is.na(rec_rows), 0, rec_rows)
    sum(ifelse(is.na(total_rows), fallback_rows, total_rows), na.rm = TRUE)
  } else {
    sent + received
  }
  ids <- NULL
  for (column in intersect(c("call_id", "request_id", "response_id"), names(df))) {
    candidate <- as.character(unlist(df[[column]], use.names = FALSE))
    candidate <- candidate[!is.na(candidate) & nzchar(candidate)]
    if (length(candidate) > 0L) {
      ids <- candidate
      break
    }
  }
  count <- if (!is.null(ids) && !is_demo_result(df)) {
    list(calls = as.integer(length(unique(ids))))
  } else {
    list(result_rows = as.integer(NROW(df)))
  }

  c(count, list(sent = as.integer(sent), received = as.integer(received),
                total = as.integer(total)))
}
