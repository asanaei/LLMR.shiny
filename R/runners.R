# runners.R --------------------------------------------------------------------
# Runners. Demo mode is a deterministic offline stub returning LLMR-shaped
# response columns, marked as a demo; live mode returns NULL so the calling
# package uses its own default runner (LLMR::call_llm_par via its .runner seam).

#' Demo-result notice string
#'
#' @return The marker text stamped on every demo result.
#' @export
demo_notice <- function() {
  "DEMO RESULT -- offline stub, not a model, not a finding"
}

#' Build an LLM config, falling back to a plain list without LLMR
#'
#' @param provider,model Provider and model ids.
#' @param ... Passed to [LLMR::llm_config()] (e.g. `temperature`).
#' @return An `llm_config` when LLMR is available, else a tagged list.
#' @export
build_llm_config <- function(provider, model, ...) {
  dots <- list(...)
  if (requireNamespace("LLMR", quietly = TRUE)) {
    return(do.call(LLMR::llm_config, c(list(provider = provider, model = model), dots)))
  }
  structure(
    c(list(provider = provider, model = model), dots),
    class = "llmrshiny_config"
  )
}

#' A deterministic offline demo runner
#'
#' Returns a function with the `.runner` contract `(experiments, ...)` that adds
#' LLMR-shaped response columns. The per-row response text is decided by
#' `responder`, a function `(text) -> character`. Results are marked as demo.
#'
#' @param responder A function mapping a single input text to a response string.
#'   Defaults to echoing a short stub.
#' @param text_cols Candidate column names to read the input text from.
#' @return A runner function of class `llmrshiny_demo_runner`.
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

    response <- vapply(texts, function(t) as.character(responder(t %||% "")),
                       character(1), USE.NAMES = FALSE)

    df$response_text <- response
    df$success <- TRUE
    df$sent_tokens <- pmax(1L, nchar(texts) %/% 4L)
    df$rec_tokens <- pmax(1L, nchar(response) %/% 4L)
    df$total_tokens <- df$sent_tokens + df$rec_tokens
    df$response_id <- paste0("demo-", seq_len(n))
    annotate_demo_result(df)
  }
  structure(runner, class = c("llmrshiny_demo_runner", "function"))
}

#' Build a runner for a mode
#'
#' @param mode `"demo"` or `"live"`.
#' @param responder Optional demo responder (see [demo_runner()]).
#' @return A demo runner for `"demo"`, or `NULL` for `"live"` (use the package
#'   default downstream).
#' @export
build_runner <- function(mode, responder = NULL) {
  # An unrecognized mode (a typo) must not silently fall through to the live,
  # key-consuming path; require an explicit known mode.
  if (!identical(mode, "demo") && !identical(mode, "live")) {
    stop("`mode` must be \"demo\" or \"live\", not ",
         deparse(mode), ".", call. = FALSE)
  }
  if (identical(mode, "demo")) demo_runner(responder) else NULL
}

#' Mark a result as a demo result
#' @param x A result object.
#' @return `x` with a `llmrshiny_demo` attribute.
#' @export
annotate_demo_result <- function(x) {
  attr(x, "llmrshiny_demo") <- TRUE
  x
}

#' Is a result a demo result?
#' @param x A result object.
#' @return `TRUE` when marked by [annotate_demo_result()].
#' @export
is_demo_result <- function(x) {
  isTRUE(attr(x, "llmrshiny_demo", exact = TRUE))
}

#' A banner announcing a demo result
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
#' @param fallback_calls Call count to assume when none can be read.
#' @return A list `list(calls, sent, received, total)`.
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

  sent <- sum(as.numeric(df$sent_tokens %||% 0), na.rm = TRUE)
  received <- sum(as.numeric(df$rec_tokens %||% 0), na.rm = TRUE)
  # Prefer the per-row total column; fall back to sent + received (both already
  # row-summed). Parentheses matter: %||% binds looser than +.
  total <- if (!is.null(df$total_tokens)) {
    sum(as.numeric(df$total_tokens), na.rm = TRUE)
  } else {
    sent + received
  }
  calls <- if ("response_text" %in% names(df)) NROW(df) else fallback_calls

  list(calls = as.integer(calls), sent = as.integer(sent),
       received = as.integer(received), total = as.integer(total))
}
