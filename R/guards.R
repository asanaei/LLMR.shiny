# guards.R ---------------------------------------------------------------------
# Guards shared by every LLMR-family GUI: package availability, installation
# guidance, error-to-banner mapping, and an auth-aware call wrapper.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

pkg_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

github_remote_for <- function(package) {
  remotes <- c(
    LLMRcontent = "asanaei/LLMRcontent",
    LLMRpanel = "asanaei/LLMRpanel",
    FocusGroup = "asanaei/FocusGroup"
  )
  if (package %in% names(remotes)) remotes[[package]] else NULL
}

#' Install-guidance card for a missing package
#'
#' @param package Package name.
#' @param title Card title (defaults to the package name).
#' @return A `bslib::card` with an installation command.
#' @export
install_guidance_ui <- function(package, title = package) {
  remote <- github_remote_for(package)
  command <- if (is.null(remote)) {
    paste0('install.packages("', package, '")')
  } else {
    paste0('remotes::install_github("', remote, '")')
  }
  bslib::card(
    class = "border-warning",
    bslib::card_header(paste(title, "install needed")),
    bslib::card_body(
      shiny::tags$p(
        paste0(package, " is not installed. This app keeps running, but this workflow needs the package.")
      ),
      shiny::tags$pre(command)
    )
  )
}

#' Error category of a caught condition
#'
#' LLMR's classed errors carry their category in the condition class
#' (`llmr_api_auth_error`, `llmr_api_rate_limit_error`, and so on); the class is
#' read first. A plain `category` field or attribute is honored as a fallback
#' for conditions built by other tools.
#'
#' @param e A condition.
#' @return A length-1 character category (e.g. `"auth"`, `"rate_limit"`,
#'   `"param"`, `"server"`, `"unknown"`), or `NA`.
#' @export
condition_category <- function(e) {
  hit <- grep("^llmr_api_(.+)_error$", class(e), value = TRUE)
  if (length(hit) > 0) {
    return(sub("^llmr_api_(.+)_error$", "\\1", hit[[1]]))
  }
  e$category %||% attr(e, "category", exact = TRUE) %||% NA_character_
}

# TRUE when the condition is an LLMR llmr_api_auth_error or its category
# otherwise resolves to "auth". This helper is used by llmr_error_banner();
# the exported classifier is condition_category().
is_auth_error <- function(e) {
  inherits(e, "llmr_api_auth_error") || identical(condition_category(e), "auth")
}

# Map a caught LLM error to a banner card: an auth failure becomes a
# key-state banner naming the environment variables to set; any other error
# shows its message verbatim. Used only inside safe_llmr_call(), whose result
# carries the card.
llmr_error_banner <- function(e, provider = NULL) {
  if (is_auth_error(e)) {
    ks <- key_state(provider %||% "groq")
    return(
      bslib::card(
        class = "border-warning",
        bslib::card_header("Live run disabled"),
        bslib::card_body(
          shiny::tags$p("No API key was available for live execution."),
          shiny::tags$p(
            paste0(
              "Set one of these in ~/.Renviron, restart R, and run the app again: ",
              paste(ks$env_vars, collapse = " or "),
              "."
            )
          )
        )
      )
    )
  }

  bslib::card(
    class = "border-warning",
    bslib::card_header("Run stopped"),
    bslib::card_body(
      shiny::tags$p("The workflow returned an error before producing a result."),
      shiny::tags$pre(conditionMessage(e))
    )
  )
}

#' Run an expression, capturing any error as a banner
#'
#' Evaluates `expr`; on error returns a list with `ok = FALSE` and a ready-made
#' `ui` banner (auth-aware) instead of stopping. On success returns
#' `ok = TRUE` and the `value`.
#'
#' @param expr An expression to evaluate.
#' @param provider Optional provider id, for an auth banner.
#' @return `list(ok, value, error, ui)`.
#' @export
safe_llmr_call <- function(expr, provider = NULL) {
  tryCatch(
    list(ok = TRUE, value = eval.parent(substitute(expr)), error = NULL, ui = NULL),
    error = function(e) {
      list(ok = FALSE, value = NULL, error = e, ui = llmr_error_banner(e, provider))
    }
  )
}
