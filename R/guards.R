# guards.R ---------------------------------------------------------------------
# Defensive helpers shared by every LLMR-family GUI: a null-coalescing operator,
# package-availability checks, install guidance, error-to-banner mapping, and a
# call wrapper that turns an auth failure into a key banner instead of a crash.

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL` or empty, in which case `y`.
#'
#' @param x,y Values; `x` is preferred when non-empty.
#' @return `x` if non-`NULL` and length > 0, otherwise `y`.
#' @name null-coalesce
#' @usage x \%||\% y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Is a package installed?
#'
#' @param package Package name.
#' @return `TRUE` if the package can be loaded.
#' @export
pkg_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

#' GitHub remote for an LLMR-family package
#'
#' @param package Package name.
#' @return A `"owner/repo"` string for `remotes::install_github()`.
#' @export
github_remote_for <- function(package) {
  remotes <- c(
    LLMR = "asanaei/LLMR",
    LLMRcontent = "asanaei/LLMRcontent",
    LLMRpanel = "asanaei/LLMRpanel",
    FocusGroup = "asanaei/FocusGroup"
  )
  remotes[[package]] %||% paste0("asanaei/", package)
}

#' Install-guidance card for a missing package
#'
#' @param package Package name.
#' @param title Card title (defaults to the package name).
#' @return A `bslib::card` with an `install_github()` snippet.
#' @export
install_guidance_ui <- function(package, title = package) {
  remote <- github_remote_for(package)
  bslib::card(
    class = "border-warning",
    bslib::card_header(paste(title, "install needed")),
    bslib::card_body(
      shiny::tags$p(
        paste0(package, " is not installed. This app keeps running, but this workflow needs the package.")
      ),
      shiny::tags$pre(paste0('remotes::install_github("', remote, '")'))
    )
  )
}

#' Error category of a caught condition
#'
#' Reads the `category` field that LLMR's classed errors carry.
#'
#' @param e A condition.
#' @return A length-1 character category, or `NA`.
#' @export
condition_category <- function(e) {
  e$category %||% attr(e, "category", exact = TRUE) %||% NA_character_
}

#' Is a caught condition an auth error?
#'
#' @param e A condition.
#' @return `TRUE` when the condition's category is `"auth"`.
#' @export
is_auth_error <- function(e) {
  identical(condition_category(e), "auth")
}

#' Map a caught LLM error to a banner card
#'
#' An auth failure becomes a key-state banner naming the environment variables
#' to set; any other error shows its message verbatim. Neither crashes the app.
#'
#' @param e A caught condition.
#' @param provider Optional provider id, for the key banner.
#' @return A `bslib::card`.
#' @export
llmr_error_banner <- function(e, provider = NULL) {
  if (is_auth_error(e)) {
    ks <- key_state(provider %||% "groq")
    return(
      bslib::card(
        class = "border-warning",
        bslib::card_header("Live run disabled"),
        bslib::card_body(
          shiny::tags$p("No API key was available to the LLM runner."),
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
