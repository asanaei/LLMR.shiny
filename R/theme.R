#' Shared theme for an LLMR studio
#'
#' The family theme uses a common Bootstrap palette and a studio accent only
#' for the active navigation underline.
#'
#' @param studio One of `"content"`, `"panel"`, or `"focus"`.
#' @param ... Additional arguments passed to [bslib::bs_theme()]. The Bootstrap
#'   version, Bootswatch base, and family colors cannot be overridden.
#' @return A Bootstrap 5 `bslib` theme.
#' @export
#' @examples
#' theme <- llmr_theme("content")
#' if (interactive()) {
#'   shiny::shinyApp(
#'     bslib::page_navbar(
#'       bslib::nav_panel("Home", "Demo"),
#'       title = "LLMRcontent",
#'       theme = theme
#'     ),
#'     function(input, output, session) {}
#'   )
#' }
llmr_theme <- function(studio = c("content", "panel", "focus"), ...) {
  studio <- match.arg(studio)
  dots <- list(...)
  fixed <- c(
    "version", "preset", "bootswatch", "primary", "secondary", "success",
    "warning", "danger", "info"
  )
  conflict <- intersect(names(dots), fixed)
  if (length(conflict)) {
    stop(
      "The shared theme fixes: ", paste(conflict, collapse = ", "), ".",
      call. = FALSE
    )
  }

  accent <- switch(
    studio,
    content = "#009F8F",
    panel = "#559982",
    focus = "#D36D27"
  )
  args <- c(
    list(
      version = 5,
      bootswatch = "flatly",
      primary = "#2C3E50",
      secondary = "#6C757D",
      success = "#2E7D32",
      warning = "#A15C00",
      danger = "#B8322A",
      info = "#2C6E8F"
    ),
    dots
  )
  theme <- do.call(bslib::bs_theme, args)

  bslib::bs_add_rules(
    theme,
    paste0(
      ".navbar .nav-link, .nav-tabs .nav-link, .btn {",
      "text-transform: none; letter-spacing: normal;",
      "}",
      ".navbar-nav .nav-link.active, .nav-tabs .nav-link.active {",
      "box-shadow: inset 0 -3px 0 ", accent, ";",
      "}"
    )
  )
}
