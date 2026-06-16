# display.R --------------------------------------------------------------------
# The display layer over the shared LLMR generics, plus the standard sidebar and
# the shared reactive context every GUI's server builds. These are what let a
# new GUI be a thin module layer: it calls shell_sidebar() in the UI and
# shell_context() in the server and gets keys, runner, and cost wiring for free.

#' Render an object's `report()` prose, falling back to print output
#'
#' @param x A result object with an `LLMR::report()` method, or any object.
#' @param ... Passed to `report()`.
#' @return A character scalar of report text.
#' @export
report_text <- function(x, ...) {
  out <- tryCatch(
    if (requireNamespace("LLMR", quietly = TRUE)) LLMR::report(x, ...) else NULL,
    error = function(e) NULL
  )
  if (is.null(out)) {
    out <- paste(utils::capture.output(print(x)), collapse = "\n")
  }
  if (!is.character(out)) out <- paste(utils::capture.output(print(out)), collapse = "\n")
  paste(out, collapse = "\n")
}

#' Render an object's `diagnostics()` as a display table
#'
#' @param x A result object with an `LLMR::diagnostics()` method.
#' @param ... Passed to `diagnostics()`.
#' @return A data frame, or `NULL` when no method applies.
#' @export
diagnostics_table <- function(x, ...) {
  out <- tryCatch(
    if (requireNamespace("LLMR", quietly = TRUE)) LLMR::diagnostics(x, ...) else NULL,
    error = function(e) NULL
  )
  if (is.null(out)) return(NULL)
  as_display_table(out)
}

#' The standard GUI sidebar: provider, model, mode, key tile, cost tile
#'
#' @param id The module namespace (or `NULL` for top-level inputs).
#' @param default_provider Provider selected initially.
#' @return A `bslib::sidebar`.
#' @export
shell_sidebar <- function(id = NULL, default_provider = "groq") {
  ns <- if (is.null(id)) identity else shiny::NS(id)
  bslib::sidebar(
    width = 330,
    shiny::selectInput(ns("provider"), "Provider",
                       choices = provider_choices(), selected = default_provider),
    shiny::textInput(ns("model"), "Model",
                     value = provider_default_model(default_provider)),
    shiny::radioButtons(ns("run_mode"), "Mode",
                        choices = c("Demo" = "demo", "Live" = "live"),
                        selected = "demo", inline = TRUE),
    shiny::uiOutput(ns("key_state_tile")),
    shiny::uiOutput(ns("cost_tile"))
  )
}

#' Wire the standard sidebar and return the shared reactive context
#'
#' Call once at the top of a GUI's server with the top-level `input`, `output`,
#' `session`. It keeps the model field in sync with the provider, renders the
#' key and cost tiles, tracks usage, and returns a list of reactives and
#' mutators (`provider`, `model`, `mode`, `key`, `can_run`, `set_plan`,
#' `add_usage`) for the per-package modules to consume.
#'
#' @param input,output,session The top-level Shiny server arguments.
#' @return A shared-context list.
#' @export
shell_context <- function(input, output, session) {
  shiny::observeEvent(input$provider, {
    shiny::updateTextInput(session, "model",
                           value = provider_default_model(input$provider))
  }, ignoreInit = TRUE)

  cost_state <- shiny::reactiveVal(cost_empty())

  output$key_state_tile <- shiny::renderUI(key_state_tile(key_state(input$provider)))
  output$cost_tile <- shiny::renderUI(cost_tile(cost_state()))

  list(
    provider = shiny::reactive(input$provider),
    model = shiny::reactive(input$model),
    mode = shiny::reactive(input$run_mode),
    key = shiny::reactive(key_state(input$provider)),
    can_run = shiny::reactive({
      identical(input$run_mode, "demo") || isTRUE(key_state(input$provider)$found)
    }),
    set_plan = function(calls, label = "Next run") {
      cost_state(cost_set_plan(cost_state(), calls, label))
    },
    add_usage = function(tokens) {
      cost_state(cost_add_usage(cost_state(), tokens))
    }
  )
}
