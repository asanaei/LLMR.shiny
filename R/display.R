# display.R --------------------------------------------------------------------
# Display helpers for results returned by the shared LLMR generics, plus the
# standard sidebar, shared reactive values, and server helpers. A GUI calls
# shell_sidebar() in the UI and shell_context() in the server to connect keys,
# execution, and usage state.

llmr_method_exists <- function(generic, x) {
  if (!pkg_available("LLMR")) return(FALSE)
  any(vapply(class(x), function(class) {
    !is.null(utils::getS3method(
      generic, class, optional = TRUE, envir = asNamespace("LLMR")
    ))
  }, logical(1)))
}

#' Output container for long text results
#'
#' @param id Output id passed to [shiny::verbatimTextOutput()].
#' @param height CSS height of the scrollable container.
#' @return A bordered, scrollable Shiny output container.
#' @export
#' @examples
#' text_block_output("report")
text_block_output <- function(id, height = "18rem") {
  output <- shiny::tagAppendAttributes(
    shiny::verbatimTextOutput(id),
    style = paste(
      "min-height: 100%; max-height: none; overflow: visible;",
      "margin: 0; flex: 0 0 auto;"
    )
  )
  shiny::tags$div(
    class = "llmr-text-block border rounded p-3 bg-body-tertiary",
    style = paste0(
      "height: ", shiny::validateCssUnit(height),
      "; overflow: auto; flex: 0 0 auto;"
    ),
    output
  )
}

#' Inline help tooltip
#'
#' @param text Help text shown by the tooltip and exposed to assistive
#'   technology.
#' @param placement Tooltip placement relative to the icon.
#' @return A focusable information icon wrapped in a `bslib` tooltip.
#' @export
#' @examples
#' shiny::tags$label(
#'   "Turn-taking flow",
#'   help_tip("How the next speaker is chosen.")
#' )
help_tip <- function(text, placement = "right") {
  trigger <- shiny::icon(
    "circle-info",
    class = "small text-body-secondary llmr-help-tip"
  )
  trigger$attribs$role <- "img"
  trigger$attribs[["aria-label"]] <- text
  trigger$attribs$title <- text
  trigger$attribs$tabindex <- "0"

  bslib::tooltip(trigger, text, placement = placement)
}

#' Render an object's `report()` prose, falling back to print output
#'
#' @param x A result object with an `LLMR::report()` method, or any object.
#' @param ... Passed to `report()`.
#' @return A character scalar of report text.
#' @export
report_text <- function(x, ...) {
  if (!llmr_method_exists("report", x)) {
    return(paste(utils::capture.output(print(x)), collapse = "\n"))
  }
  out <- LLMR::report(x, ...)
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
  if (!llmr_method_exists("diagnostics", x)) return(NULL)
  out <- LLMR::diagnostics(x, ...)
  if (is.null(out)) return(NULL)
  as_display_table(out)
}

#' The standard GUI sidebar: mode, live configuration, and usage
#'
#' @param id The module namespace (or `NULL` for top-level inputs).
#' @param default_provider Provider selected initially.
#' @param max_tokens_help Help text for a blank maximum-output-token field.
#' @param max_tokens_placeholder Placeholder for the maximum-output-token field.
#' @return A `bslib::sidebar`.
#' @export
shell_sidebar <- function(
    id = NULL, default_provider = "groq",
    max_tokens_help = "Leave blank to use the model default.",
    max_tokens_placeholder = "Model default") {
  ns <- shiny::NS(id)
  max_tokens_input <- shiny::numericInput(
    ns("max_tokens"),
    shiny::tagList(
      "Max output tokens ",
      help_tip(max_tokens_help)
    ),
    value = NULL,
    min = 1,
    step = 1
  )
  max_tokens_input <- shiny::tagAppendAttributes(
    max_tokens_input,
    placeholder = max_tokens_placeholder,
    .cssSelector = "input"
  )

  bslib::sidebar(
    width = 330,
    shiny::radioButtons(ns("run_mode"), "Mode",
                        choices = c("Demo" = "demo", "Live" = "live"),
                        selected = "demo", inline = TRUE),
    shiny::conditionalPanel(
      condition = "input.run_mode === 'demo'",
      shiny::tags$p(
        class = "text-body-secondary small",
        "Bundled deterministic demo. No model, API key, or API calls."
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.run_mode === 'live'",
      shiny::selectInput(
        ns("provider"), "Provider",
        choices = provider_choices(), selected = default_provider
      ),
      shiny::textInput(
        ns("model"),
        shiny::tagList(
          "Model ",
          help_tip("Use the provider's own model id.")
        ),
        value = provider_default_model(default_provider),
        placeholder = "Model id from the selected provider"
      ),
      shiny::uiOutput(ns("key_state_tile")),
      bslib::accordion(
        bslib::accordion_panel(
          "Generation settings",
          shiny::numericInput(
            ns("temperature"),
            shiny::tagList(
              "Temperature ",
              help_tip("Higher values give more variable wording.")
            ),
            value = 0.7,
            min = 0,
            max = 2,
            step = 0.1
          ),
          max_tokens_input,
          shiny::selectInput(
            ns("reasoning_effort"),
            shiny::tagList(
              "Reasoning effort ",
              help_tip("Support depends on the provider and model.")
            ),
            choices = c(
              "Model default" = "",
              "Low" = "low",
              "Medium" = "medium",
              "High" = "high"
            ),
            selected = ""
          )
        ),
        open = FALSE
      ),
      ns = ns
    ),
    shiny::uiOutput(ns("usage_tile"))
  )
}

#' Wire the standard sidebar and return the shared reactive context
#'
#' Call once at the top of a GUI's server with the top-level `input`, `output`,
#' `session`. It keeps the model field in sync with the provider, renders the
#' key and usage tiles, tracks usage, and returns a list of reactives and
#' mutators (`provider`, `model`, `mode`, `temperature`, `max_tokens`,
#' `reasoning_effort`, `key`, `can_run`, `set_plan`, `add_usage`) for the
#' per-package modules to consume.
#'
#' @param input,output,session The top-level Shiny server arguments.
#' @return A shared-context list.
#' @export
shell_context <- function(input, output, session) {
  shiny::observeEvent(input$provider, {
    shiny::updateTextInput(session, "model",
                           value = provider_default_model(input$provider))
  }, ignoreInit = TRUE)

  usage_state <- shiny::reactiveVal(usage_empty())

  output$key_state_tile <- shiny::renderUI(key_state_tile(key_state(input$provider)))
  output$usage_tile <- shiny::renderUI(usage_tile(usage_state()))

  list(
    provider = shiny::reactive(input$provider),
    model = shiny::reactive(input$model),
    mode = shiny::reactive(input$run_mode),
    temperature = shiny::reactive(input$temperature),
    max_tokens = shiny::reactive(input$max_tokens),
    reasoning_effort = shiny::reactive(input$reasoning_effort),
    key = shiny::reactive(key_state(input$provider)),
    can_run = shiny::reactive({
      identical(input$run_mode, "demo") || isTRUE(key_state(input$provider)$found)
    }),
    set_plan = function(calls, label = "Next run") {
      usage_state(usage_set_plan(usage_state(), calls, label))
    },
    add_usage = function(tokens) {
      usage_state(usage_add(usage_state(), tokens))
    }
  )
}
