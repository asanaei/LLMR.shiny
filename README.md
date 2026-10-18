# LLMR.shiny <img src="man/figures/logo.png" align="right" width="120" alt="LLMR.shiny icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMR.shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMR.shiny/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMR.shiny/)
<!-- badges: end -->

## Who this package is for

LLMR.shiny provides reusable Shiny user interface and server components for
authors of LLMR family applications. Research users normally encounter it as a
dependency of LLMRpanel, LLMRcontent, or FocusGroup. The package does not
implement their research workflows.

## Build an LLMR family Shiny interface

`shell_sidebar()` supplies provider, model, execution-mode, credential, and
usage controls. `shell_context()` connects those controls to shared reactive
values and server helpers, and each workflow module receives the same context.
The app below runs offline. Its module echoes the texts entered, records the
planned calls before a run and the result rows after it, and shows a card in
place of a result when the input is empty or the mode is Live.

```r
library(shiny)
library(LLMR.shiny)

echo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textAreaInput(ns("texts"), "One text per line", "first text\nsecond text"),
    actionButton(ns("run"), "Run"),
    uiOutput(ns("status")),
    tableOutput(ns("result"))
  )
}

echo_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    requests <- reactive({
      lines <- trimws(unlist(strsplit(as.character(input$texts), "\n", fixed = TRUE)))
      data.frame(text = lines[nzchar(lines)])
    })
    observeEvent(requests(), shared$set_plan(nrow(requests()), "Echo run"))

    attempt <- reactiveVal(NULL)
    observeEvent(input$run, {
      attempt(safe_llmr_call({
        if (!identical(shared$mode(), "demo")) {
          stop("This example makes no live calls; switch the mode to Demo.")
        }
        if (nrow(requests()) == 0L) stop("Enter at least one text.")
        result <- build_runner("demo")(requests())
        shared$add_usage(extract_token_counts(result))
        result
      }))
    })

    output$status <- renderUI({
      if (!shared$can_run()) return(live_run_blocker_ui(shared$key()))
      req(attempt())
      if (attempt()$ok) demo_banner_ui() else attempt()$ui
    })
    output$result <- renderTable({
      req(attempt()$ok)
      as_display_table(attempt()$value[c("text", "response_text")])
    })
  })
}

ui <- bslib::page_sidebar(
  title = "Shared context",
  sidebar = shell_sidebar(),
  echo_ui("echo")
)

server <- function(input, output, session) {
  shared <- shell_context(input, output, session)
  echo_server("echo", shared)
}

shinyApp(ui, server)
```

The context gives a module `provider()`, `model()`, `mode()`, the generation
settings, `key()`, `can_run()`, `set_plan()`, and `add_usage()`. `can_run()`
is `TRUE` in demonstration mode or when the selected provider's key is set. It
does not examine a workflow's data or instrument, so each module validates its
own input, as `echo_server()` does for empty input.

## Providers, models, and credential status

`provider_registry()` defines the provider choices and optional model defaults.
Model fields start blank unless local defaults are supplied:

```r
options(LLMR.shiny.default_models = c(groq = "your-current-model"))
provider_registry()
```

`key_state()` reports whether a provider key is present and names the
environment variable in use. It does not return the key value. Applications
can display that status with `key_state_tile()` or explain a blocked live run
with `live_run_blocker_ui()`. `build_llm_config()` constructs the configuration
used by live execution and requires the optional LLMR package.

## Import data and map columns

`read_csv_upload()` reads a Shiny `fileInput()` value. By default it infers
each column's type, as `read.csv()` does, so the identifiers `001` and `01`
both become 1 and the text "NA" becomes missing. Reading every column as
written keeps identifiers and text intact. Column mapping then validates the
selected names before creating the `text` and optional `labels` columns
expected by an application workflow.

```r
uploaded <- read_csv_upload(input$file, col_classes = "character",
                            na_strings = character(0))
validate_column_mapping(uploaded, text_col = input$text_col,
                        label_col = input$label_col)
mapped <- map_columns(uploaded, text_col = input$text_col,
                      label_col = input$label_col)
```

Columns that should be numbers or logical values can be converted afterwards
with `utils::type.convert(x, as.is = TRUE, na.strings = "NA")`, which gives
the types of the default reading. `read_csv_path()` reads a file path with
the same options.

## Select personas

The persona selector is a reusable Shiny module. Its server returns the row
indices selected from a supplied persona data frame.

```r
# UI
persona_selector_ui("personas")

# server
selected_rows <- persona_selector_server(
  "personas",
  data = shiny::reactive(personas)
)
```

The module uses `LLMR::llm_persona_overview()` when the data support it and
LLMR is installed. Otherwise, it displays the first columns. The optional DT
package supplies the selectable table.

## Display results and handle errors

`report_text()` and `diagnostics_table()` display results returned by
`LLMR::report()` and `LLMR::diagnostics()`. `as_display_table()` prepares data
frames, matrices, or a selected list component for tabular display.

`safe_llmr_call()` evaluates an expression and returns a status list. On
success, the value is in `value`. On error, the caught condition is in `error`
and a Shiny error card is in `ui`. The exported `condition_category()`
identifies authentication, rate-limit, parameter, and server conditions when
that information is available.

```r
attempt <- safe_llmr_call(
  build_llm_config(shared$provider(), shared$model()),
  provider = shared$provider()
)

if (!attempt$ok) {
  output$run_error <- shiny::renderUI(attempt$ui)
  category <- condition_category(attempt$error)
}
```

## Track planned and realized usage

The `usage_*()` helpers keep planned calls, realized calls or result rows, and
token counts in a session record. `extract_token_counts()` reads available
counts from result objects before `usage_add()` updates the record.

```r
response_rows <- data.frame(
  response_id = c("r1", "r2"),
  sent_tokens = c(20L, 25L),
  rec_tokens = c(4L, 6L)
)
usage <- usage_set_plan(usage_empty(), calls = 120, label = "Panel run")
usage <- usage_add(usage, extract_token_counts(response_rows))
usage_tile(usage)
```

Applications built with `shell_context()` can use `set_plan()` and
`add_usage()` to update the same record.

## Live and demonstration execution

`build_runner("live")` returns a function that passes a request data frame to
`LLMR::call_llm_par()`. `build_runner("demo")` returns an offline response
function for examples and interface demonstrations. Demonstration results are
marked by `annotate_demo_result()` and can be checked with `is_demo_result()`;
`demo_notice()` and `demo_banner_ui()` label them in application output.

```r
respond <- build_runner("demo")
demo_result <- respond(data.frame(text = c("first", "second")))
is_demo_result(demo_result)
```

`demo_runner()` also accepts a response function when an application needs
fixed example output.

## Install for GUI development

Application packages install LLMR.shiny as a dependency. GUI authors can
install it directly from CRAN:

```r
install.packages("LLMR.shiny")
```

or the development version:

```r
remotes::install_github("asanaei/LLMR.shiny")
```

LLMR is optional for package installation. Live execution and live
configuration construction require it:

```r
install.packages("LLMR")
```

## Related packages

LLMR.shiny provides the shared Shiny components used by the graphical
interfaces in the LLMR family. [LLMR](https://asanaei.github.io/LLMR/) is the
common provider interface on CRAN.
[LLMRcontent](https://asanaei.github.io/LLMRcontent/) codes text with codebooks
and builds replication archives.
[LLMRpanel](https://asanaei.github.io/LLMRpanel/) administers survey and
experimental instruments to panels of model personas.
[FocusGroup](https://asanaei.github.io/FocusGroup/) runs moderated group
discussions. [LLMRagent](https://asanaei.github.io/LLMRagent/) provides tools
for agent experiments. The
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/) describes the
package boundaries.

## Contributing

Report bugs and feature requests in the
[GitHub repository](https://github.com/asanaei/LLMR.shiny). Pull requests may
be submitted there.

## License

This project uses the MIT License; see `LICENSE`.
