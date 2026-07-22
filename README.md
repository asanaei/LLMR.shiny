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
values and server helpers.

In your UI:

```r
bslib::page_navbar(
  title = "MyStudio",
  sidebar = LLMR.shiny::shell_sidebar(),
  bslib::nav_panel("Workflow", my_module_ui("work"))
)
```

In your server:

```r
function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)
  my_module_server("work", shared)
}
```

`shared` gives your module `provider()`, `model()`, `mode()`, `key()`,
`can_run()`, `set_plan()`, and `add_usage()`. A change here is available to any
module that receives the same context.

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

`read_csv_upload()` reads a Shiny `fileInput()` value. Column mapping validates
the selected names before creating the `text` and optional `labels` columns
expected by an application workflow.

```r
uploaded <- read_csv_upload(input$file)
validate_column_mapping(uploaded, text_col = input$text_col,
                         label_col = input$label_col)
mapped <- map_columns(uploaded, text_col = input$text_col,
                      label_col = input$label_col)
```

`read_csv_path()` provides the same base R CSV import for a file path.

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
