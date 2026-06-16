# LLMR.shiny

The shared Shiny substrate for the LLMR family of GUIs. It holds the
package-agnostic shell so each GUI is a thin module layer rather than a fresh
re-implementation:

- provider and model selection (`provider_registry()`, `shell_sidebar()`)
- environment-variable-only API key handling (`key_state()`, never a paste, never
  a printed value)
- a deterministic offline demo runner and the live runner seam (`demo_runner()`,
  `build_runner()`)
- session cost accounting (`cost_empty()`, `cost_tile()`)
- error-to-banner mapping, auth-aware (`safe_llmr_call()`, `llmr_error_banner()`)
- CSV upload and column mapping (`read_csv_upload()`, `map_columns()`)
- a display layer over the shared `diagnostics()` / `report()` generics
  (`report_text()`, `diagnostics_table()`)
- the standard reactive context every GUI server builds (`shell_context()`)

## For GUI authors

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
`can_run()`, `set_plan()`, and `add_usage()`. Update the substrate once and every
GUI inherits the change.

## Install

```r
remotes::install_github("asanaei/LLMR")
remotes::install_github("asanaei/LLMR.shiny")
```
