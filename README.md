# LLMR.shiny <img src="man/figures/logo.png" align="right" width="120" alt="LLMR.shiny icon" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMR.shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMR.shiny/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMR.shiny/)
<!-- badges: end -->

The shared Shiny substrate for the LLMR family of GUIs. It defines the shell
each GUI builds on, so a new interface supplies only its package-specific module
code:

- provider and model selection (`provider_registry()`, `shell_sidebar()`)
- environment-variable-only API key handling (`key_state()`, never a paste, never
  a printed value)
- a deterministic offline demo runner and a live runner (`demo_runner()`,
  `build_runner()`)
- session cost accounting (`cost_empty()`, `cost_tile()`)
- authentication-sensitive error banners (`safe_llmr_call()`, `llmr_error_banner()`)
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
`can_run()`, `set_plan()`, and `add_usage()`. A change here is available to any
GUI that imports the shared shell.

## Install

```r
remotes::install_github("asanaei/LLMR")
remotes::install_github("asanaei/LLMR.shiny")
```
