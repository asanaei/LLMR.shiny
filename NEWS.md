# LLMR.shiny 0.1.1

* Added a persona selector module, `persona_selector_ui()` /
  `persona_selector_server()`: a dataset-agnostic, scrollable, multi-select table
  of a persona data frame that returns the chosen row indices. It reads the
  shared persona overview (`LLMR::llm_persona_overview()`) when available. `DT` is
  a new optional dependency (Suggests).

# LLMR.shiny 0.1.0

* First release. A shared Shiny substrate for the LLMR family's optional GUIs:
  the provider and model sidebar, environment-variable key handling, cost
  accounting tiles, an offline demo runner, CSV upload and column mapping, and
  helpers that turn the `LLMR::diagnostics()` and `LLMR::report()` generics into
  display tables. Each downstream GUI builds on it as a thin module layer.
