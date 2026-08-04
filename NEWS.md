# LLMR.shiny 0.1.2

* `build_runner("live")` returns a callable function, and `report_text()`
  propagates the report generic's output.
* Removed placeholder model configurations in demonstration mode from
  `build_llm_config()`; a demonstration run never fabricates an `llm_config`.
* Validation messages in studio tables now remain in normal document flow, so
  later controls and plots do not overlap them.
* Added `text_block_output()` and defensive theme rules for long text results
  inside fillable layouts.
* Added live generation settings for temperature, maximum output tokens, and
  reasoning effort, with unset model parameters omitted from
  `build_llm_config()`.
* Added active default models, Gemini support, and model-entry guidance to the
  shared sidebar.
* Added `guess_column()` for name-based text, label, and identifier mapping
  defaults with exclusion and positional fallback.
* Added accessible `help_tip()` icons for compact, inline guidance.
* Persona selectors now report the live selected-row count and explain the
  seeded-sample behavior when no rows are selected.
* `as_display_table()` now rounds ordinary double columns to three significant
  digits for display, with a `digits` override, while preserving integer,
  identifier, and nonnumeric columns.
* Demonstration results carry explicit source fields (`annotate_demo_result()`,
  now exported; `is_demo_result()`), and usage tiles read the renamed
  `usage_*` helpers.
* `is_auth_error()` and `llmr_error_banner()` are internal; the exported
  error-handling functions are `safe_llmr_call()` and `condition_category()`.

# LLMR.shiny 0.1.1

Initial CRAN release.

* Shared Shiny components for the LLMR-family GUIs: provider and model
  selection, the standard sidebar, and shared reactive values and server
  helpers.
* Environment-variable-only API-key handling; no key is ever pasted or printed.
* Reproducible offline demonstration responses marked as demonstrations, a
  replaceable function for live execution, and session usage accounting with
  authentication-aware error banners.
* CSV upload with column mapping, and a dataset-agnostic persona selector
  module (optional `DT` dependency).
