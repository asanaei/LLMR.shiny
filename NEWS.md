# LLMR.shiny 0.1.2

* `build_runner("live")` returns a callable function, and `report_text()`
  propagates the report generic's output.
* Demonstration results carry explicit source fields (`annotate_demo_result()`,
  now exported; `is_demo_result()`), and usage tiles read the renamed
  `usage_*` helpers.
* `is_auth_error()` and `llmr_error_banner()` are internal; the exported
  error-handling functions are `safe_llmr_call()` and `condition_category()`.
* Removed placeholder model configurations in demonstration mode from
  `build_llm_config()`; a demonstration run never fabricates an `llm_config`.

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
