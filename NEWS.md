# LLMR.shiny 0.1.2

* `build_runner("live")` returns a callable runner, and `report_text()`
  propagates the report generic's output.
* Demo results carry durable provenance (`annotate_demo_result()`, now
  exported; `is_demo_result()`), and usage tiles read the renamed
  `usage_*` helpers.
* `is_auth_error()` and `llmr_error_banner()` are internal; the public error
  boundary is `safe_llmr_call()` with `condition_category()`.
* Removed counterfeit demo configs from `build_llm_config()`; a demo run
  never fabricates an `llm_config`.

# LLMR.shiny 0.1.1

Initial CRAN release.

* A shared Shiny substrate for the LLMR-family GUIs: provider and model
  selection, the standard sidebar, and the shared reactive context.
* Environment-variable-only API-key handling; no key is ever pasted or printed.
* A deterministic offline demo runner, a live-runner seam, and session usage
  accounting with auth-aware error banners.
* CSV upload with column mapping, and a dataset-agnostic persona selector
  module (optional `DT` dependency).
