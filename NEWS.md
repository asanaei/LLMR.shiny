# LLMR.shiny 0.1.1

Initial CRAN release.

* A shared Shiny substrate for the LLMR-family GUIs: provider and model
  selection, the standard sidebar, and the shared reactive context.
* Environment-variable-only API-key handling; no key is ever pasted or printed.
* A deterministic offline demo runner, a live-runner seam, and session cost
  accounting with auth-aware error banners.
* CSV upload with column mapping, and a dataset-agnostic persona selector
  module (optional `DT` dependency).
