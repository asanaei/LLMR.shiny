# LLMR.shiny 0.1.3

* `read_csv_upload()` and `read_csv_path()` gain `col_classes` and
  `na_strings`, passed to `utils::read.csv()` as `colClasses` and
  `na.strings`. The defaults keep the earlier reading, which infers each
  column's type: `001` and `01` both become 1, long integer-like identifiers
  lose digits, `T` and `F` become logical, and the text "NA" becomes missing.
  With `col_classes = "character"` and `na_strings = character(0)` every field
  is kept as written.
* `validate_column_mapping()`, and with it `map_columns()`, stop with "The
  data have duplicate column names" when a column name is repeated and with
  "Column(s) ... have no name" when a name is empty or missing. Before, a
  repeated `text` heading was mapped from its first copy and the second copy
  kept the same name. A label selection that is `NA` or names more than one
  column stops with "Select one label column, or none."; before, `NA` was
  reported as a column not in the data and a longer selection raised a base R
  error.
* Token usage that a reply did not report is unknown, not zero.
  `extract_token_counts()` sums the counts that were reported and returns
  three new integer counts, `unknown`, `sent_unknown`, and `received_unknown`.
  A subtotal is `NA` when no reply reported it and at least one reply is
  unknown. Before, absent or all-`NA` token columns gave 0. `usage_empty()`
  gains the same three fields, set to zero, and `usage_add()` accumulates
  them. `usage_tile()` shows a subtotal with unreported replies as "at least"
  the reported part, or as "unknown", and adds a line with the number of such
  replies; before, it showed unreported usage as 0. Records made by 0.1.2 lack
  the three fields and are read as having none unknown.
* The token subtotals `sent`, `received`, and `total` in the results of
  `usage_empty()`, `usage_add()`, and `extract_token_counts()` are doubles.
  They were integers, which overflow above 2,147,483,647. Code that tests them
  with `is.integer()` or compares them with `identical(x, 0L)` sees the change.
* `usage_add()` computes an absent `total` as `sent` plus `received` when both
  are known and no reply is unknown. Before, an absent total added nothing. An
  explicit `NA` total stays unknown.
* Given a list, `extract_token_counts()` chooses the call table by its
  columns. Frames with call identifiers and token columns rank first, then
  frames with identifiers, then frames with token columns. Before, it took the
  first data frame, so a summary table placed first gave one result row and 0
  tokens. When several frames share the best rank, the first is used with the
  warning "Several call frames have the same rank: ...; using ...".
* `extract_token_counts()` counts calls from `call_id`, `request_id`, or
  `response_id` only when every successful row has an identifier, and counts a
  failed row without one as one call. Before, it counted the distinct
  identifiers present, so a table missing half its identifiers reported half
  its calls. When identifiers are incomplete, it counts result rows instead
  and warns, for example, "2 of 4 rows have no `response_id`, so calls cannot
  be counted; counting result rows instead."
* Missing usage on failed calls counts as unknown, since a failed call can
  still be billed; before, it counted as 0. Rows with `run_mode = "demo"` add
  result rows and no tokens; before, the stub token counts of a demonstration
  result were added to the session's tokens.
* A token column that is not numeric, such as a factor or character column,
  is read as unknown with the warning "Token column `sent_tokens` is factor,
  not numeric; its counts are treated as unknown." Before, a factor column
  added its level codes and a character column added the numbers it could be
  coerced to. This is a warning rather than an error because usage is counted
  after a Studio run has finished, and an error at that point would end the
  Studio session. Negative, fractional, and non-finite values in a token
  column are also unknown, with the warning "Token column `sent_tokens`
  contains invalid counts; they are treated as unknown."; before, they were
  added as given. In `usage_add()`, an invalid token value is unknown with the
  warning "`tokens$sent` must be one finite nonnegative whole number; it is
  treated as unknown."; before, it was coerced with `as.integer()`.
* An `NA` call or result-row count given to `usage_add()`, or
  `fallback_calls = NA` given to `extract_token_counts()`, is kept as unknown
  with the warning "`tokens$calls` is NA; the count is recorded as unknown.",
  and `usage_tile()` shows "Unknown number of calls". Before, it made the
  record's count `NA`, shown as "NA calls". Any other count that is not one
  finite nonnegative whole number within the integer range stops with, for
  example, "`tokens$calls` must be one nonnegative whole number." This applies
  to `calls`, `result_rows`, `unknown`, `sent_unknown`, and
  `received_unknown` in `usage_add()` and to `fallback_calls`. Before, a
  fractional count was truncated, and a negative or character count was added
  as given.
* The package requires shiny (>= 1.5.0), which introduced `moduleServer()`,
  used by `persona_selector_server()`, and bslib (>= 0.5.1), which introduced
  `tooltip()`, used by `help_tip()` and `shell_sidebar()`. Before, earlier
  versions installed without complaint and failed when those functions ran.

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
