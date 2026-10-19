## The update

0.1.3 changes how the shared Studio components read uploaded files and count
usage. `read_csv_upload()` and `read_csv_path()` gain `col_classes` and
`na_strings`, so a Studio can keep identifiers such as `001` and the text "NA"
as written; the defaults reproduce the earlier reading. Token usage that a
reply did not report is now counted as unknown rather than as zero:
`extract_token_counts()` and `usage_add()` return and accumulate the number of
replies with unknown usage, token subtotals are doubles, and the usage tile
shows lower bounds. Column mapping refuses duplicate or empty column names.
DESCRIPTION now requires shiny (>= 1.5.0) and bslib (>= 0.5.1), the versions
that introduced `moduleServer()` and `tooltip()`, which the package calls.
Exported names are unchanged; each CSV reader gains two arguments with
defaults.

LLMR stays in Suggests: demonstration execution, CSV mapping, and usage
accounting work without it, and live execution reports the requirement when
LLMR is unavailable.

## Test environments

- local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3: R CMD check
  --as-cran --no-manual on the built tarball, with
  _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 1 note

- "checking for future file timestamps ... NOTE: unable to verify current
  time": the check machine could not reach a time server.

## Reverse dependencies

Two CRAN packages suggest LLMR.shiny: FocusGroup 0.5.2 and LLMRpanel 0.6.1.
Their CRAN tarballs were checked with R CMD check --no-manual
--ignore-vignettes against this build of LLMR.shiny 0.1.3 and LLMR 0.8.11
from CRAN. Both returned Status: OK.
