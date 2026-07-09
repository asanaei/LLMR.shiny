## Submission

Initial submission of LLMR.shiny, the shared Shiny substrate that the LLMR
family of GUI packages build on.

LLMR is in Suggests, not Imports: the substrate runs fully offline (demo
runner, key-state tiles, CSV mapping, cost accounting) without it, and every
use of LLMR in the code is guarded by requireNamespace("LLMR", quietly = TRUE)
with a documented fallback. The same holds for the other soft dependency, DT.
The test suite passes with and without the suggested packages
(_R_CHECK_FORCE_SUGGESTS_=false).

## Test environments

- local macOS (Darwin 25.5.0), R 4.4.3
- R CMD check --as-cran --no-manual on the built tarball, with NOT_CRAN=false
  and _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 2 notes

- "New submission": expected for a first submission.
- "checking for future file timestamps ... NOTE: unable to verify current
  time": environmental (the check machine could not reach a time server); it
  does not reproduce on CRAN's builders.

## Reverse dependencies

None on CRAN. The author's GUI packages (LLMRcontent, LLMRpanel, FocusGroup)
will depend on this package and are submitted after it.
