## Submission

Initial submission of LLMR.shiny, the shared Shiny substrate that the LLMR
family of GUI packages build on.

LLMR is in Suggests, not Imports: the demo runner, key-state tiles, CSV
mapping, and usage accounting work without it. Live runner calls and live
configuration construction require LLMR and report that requirement when it is
unavailable. DT remains an optional dependency. The test suite passes without
forced suggested packages (_R_CHECK_FORCE_SUGGESTS_=false).

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
