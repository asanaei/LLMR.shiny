## Submission

Update to 0.1.2, a correction of the 0.1.1 release: `build_runner("live")`
returns a callable function, demonstration results carry explicit source
fields, usage helpers use token vocabulary, and two error-display
internals left the exported surface while the demo annotator joined it
(downstream family packages call it across the namespace boundary).

LLMR is in Suggests, not Imports: demonstration execution, key-state tiles, CSV
mapping, and usage accounting work without it. Live execution and live
configuration construction require LLMR and report that requirement when it is
unavailable. DT remains an optional dependency. The test suite passes without
forced suggested packages (_R_CHECK_FORCE_SUGGESTS_=false).

## Test environments

- local macOS (Darwin 25.5.0), R 4.4.3
- R CMD check --as-cran --no-manual on the built tarball, with NOT_CRAN=false
  and _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 3 notes

- "New submission": expected for a first submission.
- "checking for future file timestamps ... NOTE: unable to verify current
  time": environmental (the check machine could not reach a time server); it
  does not reproduce on CRAN's builders.
- "checking HTML version of manual ... NOTE": emitted by an older
  system `tidy` that does not recognize the HTML5 elements R generates;
  it does not reproduce on CRAN.

## Reverse dependencies

None on CRAN. The author's GUI packages (LLMRcontent, LLMRpanel, FocusGroup)
will depend on this package and are submitted after it.
