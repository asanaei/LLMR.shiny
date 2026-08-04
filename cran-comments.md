## Resubmission

This resubmits the 0.1.2 declined in late July: CRAN asked that the update
wait about a week, because 0.1.1 had been published on 2026-07-21 only days
before. The code is unchanged from that submission; the only difference is
the order of the NEWS bullets. Three new packages under
submission (FocusGroup, LLMRcontent, LLMRpanel) declare LLMR.shiny
(>= 0.1.2) in Suggests and their GUIs call objects that 0.1.2 exports, so
this update precedes them.

## The update

0.1.2 corrects the 0.1.1 release: `build_runner("live")` returns a callable
function, demonstration results carry explicit source fields, usage helpers
use token vocabulary, and two error-display internals left the exported
surface while the demo annotator joined it (downstream family packages call
it across the namespace boundary).

LLMR is in Suggests, not Imports: demonstration execution, key-state tiles, CSV
mapping, and usage accounting work without it. Live execution and live
configuration construction require LLMR and report that requirement when it is
unavailable. DT remains an optional dependency. The test suite passes without
forced suggested packages (_R_CHECK_FORCE_SUGGESTS_=false).

## Test environments

- local macOS (Darwin 25.5.0), R 4.4.3
- R CMD check --as-cran on the built tarball, with NOT_CRAN=false
  and _R_CHECK_FORCE_SUGGESTS_=false

## R CMD check results

0 errors | 0 warnings | 2 notes

- "checking for future file timestamps ... NOTE: unable to verify current
  time": environmental (the check machine could not reach a time server); it
  does not reproduce on CRAN's builders.
- "checking HTML version of manual ... NOTE": emitted by an older
  system `tidy` that does not recognize the HTML5 elements R generates;
  it does not reproduce on CRAN.

## Reverse dependencies

None on CRAN. The author's GUI packages (FocusGroup, LLMRcontent, LLMRpanel)
Suggest this package and are submitted after it.
