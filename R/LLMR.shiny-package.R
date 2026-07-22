# LLMR.shiny-package.R ----------------------------------------------------------
# Package-level imports. Functions are fully qualified at call sites
# (shiny::, bslib::); these importFrom tags cover the few bare uses and keep
# R CMD check quiet about utils/stats. DT is an optional GUI dependency and is
# not used by the core components.

#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils capture.output head read.csv str
"_PACKAGE"
