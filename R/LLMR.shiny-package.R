# LLMR.shiny-package.R ----------------------------------------------------------
# Package-level imports. Functions are fully qualified at call sites
# (shiny::, bslib::); these importFrom tags cover the few bare uses and keep
# R CMD check quiet about utils/stats. DT is a downstream GUI concern, not used
# in this substrate.

#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils capture.output head read.csv str
"_PACKAGE"
