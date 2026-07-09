# persona_selector.R --------------------------------------------------------
# A dataset-agnostic Shiny module for choosing rows of a persona data frame. It
# shows a compact, scrollable table (ordered as the data is ordered) and lets the
# user toggle rows; the server returns the chosen row indices into the supplied
# data. It knows nothing about focus groups or panels -- the caller decides what
# the selected rows become. Uses DT, which is an optional dependency.

#' UI for the persona selector module
#'
#' Renders the selectable persona table. Pair with [persona_selector_server()].
#' The table's height is set by the `height` argument of
#' [persona_selector_server()], which controls the scrollable body.
#'
#' @param id Module id.
#' @return A Shiny UI element (a `DT` output).
#' @seealso [persona_selector_server()].
#' @export
persona_selector_ui <- function(id) {
  ns <- shiny::NS(id)
  if (!pkg_available("DT")) {
    return(shiny::tags$div(class = "text-muted",
      "Install the 'DT' package to browse and select personas."))
  }
  shiny::tagList(DT::DTOutput(ns("table")))
}

#' Server for the persona selector module
#'
#' Renders a compact overview of `data` as a multi-select table and returns the
#' selected row indices (relative to `data`) as a reactive. When `data` carries
#' the persona contract (see [LLMR::llm_persona_overview()]), the overview columns
#' are chosen automatically; otherwise the first few columns are shown.
#'
#' @param id Module id.
#' @param data A persona data frame (or a `reactive` returning one).
#' @param overview Optional overview data frame, or a function `function(df)`
#'   building one. Defaults to [LLMR::llm_persona_overview()] when LLMR is
#'   available, else the first columns of `data`.
#' @param page_length Rows per page in the table. Default `8`.
#' @param height CSS height for the scrollable table body. Default `"260px"`.
#' @return A `reactive` returning an integer vector of selected row indices into
#'   `data` (`integer(0)` when nothing is selected). When `DT` is not installed
#'   the module renders nothing and the reactive is always `integer(0)`,
#'   matching the install guidance shown by [persona_selector_ui()].
#' @seealso [persona_selector_ui()].
#' @export
persona_selector_server <- function(id, data, overview = NULL,
                                    page_length = 8L, height = "260px") {
  shiny::moduleServer(id, function(input, output, session) {
    if (!pkg_available("DT")) {
      return(shiny::reactive(integer(0)))
    }
    data_r <- if (shiny::is.reactive(data)) data else shiny::reactive(data)

    overview_r <- shiny::reactive({
      d <- data_r()
      if (is.null(d)) return(NULL)
      if (is.function(overview)) return(overview(d))
      if (!is.null(overview)) return(overview)
      if (requireNamespace("LLMR", quietly = TRUE)) {
        ov <- try(LLMR::llm_persona_overview(d), silent = TRUE)
        if (!inherits(ov, "try-error")) return(ov)
      }
      as.data.frame(d[, seq_len(min(6L, ncol(d))), drop = FALSE])
    })

    output$table <- DT::renderDT({
      ov <- overview_r(); shiny::req(ov)
      # round any numeric columns for a tidy display
      num <- vapply(ov, is.numeric, logical(1))
      if (any(num)) ov[num] <- lapply(ov[num], function(v) round(v, 2))
      DT::datatable(
        ov, selection = "multiple", rownames = TRUE,
        options = list(pageLength = page_length, scrollX = TRUE,
                       scrollY = height, lengthChange = FALSE,
                       searchHighlight = TRUE))
    }, server = TRUE)

    shiny::reactive({
      sel <- input$table_rows_selected
      if (is.null(sel)) integer(0) else as.integer(sel)
    })
  })
}
