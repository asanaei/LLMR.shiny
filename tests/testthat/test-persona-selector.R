# Tests for the persona selector module. Offline; no live calls.

test_that("persona_selector_server returns the selected row indices", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  d <- data.frame(ideology = c(-1, 0, 1),
                  party = c("D", "I", "R"), stringsAsFactors = FALSE)
  shiny::testServer(persona_selector_server, args = list(id = "p", data = d), {
    # nothing selected -> integer(0)
    session$setInputs(table_rows_selected = NULL)
    expect_equal(session$returned(), integer(0))
    # a selection comes back as integer indices into `data`
    session$setInputs(table_rows_selected = c(1L, 3L))
    expect_equal(session$returned(), c(1L, 3L))
  })
})

test_that("the selector degrades without DT instead of erroring", {
  skip_if_not_installed("shiny")
  local_mocked_bindings(pkg_available = function(package) FALSE)
  ui <- persona_selector_ui("p")
  expect_s3_class(ui, "shiny.tag")
  expect_match(paste(as.character(ui), collapse = " "), "DT")

  d <- data.frame(a = 1:3)
  shiny::testServer(persona_selector_server, args = list(id = "p", data = d), {
    expect_equal(session$returned(), integer(0))
  })
})
