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
    expect_identical(
      output$selection_count,
      paste(
        "0 of 3 selected.",
        "A seeded sample of the requested size will be drawn."
      )
    )
    # a selection comes back as integer indices into `data`
    session$setInputs(table_rows_selected = c(1L, 3L))
    expect_equal(session$returned(), c(1L, 3L))
    expect_identical(output$selection_count, "2 of 3 selected.")
  })
})

test_that("persona_selector_ui places a live count beside the table", {
  skip_if_not_installed("DT")
  html <- paste(as.character(persona_selector_ui("people")), collapse = " ")
  expect_match(html, 'id="people-selection_count"', fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
  expect_lt(
    regexpr("people-selection_count", html, fixed = TRUE),
    regexpr("people-table", html, fixed = TRUE)
  )
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
