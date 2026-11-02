box::use(../../app/views/section_labels)

test_that("legend availability follows the group mapping without resetting settings", {
  r <- shiny::reactiveValues(cluster_data = data.frame(category = c("A", "B")))
  mapping <- shiny::reactiveVal(list(group = NULL))
  shiny::testServer(section_labels$server_legend, args = list(r = r, mapping = mapping), {
    messages <- list()
    root <- session$rootScope()
    switch_update <- NULL
    root$sendInputMessage <- function(id, message) switch_update <<- message
    root$sendCustomMessage <- function(type, message) {
      messages[[type]] <<- message
    }
    session$setInputs(show = TRUE, fontSize = 18, position = "top")
    expect_false(messages[["gv-legend-availability"]]$available)
    expect_true(switch_update$disabled)
    expect_false(switch_update$checked)
    expect_false(switch_update$notify)
    expect_false(session$returned()$args$show)

    mapping(list(group = "category"))
    session$flushReact()
    expect_true(messages[["gv-legend-availability"]]$available)
    expect_false(switch_update$disabled)
    expect_true(switch_update$checked)
    expect_true(session$returned()$args$show)
    expect_equal(session$returned()$style$legendTextOptions$fontSize, "18px")

    r$cluster_data <- data.frame(other = "C")
    session$flushReact()
    expect_false(messages[["gv-legend-availability"]]$available)
    expect_false(session$returned()$args$show)
    expect_equal(session$returned()$style$position, "top")

    r$cluster_data <- data.frame(category = "A")
    session$flushReact()
    expect_true(messages[["gv-legend-availability"]]$available)
    expect_true(session$returned()$args$show)
    session$setInputs(show = FALSE)
    mapping(list(group = NULL))
    session$flushReact()
    expect_false(messages[["gv-legend-availability"]]$available)
    mapping(list(group = "category"))
    session$flushReact()
    expect_false(session$returned()$args$show)
    r$cluster_data <- data.frame(category = c(NA_character_, " "))
    session$flushReact()
    expect_false(messages[["gv-legend-availability"]]$available)
  })
})

test_that("patterns set on legend markers merge with the pattern settings of Colors", {
  box::use(../../app/logic/utils[with_group_patterns])
  groups <- c("A", "B", "A", "No Hit", "C")
  # Without patterns in Colors: only that group, on its gene colour
  off <- with_group_patterns(list(show = FALSE), groups, list(B = "dots"), list())
  expect_true(off$show)
  expect_equal(off$args$patterns, list(B = "dots"))
  expect_true("background" %in% names(off$args) && is.null(off$args$background))
  # One pattern per group in Colors: the others keep theirs
  on <- with_group_patterns(list(show = TRUE, args = list(background = "white")), groups,
                            list(B = "none"), list(A = "red"))
  expect_equal(on$args$patterns, list(A = "diagonal", C = "crosshatch"))
  expect_equal(on$args$color$A, "red")
  expect_equal(on$args$color$C, "black")
  # No pattern left: no GC_pattern()
  expect_false(with_group_patterns(list(show = FALSE), groups, list(B = "none"), list())$show)
})
