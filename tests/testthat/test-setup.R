box::use(
  ../../app/logic/setup,
  ../../app/views/section_labels,
  ../../app/views/section_scale,
  ../../app/views/section_compare
)

runtime_id <- getFromNamespace("runtime_mount_id", "shinyblocks")

test_that("shared colour pickers edit the data mapping and ignore stale example events", {
  r <- shiny::reactiveValues(input = list(group = NULL),
                             cluster_data = data.frame(name = c("A", "B")))
  shiny::testServer(setup$server_group_setup, args = list(r = r), {
    updates <- list()
    root <- session$rootScope()
    root$sendInputMessage <- function(id, message) updates[[id]] <<- message
    session$flushReact()
    expect_equal(updates[[runtime_id("select", root$ns("legend-groupColumn"))]]$selected, "__none__")
    expect_match(output$legendHint$html, "Choose a column to show a legend")
    session$setInputs(choice = list(column = "name"))
    expect_equal(r$input$group, "name")
    expect_equal(updates[[runtime_id("select", root$ns("data-group"))]]$selected, "name")
    expect_equal(updates[[runtime_id("select", root$ns("colors-groupColumn"))]]$selected, "name")
    expect_false(updates[[runtime_id("select", root$ns("colors-groupColumn"))]]$notify)
    session$setInputs(choice = list(column = "unknown"))
    expect_equal(r$input$group, "name")
    r$pending <- list(token = 1)
    session$setInputs(choice = list(column = "__none__"))
    expect_equal(r$input$group, "name")
    r$pending <- NULL
    session$setInputs(choice = list(column = "__none__"))
    expect_null(r$input$group)
    r$cluster_data <- data.frame(other = "C")
    session$flushReact()
    expect_equal(updates[[runtime_id("select", root$ns("legend-groupColumn"))]]$selected, "__none__")
  })
})

test_that("missing and blank label columns explain setup and do not draw labels", {
  r <- shiny::reactiveValues(input = list(), cluster_data = data.frame(name = c("A", "B")))
  shiny::testServer(section_labels$server_labels, args = list(r = r), {
    session$setInputs(show = TRUE, label = "__none__")
    expect_false(session$returned()$args$show)
    expect_match(output$setup$html, "Choose a column")
    session$setInputs(label = "name")
    expect_true(session$returned()$args$show)
    session$setInputs(show = FALSE)
    expect_match(output$setup$html, "Gene labels are hidden")
    r$cluster_data <- data.frame(name = c(" ", NA_character_))
    session$flushReact()
    session$setInputs(show = TRUE)
    expect_false(session$returned()$args$show)
    expect_match(output$setup$html, "This column has no label text")
  })
})

test_that("hidden scale features offer actions using the existing switches", {
  shiny::testServer(section_scale$server_scale, {
    updates <- list()
    root <- session$rootScope()
    root$sendInputMessage <- function(id, message) updates[[id]] <<- message
    session$setInputs(show = FALSE, bar_show = FALSE)
    expect_match(output$axisSetup$html, "position axis is hidden")
    expect_match(output$barSetup$html, "scale bar is hidden")
    session$setInputs(enableAxis = 1, enableBar = 1)
    expect_true(updates[[runtime_id("switch", session$ns("show"))]]$checked)
    expect_true(updates[[runtime_id("switch", session$ns("bar_show"))]]$checked)
  })
})

test_that("tooltip and link setup failures appear beside their controls", {
  r <- shiny::reactiveValues(input = list(cluster = "cluster"),
                             cluster_data = data.frame(cluster = c("one", "two"), gene = c("A", "A")),
                             link_data = NULL)
  shiny::testServer(section_compare$server_tooltip, args = list(r = r), {
    session$setInputs(show = TRUE, columns = character(), formatter = "")
    expect_match(output$setup$html, "Choose what to show")
    session$setInputs(columns = "gene")
    expect_match(session$returned()$args$formatter, "gene")
  })
  shiny::testServer(section_compare$server_links, args = list(r = r), {
    session$setInputs(show = TRUE, source = "alignment", measure = "identity")
    expect_match(output$setup$html, "Upload an alignment CSV")
    r$link_data <- data.frame(cluster1 = "one", cluster2 = "two", start1 = 1, end1 = 2,
                              start2 = 1, end2 = 2)
    session$flushReact()
    expect_match(output$setup$html, "Use strand only")
    session$setInputs(measure = "none")
    expect_true(session$returned()$ready)
  })
})
