box::use(
  ../../app/logic/setup,
  ../../app/views/section_labels,
  ../../app/views/section_scale,
  ../../app/views/section_compare
)

runtime_id <- getFromNamespace("runtime_mount_id", "shinyblocks")

test_that("shared colour pickers edit the data mapping and ignore stale example events", {
  r <- shiny::reactiveValues(cluster_data = data.frame(name = c("A", "B")))
  mapping <- shiny::reactiveVal(list(group = NULL))
  shiny::testServer(setup$server_group_setup, args = list(r = r, mapping = mapping), {
    updates <- list()
    root <- session$rootScope()
    root$sendInputMessage <- function(id, message) updates[[id]] <<- message
    session$flushReact()
    expect_equal(updates[[runtime_id("select", root$ns("legend-groupColumn"))]]$selected, "__none__")
    expect_match(output$legendHint$html, "Choose a column to show a legend")
    data_group <- runtime_id("select", root$ns("data-group"))
    session$setInputs(choice = list(column = "name"))
    expect_equal(updates[[data_group]]$selected, "name")
    # The data mapping follows when the Data picker sends its new value
    mapping(list(group = "name"))
    session$flushReact()
    expect_equal(updates[[runtime_id("select", root$ns("colors-groupColumn"))]]$selected, "name")
    expect_false(updates[[runtime_id("select", root$ns("colors-groupColumn"))]]$notify)
    updates <- list()
    session$setInputs(choice = list(column = "unknown"))
    expect_null(updates[[data_group]])
    r$pending <- list(token = 1)
    session$setInputs(choice = list(column = "__none__"))
    expect_null(updates[[data_group]])
    r$pending <- NULL
    session$setInputs(choice = list(column = "__none__"))
    expect_equal(updates[[data_group]]$selected, "__none__")
    r$cluster_data <- data.frame(other = "C")
    session$flushReact()
    expect_equal(updates[[runtime_id("select", root$ns("legend-groupColumn"))]]$selected, "__none__")
  })
})

test_that("missing and blank label columns explain setup and do not draw labels", {
  r <- shiny::reactiveValues(cluster_data = data.frame(name = c("A", "B")))
  shiny::testServer(section_labels$server_labels, args = list(r = r), {
    session$setInputs(show = TRUE, label = "__none__")
    expect_false(session$returned()$args$show)
    expect_match(output$setup$html, "Choose a column")
    session$setInputs(label = "name")
    expect_true(session$returned()$args$show)
    session$setInputs(show = FALSE)
    expect_null(output$setup)
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
  r <- shiny::reactiveValues(cluster_data = data.frame(cluster = c("one", "two"), gene = c("A", "A")),
                             link_data = NULL)
  mapping <- shiny::reactive(list(cluster = "cluster"))
  shiny::testServer(section_compare$server_tooltip, args = list(r = r), {
    session$setInputs(show = TRUE, columns = character(), formatter = "")
    expect_match(output$setup$html, "Choose what to show")
    session$setInputs(columns = "gene")
    expect_match(session$returned()$args$formatter, "gene")
  })
  shiny::testServer(section_compare$server_links, args = list(r = r, mapping = mapping), {
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


test_that("choosing label text enables labels without overriding presets or explicit hiding", {
  r <- shiny::reactiveValues(pending = NULL,
    cluster_data = data.frame(name = "A", product = "Enzyme", empty = " "))
  shiny::testServer(section_labels$server_labels, args = list(r = r), {
    updates <- list()
    root <- session$rootScope()
    root$sendInputMessage <- function(id, message) updates[[id]] <<- message
    session$setInputs(show = FALSE, label = "__none__")
    switch_id <- runtime_id("switch", session$ns("show"))
    updates <- list()
    session$setInputs(label = "name")
    expect_true(updates[[switch_id]]$checked)
    updates <- list()
    session$setInputs(show = FALSE)
    expect_null(updates[[switch_id]])
    session$setInputs(label = "empty")
    expect_null(updates[[switch_id]])
    r$pending <- list(token = 1)
    session$setInputs(label = "product")
    expect_null(updates[[switch_id]])
    r$pending <- NULL
    session$flushReact()
    expect_null(updates[[switch_id]])
    session$setInputs(label = "name")
    expect_true(updates[[switch_id]]$checked)
  })
})


test_that("gene-colored links omit inactive color settings and restore them in custom mode", {
  r <- shiny::reactiveValues(
    cluster_data = data.frame(cluster = c("A", "B"), gene = c("g", "g")),
    link_data = data.frame(cluster1 = "A", start1 = 1, end1 = 10,
                           cluster2 = "B", start2 = 1, end2 = 10))
  shiny::testServer(section_compare$server_links,
                    args = list(r = r, mapping = shiny::reactive(list(cluster = "cluster"))), {
    session$setInputs(source = "matching", show = TRUE, group = "gene", measure = "none",
      useGroupColors = TRUE, normalColor = "red", invertedColor = "blue",
      vmin = "10", vmax = "90", colorBar = "show")
    fields <- c("normal_color", "inverted_color", "vmin", "vmax", "color_bar")
    expect_true(session$returned()$style$use_group_colors)
    expect_false(any(fields %in% names(session$returned()$style)))
    session$setInputs(useGroupColors = FALSE)
    expect_equal(session$returned()$style[fields], list(normal_color = "red", inverted_color = "blue",
      vmin = 10, vmax = 90, color_bar = FALSE))
    expect_null(session$returned()$style$low_color)
    session$setInputs(useGroupColors = TRUE, source = "alignment")
    expect_false(session$returned()$style$use_group_colors)
    expect_equal(session$returned()$style$normal_color, "red")
  })
})


test_that("the color bar is available only for a selected measure with finite values", {
  r <- shiny::reactiveValues(
    cluster_data = data.frame(gene = c("A", "A"), identity = c(95, 99), similarity = c(NA, NA)),
    link_data = data.frame(identity = c(NA, NaN)))
  shiny::testServer(section_compare$server_links,
                    args = list(r = r, mapping = shiny::reactive(list(cluster = "cluster"))), {
    session$setInputs(source = "matching", group = "gene", measure = "identity", colorBar = "show")
    expect_identical(output$colorBarAvailable, "true")
    expect_true(session$returned()$style$color_bar)
    session$setInputs(measure = "similarity")
    expect_identical(output$colorBarAvailable, "false")
    expect_false(session$returned()$style$color_bar)
    session$setInputs(measure = "none")
    expect_identical(output$colorBarAvailable, "false")
    session$setInputs(source = "alignment", measure = "identity")
    expect_identical(output$colorBarAvailable, "false")
    r$link_data <- data.frame(identity = 98)
    session$flushReact()
    expect_identical(output$colorBarAvailable, "true")
    r$link_data <- data.frame(other = 1)
    session$flushReact()
    expect_identical(output$colorBarAvailable, "false")
  })
})
