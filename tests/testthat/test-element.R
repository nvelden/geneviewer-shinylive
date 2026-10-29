box::use(../../app/views/section_element, ../../app/logic/code_gen)

test_that("element edits apply right away, reset, and replay as code", {
  data_key <- shiny::reactiveVal("data-1")
  current <- shiny::reactiveVal("element")
  opened <- character()
  shiny::testServer(section_element$server_element,
    args = list(data_key = data_key, open_section = function(s, tab = NULL) opened <<- c(opened, s),
                section = current), {
      session$flushReact()
      editor <- session$returned
      event <- list(component = "genes", cluster = "A", data = list(rowID = 1, name = "same"))
      expect_true(editor$open(event))
      expect_equal(opened, "element")
      session$flushReact()
      session$setInputs(token = "1", fill = "red", marker = "box", strokeWidth = "2")
      expect_equal(editor$edits()[[1]]$style, list(fill = "red", marker = "box", strokeWidth = 2))
      # Opening the same element again keeps its fields
      editor$open(event)
      expect_equal(editor$target()$token, "1")

      # Inputs left over from the previous element cannot change this one
      editor$open(list(component = "genes", cluster = "A", data = list(rowID = 2)))
      session$flushReact()
      session$setInputs(fill = "green")
      expect_length(editor$edits(), 1)
      session$setInputs(token = "2", fill = "blue", marker = "__none__", strokeWidth = "")
      expect_equal(editor$edits()[[2]]$style, list(fill = "blue"))
      session$setInputs(markerHeight = "40", arrowheadWidth = "12", cornerRadius = "3",
                        strokeDasharray = "6 3", y = "70")
      expect_equal(editor$edits()[[2]]$style,
                   list(fill = "blue", markerHeight = 40, cornerRadius = 3, arrowheadWidth = 12,
                        strokeDasharray = "6 3", y = 70))

      edits <- editor$edits()
      chart <- geneviewer::GC_chart(data.frame(start = c(1, 30), end = c(20, 50)))
      calls <- list(list(name = "GC_chart", args = list()))
      edited <- chart
      for (edit in edits) {
        args <- c(list(component = edit$component), edit$id, edit$style)
        calls <- c(calls, list(list(name = "GC_element_style", args = args)))
        edited <- do.call(geneviewer::GC_element_style, c(list(edited), args))
      }
      code <- code_gen$chart_code(calls, "data <- data.frame(start = c(1, 30), end = c(20, 50))")
      expect_match(code, "GC_element_style")
      replay <- withr::with_package("geneviewer", eval(parse(text = code), new.env()))
      expect_identical(replay$x$series[[1]]$genes$itemStyle, edited$x$series[[1]]$genes$itemStyle)

      session$setInputs(reset = 1)
      expect_length(editor$edits(), 1)
      expect_equal(editor$target()$token, "3")
      # One switch, above the settings of the element and of all genes
      session$flushReact()
      expect_match(as.character(output$scope$html), '(?s)aria-pressed="true".*Row 2.*aria-pressed="false"', perl = TRUE)
      session$setInputs(scope = "all")
      expect_equal(tail(opened, 1), "genes")
      current("genes")
      session$flushReact()
      expect_match(as.character(output$scope$html), "All genes")
      current("labels")
      session$flushReact()
      expect_null(output$scope)
      session$setInputs(scope = "element")
      expect_equal(tail(opened, 1), "element")

      editor$clear()
      session$setInputs(resetAll = 1)
      expect_length(editor$edits(), 0)

      editor$open(event)
      session$setInputs(token = "4", fill = "red")
      data_key("data-2")
      session$flushReact()
      expect_length(editor$edits(), 0)
      expect_null(editor$target())
      expect_false(editor$open(list(component = "legend", data = "A")))
  })
})

test_that("labels can be renamed, hidden and resized", {
  data_key <- shiny::reactiveVal("data")
  shiny::testServer(section_element$server_element,
    args = list(data_key = data_key, open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      editor$open(list(component = "labels", cluster = "A", data = list(rowID = 2)))
      session$setInputs(token = "1", text = "New", fontSize = "16", fontFamily = "serif", rotate = "3")
      expect_equal(editor$edits()[[1]]$style,
                   list(text = "New", fontSize = "16px", fontFamily = "serif", rotate = 3))
      session$setInputs(hide = TRUE)
      expect_identical(editor$edits()[[1]]$style$text, "")
  })
})

test_that("empty fields show how the element is drawn", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      editor$open(list(component = "genes", cluster = "A", data = list(rowID = 1, name = "g"),
                       style = list(fill = "#1f77b4", stroke = "#000000", marker = "boxarrow",
                                    markerHeight = 15, strokeDasharray = "6px, 3px")))
      session$flushReact()
      html <- paste(as.character(output$fields$html), collapse = "")
      expect_match(html, "#1f77b4", fixed = TRUE)
      expect_match(html, '"state":{"value":"boxarrow"}', fixed = TRUE)
      expect_match(html, '"state":{"value":"6 3"}', fixed = TRUE)
      # A field with its own value takes the chart's setting when emptied
      session$setInputs(token = "1", fill = "red")
      editor$clear()
      editor$open(list(component = "genes", cluster = "A", data = list(rowID = 1, name = "g"),
                       style = list(fill = "#ff0000")))
      session$flushReact()
      html <- paste(as.character(output$fields$html), collapse = "")
      expect_false(grepl("#ff0000", html, fixed = TRUE))
  })
})

test_that("titles, subtitles and coordinates are edited and replayed one by one", {
  opened <- list()
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"),
                open_section = function(s, tab = NULL) opened[[length(opened) + 1]] <<- c(s, tab)), {
      session$flushReact()
      editor <- session$returned
      expect_true(editor$open(list(component = "clusterLabel", cluster = "B", part = "subtitle",
                                   data = NULL, style = list(text = "Species", fontSize = 11))))
      session$flushReact()
      expect_match(paste(as.character(output$fields$html), collapse = ""),
                   "Cluster label subtitle", fixed = TRUE)
      session$setInputs(token = "1", text = "Renamed", fontStyle = "italic")
      session$setInputs(scope = "all")
      expect_equal(opened[[length(opened)]], c("titles", "label"))
      expect_true(editor$open(list(component = "coordinates", cluster = "A",
                                   data = list(rowID = 3, end = "end", gene = list(name = "g3")),
                                   style = list(text = "4,276"))))
      session$flushReact()
      # Fresh fields: the mock session keeps the inputs of the previous element
      session$setInputs(token = "2", hide = TRUE, fill = "red", text = "", fontStyle = "__none__")
      expect_false(editor$open(list(component = "clusterTitle", cluster = "A", data = NULL)))
      edits <- editor$edits()
      expect_equal(edits[[1]]$id, list(cluster = "B", part = "subtitle"))
      expect_equal(edits[[1]]$style, list(text = "Renamed", fontStyle = "italic"))
      expect_equal(edits[[2]]$id, list(row_id = 3, end = "end"))
      expect_equal(edits[[2]]$style, list(show = FALSE, fill = "red"))

      chart <- geneviewer::GC_chart(data.frame(start = c(1, 30, 60), end = c(20, 50, 80),
                                               cluster = c("A", "B", "A")), cluster = "cluster") %>%
        geneviewer::GC_clusterLabel(subtitle = "Species") %>%
        geneviewer::GC_coordinates()
      for (edit in edits) {
        chart <- do.call(geneviewer::GC_element_style,
                         c(list(chart, component = edit$component), edit$id, edit$style))
      }
      expect_equal(chart$x$series$B$clusterLabel$itemStyle,
                   list(list(text = "Renamed", fontStyle = "italic", part = "subtitle")))
      expect_equal(chart$x$series$A$coordinates$itemStyle,
                   list(list(show = FALSE, fill = "red", rowID = 3, end = "end")))
  })
})

test_that("legend labels and markers edit, reset and replay independently", {
  opened <- character()
  current <- shiny::reactiveVal("element")
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), section = current,
      open_section = function(s, tab = NULL) opened <<- c(opened, s)), {
      session$flushReact()
      editor <- session$returned
      expect_true(editor$open(list(component = "legend", data = "A", part = "label",
                                   style = list(text = "A", fill = "#000000"))))
      session$flushReact()
      expect_match(paste(output$fields$html, collapse = ""), "Legend label", fixed = TRUE)
      session$setInputs(token = "1", text = "Family A", fontSize = "18", fill = "red")
      expect_equal(editor$edits()[[1]]$id, list(part = "label", group = "A"))
      expect_equal(editor$edits()[[1]]$style, list(text = "Family A", fill = "red", fontSize = "18px"))
      session$setInputs(scope = "all")
      expect_equal(tail(opened, 1), "legend")
      current("legend")
      session$flushReact()
      expect_match(paste(output$scope$html, collapse = ""), "All legend labels", fixed = TRUE)
      expect_true(editor$open(list(component = "legend", data = "A", part = "marker",
                                   style = list(fill = "#1f77b4", strokeWidth = 1))))
      session$flushReact()
      expect_match(paste(output$fields$html, collapse = ""), "Legend marker", fixed = TRUE)
      expect_false(grepl('element-fontSize', paste(output$fields$html, collapse = ""), fixed = TRUE))
      session$setInputs(token = "2", fill = "blue", opacity = "0.8", strokeWidth = "2")
      expect_equal(editor$edits()[[2]]$id, list(part = "marker", group = "A"))
      expect_equal(editor$edits()[[2]]$style, list(fill = "blue", opacity = 0.8, strokeWidth = 2))
      expect_match(paste(output$scope$html, collapse = ""), "All legend markers", fixed = TRUE)
      chart <- geneviewer::GC_chart(data.frame(start = c(1, 30), end = c(20, 50), group = c("A", "B")), group = "group")
      calls <- list(list(name = "GC_chart", args = list(group = "group")))
      for (edit in editor$edits()) {
        args <- c(list(component = edit$component), edit$id, edit$style)
        chart <- do.call(geneviewer::GC_element_style, c(list(chart), args))
        calls <- c(calls, list(list(name = "GC_element_style", args = args)))
      }
      code <- code_gen$chart_code(calls, 'data <- data.frame(start = c(1, 30), end = c(20, 50), group = c("A", "B"))')
      replay <- withr::with_package("geneviewer", eval(parse(text = code), new.env()))
      expect_equal(replay$x$legend$itemStyle, chart$x$legend$itemStyle)
      editor$open(list(component = "legend", data = "B", part = "marker"))
      session$flushReact()
      session$setInputs(token = "3", fill = "green", opacity = "", strokeWidth = "")
      expect_length(editor$edits(), 3)
      session$setInputs(reset = 1)
      expect_length(editor$edits(), 2)
      expect_false(editor$selectable(list(component = "legend", data = "A", part = "title")))
      expect_false(editor$selectable(list(component = "legend", part = "label")))
  })
})

test_that("selected text size is a value without creating an inherited override", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      editor$open(list(component = "legend", data = "A", part = "label",
                       style = list(text = "A", fontSize = 12)))
      session$flushReact()
      html <- paste(output$fields$html, collapse = "")
      expect_match(html, 'value="12"', fixed = TRUE)
      session$setInputs(token = "1", fontSize = "12", fill = "red")
      expect_equal(editor$edits()[[1]]$style, list(fill = "red"))
      session$setInputs(fontSize = "13")
      expect_equal(editor$edits()[[1]]$style$fontSize, "13px")
      session$setInputs(fontSize = "")
      expect_null(editor$edits()[[1]]$style$fontSize)
  })
})

test_that("all reported values populate fields without pinning unchanged styles", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      editor$open(list(component = "genes", cluster = "A", data = list(rowID = 1),
        style = list(fill = "#1f77b4", opacity = 1, marker = "arrow", markerSize = "medium",
                     markerHeight = 15, stroke = "#000000", strokeWidth = 1, strokeDasharray = "none", y = 50)))
      session$flushReact()
      session$setInputs(token = "1", fill = "#1f77b4", opacity = "1", marker = "arrow",
        markerSize = "medium", markerHeight = "15", stroke = "#000000", strokeWidth = "1",
        strokeDasharray = "none", y = "50")
      expect_length(editor$edits(), 0)
      session$setInputs(strokeWidth = "2")
      expect_equal(editor$edits()[[1]]$style, list(strokeWidth = 2))
      editor$open(list(component = "labels", cluster = "A", data = list(rowID = 1),
        style = list(text = "Gene A", fill = "#000000", fontSize = 12, fontWeight = "400",
                     fontStyle = "normal", fontFamily = '"Times New Roman", serif', dx = 3, dy = 4, rotate = 10)))
      session$flushReact()
      session$setInputs(token = "2", text = "Gene A", fill = "#000000", fontSize = "12",
        fontWeight = "normal", fontStyle = "normal", fontFamily = "Times New Roman",
        rotate = "10")
      expect_length(editor$edits(), 1)
      session$setInputs(text = "Renamed")
      expect_equal(editor$edits()[[2]]$style, list(text = "Renamed"))
      session$setInputs(hide = TRUE)
      expect_identical(editor$edits()[[2]]$style$text, "")
  })
})

test_that("dragging text preserves styles and clearing the position removes only drag offsets", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      event <- list(component = "title", part = "title", style = list(text = "Chart", moveX = 0, moveY = 0))
      expect_true(editor$open(event))
      session$flushReact()
      session$setInputs(token = "1", fill = "red", fontStyle = "italic")
      event$position <- list(moveX = 25, moveY = -10)
      expect_true(editor$move(event))
      expect_equal(editor$edits()[[1]]$id, list(part = "title"))
      expect_equal(editor$edits()[[1]]$style,
        list(fill = "red", fontStyle = "italic", moveX = 25, moveY = -10))
      # Clearing both fields keeps the other styling
      session$setInputs(token = editor$target()$token, moveX = "", moveY = "")
      expect_equal(editor$edits()[[1]]$style, list(fill = "red", fontStyle = "italic"))
      event$component <- "genes"
      event$data <- list(rowID = 1)
      expect_false(editor$move(event))
      event$component <- "title"
      event$position$moveX <- Inf
      expect_false(editor$move(event))
      event$component <- "labels"
      event$cluster <- "A"
      event$part <- NULL
      event$position <- list(moveX = 10, moveY = 5)
      expect_true(editor$move(event))
      expect_equal(tail(editor$edits(), 1)[[1]]$id, list(row_id = 1))
  })
})

test_that("elements moved together by the chart keep their other styles", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      styles <- function() lapply(editor$edits(), `[[`, "style")
      title <- list(component = "title", part = "title", style = list(text = "Chart", moveX = 0, moveY = 0))
      editor$open(title)
      session$flushReact()
      session$setInputs(token = "1", fill = "red")
      # Whole pixels arrive as integers; the rebuilt fields send them back as doubles
      title$position <- list(moveX = 25L, moveY = -10L)
      editor$move(title)
      session$flushReact()
      session$setInputs(token = editor$target()$token, moveX = "25", moveY = "-10")
      expect_identical(styles(), list(list(fill = "red", moveX = 25, moveY = -10)))
      label <- list(component = "labels", cluster = "A", data = list(rowID = 1),
                    style = list(moveX = 0, moveY = 0), position = list(moveX = 5, moveY = 5))
      editor$move(label)
      session$flushReact()
      # The chart's reset: elements back where the chart puts them keep no offset
      title$position <- list(moveX = 0L, moveY = 0L)
      label$position <- list(moveX = 0, moveY = 0)
      editor$place(list(title, label, list(component = "genes", data = list(rowID = 1),
                                           position = list(moveX = 1, moveY = 1))))
      expect_equal(styles(), list(list(fill = "red")))
      expect_equal(editor$target()$drawn$moveX, 0)
      # Undoing it puts the offsets back
      label$position <- list(moveX = 5, moveY = 5)
      editor$place(list(label))
      expect_equal(styles()[[2]], list(moveX = 5, moveY = 5))
  })
})

test_that("redraw refreshes the same selected element after resetting its styles", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      event <- list(component = "labels", cluster = "A", data = list(rowID = 1),
                    style = list(text = "Gene", fill = "#000000", moveX = 0, moveY = 0))
      editor$open(event)
      session$flushReact()
      session$setInputs(token = "1", fill = "red")
      event$position <- list(moveX = 25, moveY = 10)
      editor$move(event)
      session$flushReact()
      session$setInputs(reset = 1)
      expect_length(editor$edits(), 0)
      # The widget reports the selected element's current style after redraw.
      editor$open(event)
      session$flushReact()
      expect_equal(editor$target()$drawn, event$style)
      html <- paste(output$fields$html, collapse = "")
      expect_match(html, "#000000", fixed = TRUE)
      expect_equal(editor$target()$drawn$moveX, 0)
      token <- editor$target()$token
      editor$open(event)
      expect_equal(editor$target()$token, token)
      # Refreshed inherited values must not become new overrides.
      session$setInputs(token = token, fill = "#000000", text = "Gene", moveX = "0", moveY = "0")
      expect_length(editor$edits(), 0)
      # A redraw of an explicit edit keeps the fields and current section.
      session$setInputs(fill = "red")
      edited <- event
      edited$style$fill <- "#ff0000"
      editor$open(edited, navigate = FALSE)
      expect_equal(editor$target()$token, token)
  })
})


for (component in c("scale", "scaleBar")) test_that(paste(component, "moves as a whole and replays its cluster position"), {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      event <- list(component = component, cluster = "A", style = list(moveX = 0, moveY = 0),
                    position = list(moveX = 35, moveY = 20))
      expect_true(editor$move(event))
      expect_equal(editor$edits()[[1]]$id, list(cluster = "A"))
      expect_equal(editor$edits()[[1]]$style, event$position)
      edit <- editor$edits()[[1]]
      chart <- geneviewer::GC_chart(data.frame(start = c(1, 30), end = c(20, 50), cluster = c("A", "B")),
                                  cluster = "cluster")
      chart <- do.call(geneviewer::GC_element_style, c(list(chart, component = edit$component), edit$id, edit$style))
      expect_equal(chart$x$series$A[[component]]$itemStyle, list(event$position))
      expect_null(chart$x$series$B[[component]]$itemStyle)
      session$flushReact()
      session$setInputs(token = editor$target()$token, moveX = "", moveY = "")
      expect_length(editor$edits(), 0)
    })
})

test_that("legend labels and markers keep their own drag offsets", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      marker <- list(component = "legend", data = "A", part = "marker",
                     style = list(fill = "#1f77b4", moveX = 0, moveY = 0), position = list(moveX = 12L, moveY = -4L))
      expect_true(editor$move(marker))
      label <- list(component = "legend", data = "A", part = "label",
                    style = list(text = "A", moveX = 0, moveY = 0), position = list(moveX = 3, moveY = 8))
      expect_true(editor$move(label))
      edits <- editor$edits()
      expect_equal(lapply(edits, `[[`, "id"), list(list(part = "marker", group = "A"), list(part = "label", group = "A")))
      expect_identical(lapply(edits, `[[`, "style"), list(list(moveX = 12, moveY = -4), list(moveX = 3, moveY = 8)))
      chart <- geneviewer::GC_chart(data.frame(start = 1, end = 20, group = "A"), group = "group")
      for (edit in edits) chart <- do.call(geneviewer::GC_element_style, c(list(chart, component = "legend"), edit$id, edit$style))
      expect_length(chart$x$legend$itemStyle, 2)
  })
})

test_that("several selected elements share a position and reset together", {
  shiny::testServer(section_element$server_element,
    args = list(data_key = shiny::reactiveVal("data"), open_section = function(s, tab = NULL) NULL), {
      session$flushReact()
      editor <- session$returned
      label <- function(row, x, y) list(component = "labels", cluster = "A", data = list(rowID = row),
                                        style = list(text = "Gene", moveX = x, moveY = y))
      gene <- list(component = "genes", cluster = "A", data = list(rowID = 3), style = list(fill = "red"))
      # Down differs, Right is shared; the gene cannot move
      editor$move(c(label(1, 0, 0), list(position = list(moveX = 5, moveY = 2))))
      editor$move(c(label(2, 0, 0), list(position = list(moveX = 5, moveY = 9))))
      expect_true(editor$open_group(list(label(1, 5, 2), label(2, 5, 9), gene)))
      session$flushReact()
      html <- as.character(output$fields$html)
      expect_match(html, "3 elements")
      expect_match(html, '"placeholder":"Mixed"', fixed = TRUE)
      token <- regmatches(html, regexpr('(?<=groupToken" type="text" class="shiny-input-text form-control" value=")[0-9]+', html, perl = TRUE))
      # The fields' first values change nothing
      # An empty number field sends NA
      session$setInputs(groupToken = token, groupMoveX = 5, groupMoveY = NA_real_)
      expect_equal(lapply(editor$edits(), `[[`, "style"), list(list(moveX = 5, moveY = 2), list(moveX = 5, moveY = 9)))
      # Right for all of them; Down stays as it was for each
      session$setInputs(groupMoveX = "12")
      expect_equal(lapply(editor$edits(), `[[`, "style"), list(list(moveX = 12, moveY = 2), list(moveX = 12, moveY = 9)))
      expect_length(editor$edits(), 2)
      session$setInputs(groupReset = 1)
      expect_length(editor$edits(), 0)
      # One element is not a group
      expect_false(editor$open_group(list(label(1, 0, 0))))
      session$flushReact()
      expect_no_match(as.character(output$fields$html), "elements")
  })
})
