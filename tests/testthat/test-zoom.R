box::use(
  ../../app/views/section_zoom,
  ../../app/logic/project
)

test_that("the Zoom section gives the GC_zoom() options", {
  shiny::testServer(section_zoom$server_zoom, {
    session$setInputs(show = FALSE)
    expect_false(session$returned()$show)
    session$setInputs(show = TRUE, mode = "absolute", update = "__none__",
                      overview = FALSE, height = 30)
    zoom <- session$returned()
    expect_true(zoom$show)
    expect_identical(Filter(Negate(is.null), zoom$args),
                     list(mode = "absolute", overview = FALSE, height = 30))
  })
})

test_that("a project keeps the zoom window, and leaves out a damaged one", {
  data <- data.frame(cluster = "A", start = 1, end = 10)
  file <- tempfile(fileext = ".json")
  writeLines(project$write_project(data, list(), zoom = list(mode = "relative", range = c(0.25, 0.5))), file)
  expect_identical(project$read_project(file)$zoom, list(mode = "relative", range = c(0.25, 0.5)))

  writeLines(project$write_project(data, list()), file)
  expect_null(project$read_project(file)$zoom)

  saved <- jsonlite::fromJSON(project$write_project(data, list()), simplifyVector = FALSE)
  for (zoom in list(list(mode = "relative", range = list(0.5, 2)),
                    list(mode = "sideways", range = list(1, 2)),
                    list(mode = "absolute", range = list(10)))) {
    saved$zoom <- zoom
    writeLines(jsonlite::toJSON(saved, auto_unbox = TRUE, null = "null"), file)
    expect_null(project$read_project(file)$zoom)
  }
})
