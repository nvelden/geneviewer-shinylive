box::use(
  ../../app/logic/examples,
  ../../app/logic/utils,
  ../../app/logic/alignment_data,
  ../../app/logic/code_gen,
  ../../app/logic/ui_helpers,
  ../../app/views/section_compare
)

test_that("the pQEB1 example includes its genes, inversion blocks and registered controls", {
  example <- examples$EXAMPLES$pqeb1
  genes <- example$data()
  links <- alignment_data$validate_alignment(example$links())
  expect_equal(nrow(genes), 401)
  expect_length(unique(genes$cluster), 5)
  expect_equal(nrow(links), 14)
  expect_true(any(links$start2 > links$end2))
  expect_true(all(links$identity >= 99.2))
  expect_true(all(c(links$cluster1, links$cluster2) %in% genes$cluster))
  expect_identical(example$link_code, quote(pQEB1_alignment))
  # Building the actual UI registers all controls, including hidden sections.
  app_dir <- normalizePath("../../app")
  old_dir <- getwd()
  tryCatch({
    setwd(app_dir)
    source("ui.R", local = new.env(parent = globalenv()))
  }, finally = setwd(old_dir))
  for (preset in examples$EXAMPLES) {
    expect_true(all(names(preset$inputs(preset$data())) %in% ls(ui_helpers$INPUT_TYPES)))
  }
})

test_that("host subtitles stay with the visible plasmids and Tn7 strains", {
  data <- geneviewer::pQEB1_plasmids
  order <- c("pQEB1_inv1_arsTn", "pQEB1")
  selected <- utils$order_cluster_data(data, "cluster", order)
  expect_identical(utils$cluster_values(selected, "cluster", "host"),
                   c("K. oxytoca KOX-3", "E. cloacae"))
  tn7 <- examples$EXAMPLES$tn7$data()
  clusters <- rev(unique(tn7$cluster))[1:2]
  selected <- utils$order_cluster_data(tn7, "cluster", clusters)
  expected <- vapply(clusters, function(x) tn7$strain_short[match(x, tn7$cluster)], "")
  expect_equal(utils$cluster_values(selected, "cluster", "strain_short"), unname(expected))
  expect_null(utils$cluster_values(data, "cluster", NULL))
})

test_that("alignment CSV validation preserves inversions and rejects invalid tables", {
  data <- geneviewer::pQEB1_alignment
  expect_equal(alignment_data$validate_alignment(data)$start2, data$start2)
  expect_equal(alignment_data$validate_alignment(data)$end2, data$end2)
  expect_error(alignment_data$validate_alignment(data[, names(data) != "end2"]), "end2")
  expect_error(alignment_data$validate_alignment(data[0, ]), "no blocks")
  bad <- data; bad$start1[1] <- Inf
  expect_error(alignment_data$validate_alignment(bad), "finite")
  bad <- data; bad$end2[1] <- "not a coordinate"
  expect_error(alignment_data$validate_alignment(bad), "finite")
  bad <- data; bad$cluster1[1] <- " "
  expect_error(alignment_data$validate_alignment(bad), "cluster names")
  bad <- data; names(bad)[9] <- "cluster1"
  expect_error(alignment_data$validate_alignment(bad), "duplicate")
})

test_that("generated alignment code is executable and does not embed the data frame", {
  example <- examples$EXAMPLES$pqeb1
  source <- code_gen$data_code(NULL, example_code = example$code, link_code = example$link_code)
  calls <- list(
    list(name = "GC_chart", args = list(cluster = "cluster", group = "category", strand = "strand")),
    list(name = "GC_scale", args = list(axis_type = "range")),
    list(name = "GC_links", args = list(data = quote(alignment), measure = "none", label = FALSE,
                                       color_bar = FALSE, inverted_color = "#e6894e"))
  )
  code <- code_gen$chart_code(calls, source)
  expect_match(code, "alignment <- pQEB1_alignment", fixed = TRUE)
  expect_match(code, "data = alignment", fixed = TRUE)
  expect_false(grepl("structure(list", code, fixed = TRUE))
  chart <- eval(parse(text = code), envir = new.env())
  expect_equal(nrow(chart$x$links[[1]]$data), 14)
  expect_identical(chart$x$links[[1]]$options$measure, "none")
  upload <- code_gen$data_code("genes.csv", link_code = call("read.csv", "a 'quoted' alignment.csv", stringsAsFactors = FALSE))
  expect_silent(parse(text = upload))
  expect_match(upload, "alignment <- read.csv", fixed = TRUE)
})

test_that("link controls separate alignment blocks from matching genes", {
  r <- shiny::reactiveValues(cluster_data = geneviewer::pQEB1_plasmids, link_data = NULL)
  mapping <- shiny::reactive(list(cluster = "cluster"))
  shiny::testServer(section_compare$server_links, args = list(r = r, mapping = mapping), {
    session$setInputs(show = TRUE, source = "alignment", measure = "none", colorBar = "hide")
    expect_false(session$returned()$ready)
    r$link_data <- geneviewer::pQEB1_alignment
    session$flushReact()
    value <- session$returned()
    expect_true(value$ready)
    expect_equal(nrow(value$args$data), 14)
    expect_null(value$args$group)
    expect_false(value$style$color_bar)
    session$setInputs(measure = "similarity")
    expect_false(session$returned()$ready)
    session$setInputs(source = "matching", group = "category", label = TRUE)
    value <- session$returned()
    expect_true(value$ready)
    expect_null(value$args$data)
    expect_equal(value$args$group, "category")
    expect_true(value$args$label)
    session$setInputs(source = "alignment", measure = "none")
    bad <- geneviewer::pQEB1_alignment; bad$cluster1[1] <- "unknown"
    r$link_data <- bad; session$flushReact()
    expect_false(session$returned()$ready)
    expect_match(session$returned()$problem, "unknown")
    file <- tempfile(fileext = ".csv")
    utils::write.csv(geneviewer::pQEB1_alignment, file, row.names = FALSE)
    session$setInputs(alignmentFile = data.frame(name = "blocks.csv", datapath = file))
    expect_true(session$returned()$ready)
    expect_true(is.call(r$link_code))
    session$setInputs(alignmentFile = NULL)
    expect_null(r$link_data)
    expect_null(r$link_code)
    expect_false(session$returned()$ready)
  })
})
