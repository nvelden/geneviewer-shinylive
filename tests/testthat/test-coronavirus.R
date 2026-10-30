box::use(
  ../../app/logic/examples,
  ../../app/views/section_compare,
  ../../app/views/section_genes
)

test_that("the coronavirus example has its genomes, tracks and labels", {
  example <- examples$EXAMPLES$coronavirus
  genes <- example$data()
  expect_length(unique(genes$cluster), 11)
  # ORFs inside another gene go on the tracks below, by GC_cluster()
  inputs <- example$inputs(genes)
  expect_identical(inputs[["genes-overlap"]], "nested")
  expect_identical(inputs[["genes-styleBy"]], "geneTrack")
  expect_match(genes$label_subtitle[genes$cluster == "SARS-CoV-2"][1], "<i>Sarbecovirus</i>, human")
  # One genus title per genus, above its first genome
  titles <- trimws(strsplit(inputs[["titles-title_title"]], ",")[[1]])
  expect_length(titles, 11)
  expect_setequal(titles[nzchar(titles)], unique(genes$genus))
})

test_that("link labels can go on the links and the color bar in the legend", {
  r <- shiny::reactiveValues(cluster_data = geneviewer::coronavirus_genomes, link_data = NULL)
  mapping <- shiny::reactive(list(cluster = "cluster"))
  shiny::testServer(section_compare$server_links, args = list(r = r, mapping = mapping), {
    session$setInputs(show = TRUE, source = "matching", group = "homolog",
                      measure = "identity", label = TRUE)
    value <- session$returned()
    expect_true(value$ready)
    # By default the measures go with the gene labels and the bar bottom right
    expect_null(value$args$label_position)
    expect_length(value$style$colorBarOptions, 0)
    session$setInputs(labelPosition = "link", labelSize = 9, colorBar = "show",
                      colorBarPosition = "legend", colorBarLegendSide = "right",
                      colorBarOrientation = "horizontal")
    value <- session$returned()
    expect_equal(value$args$label_position, "link")
    expect_equal(value$style$labelStyle, list(fontSize = "9px"))
    # In the legend the bar lies down: the orientation is not passed
    expect_equal(value$style$colorBarOptions, list(position = "legend", legendSide = "right"))
    session$setInputs(colorBarPosition = "topleft")
    expect_equal(session$returned()$style$colorBarOptions,
                 list(position = "topleft", orientation = "horizontal"))
    # In a corner: the measures below a lying bar, moved in, without a title
    session$setInputs(colorBarLabels = "below", colorBarX = 10, colorBarY = 30,
                      colorBarTitle = FALSE)
    expect_equal(session$returned()$style$colorBarOptions,
                 list(position = "topleft", orientation = "horizontal", labels = "below",
                      x = 10, y = 30, title = FALSE))
    # A standing bar has its measures beside it; the legend takes no nudge
    session$setInputs(colorBarOrientation = "vertical")
    expect_null(session$returned()$style$colorBarOptions$labels)
    session$setInputs(colorBarPosition = "legend", colorBarTitle = TRUE)
    expect_equal(session$returned()$style$colorBarOptions,
                 list(position = "legend", legendSide = "right"))
    # A hidden bar takes no placement
    session$setInputs(colorBar = "hide")
    expect_null(session$returned()$style$colorBarOptions)
    # Without labels, there is no label position or size
    session$setInputs(label = FALSE)
    expect_null(session$returned()$args$label_position)
    expect_length(session$returned()$style$labelStyle, 0)
  })
})

test_that("overlapping genes go on separate tracks from the Genes section", {
  shiny::testServer(section_genes$server_genes, {
    session$setInputs(overlap = "off")
    expect_null(session$returned()$cluster)
    session$setInputs(overlap = "nested", overlapSpacing = -4, overlapMin = "")
    expect_equal(session$returned()$cluster,
                 list(prevent_gene_overlap = "nested", overlap_spacing = -4))
    session$setInputs(overlap = "all", overlapMin = "10%")
    expect_equal(session$returned()$cluster$overlap_min, "10%")
    expect_true(session$returned()$cluster$prevent_gene_overlap)
    session$setInputs(overlapMin = "6")
    expect_equal(session$returned()$cluster$overlap_min, 6)
  })
})
