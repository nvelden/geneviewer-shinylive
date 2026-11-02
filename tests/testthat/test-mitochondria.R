box::use(
  ../../app/logic/examples,
  ../../app/views/section_genes,
  ../../app/views/section_labels
)

test_that("the mitochondrial example starts each genome at COX1 and spreads its labels", {
  example <- examples$EXAMPLES$mitochondria
  genes <- example$data()
  first <- genes[genes$plot_start == 1, ]
  expect_setequal(first$cluster, unique(genes$cluster))
  expect_true(all(first$gene == "COX1"))
  expect_true(all(startsWith(genes$label[genes$strand == "complement"], "-")))
  inputs <- example$inputs(genes)
  expect_identical(inputs[["labels-spread"]], "bottom")
  expect_true(inputs[["genes-separateStrands"]])
})

test_that("the spread setting gives GC_labels() spread labels and their lines", {
  r <- shiny::reactiveValues(cluster_data = geneviewer::mitochondrial_gene_order)
  shiny::testServer(section_labels$server_labels, args = list(r = r), {
    session$setInputs(show = TRUE, label = "gene", spread = "__none__")
    expect_null(session$returned()$style$adjustLabels)
    session$setInputs(spread = "bottom", leaderLength = 12, leaderLine = TRUE)
    style <- session$returned()$style
    expect_identical(style$adjustLabels, "spread")
    expect_null(style$spreadPosition)
    expect_equal(style$leaderLine, list(length = 12))
    session$setInputs(spread = "top", leaderLine = FALSE)
    style <- session$returned()$style
    expect_identical(style$spreadPosition, "top")
    expect_false(style$leaderLine)
  })
})

test_that("separate strands go to GC_cluster() instead of the overlap tracks", {
  shiny::testServer(section_genes$server_genes, {
    session$setInputs(overlap = "nested", separateStrands = TRUE, strandSpacing = -18)
    expect_equal(session$returned()$cluster, list(separate_strands = TRUE, strand_spacing = -18))
    session$setInputs(separateStrands = FALSE)
    expect_identical(session$returned()$cluster$prevent_gene_overlap, "nested")
  })
})
