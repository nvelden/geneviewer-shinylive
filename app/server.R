library(shiny)

box::use(
  ./views/view_gene_controls[server_genes],
  ./views/view_label_controls[server_labels]
)

# Define server logic required to draw a histogram
function(input, output, session) {

  r <- reactiveValues(
    input = list(),
    cluster_data  = ophA_clusters  # <-- store your data here
  )

  # inputs
  gene_inputs <- server_genes("geneControls", r = r)
  label_inputs <- server_labels("labelControls", r = r)

  output$gcChart <- renderGC_chart({

    req(gene_inputs()$geneGroup)
    req(label_inputs()$labelGroup)

    GC_chart(
      r$cluster_data,
      cluster = "cluster",
      group = "class"
    ) %>%
      GC_genes(
        group = gene_inputs()$geneGroup,
        marker = gene_inputs()$marker,
        markerSize = gene_inputs()$markerSize,
        colorScheme = gene_inputs()$colorScheme,
        customColors = NULL
      ) %>%
      GC_labels(
        label = label_inputs()$labelGroup,
        show = label_inputs()$showLabels,
        fontSize = label_inputs()$fontSize,
        fontStyle = label_inputs()$fontStyle,
        fontFamily = label_inputs()$fontFamily,
        fontWeight = label_inputs()$fontWeight,
        fill = label_inputs()$fill
      ) %>%
      GC_clusterTitle(title = c("<i>O. olearius</i>", "<i>D. bispora</i>")) %>%
      GC_legend(position = "bottom") %>%
      GC_scaleBar() %>%
      GC_clusterLabel(title = "ophA")

  })

}
