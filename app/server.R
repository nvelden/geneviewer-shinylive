library(shiny)

box::use(
  ./views/view_gene_controls[server_genes]
)

# Define server logic required to draw a histogram
function(input, output, session) {

  gene_inputs <- server_genes("gene_controls")

  output$gcChart <- renderGC_chart({

    GC_chart(
      ophA_clusters,
      cluster = "cluster",
      group = "class"
    ) %>%
      GC_genes(
        marker = gene_inputs()$marker,
        marker_size = gene_inputs()$marker_size
      ) %>%
      GC_clusterTitle(title = c("<i>O. olearius</i>", "<i>D. bispora</i>")) %>%
      GC_labels("name") %>%
      GC_legend(position = "bottom") %>%
      GC_scaleBar() %>%
      GC_clusterLabel(title = "ophA")

  })

}
