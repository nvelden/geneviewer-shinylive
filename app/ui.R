library(shiny)
library(shinydashboard)
library(geneviewer)

box::use(
  ./views/view_gene_controls[ui_genes],
  ./views/view_label_controls[ui_labels],
  ./views/view_scale_controls[ui_scale],
  ./views/view_scaleBar_controls[ui_scaleBar],
  ./views/view_clusterTitle_controls[ui_clusterTitle],
  ./views/view_clusterLabel_controls[ui_clusterLabel],
  ./views/view_legend_controls[ui_legend],
  ./views/view_color_controls[ui_color],
  ./views/view_sequence_controls[ui_sequence],
  ./views/view_coordinates_controls[ui_coordinates],
  ./views/view_gene_alignment_controls[ui_align],
  ./views/view_geneLinks_controls[ui_links]
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Gene Cluster Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      ui_genes("geneControls"),
      ui_labels("labelControls"),
      ui_align("alignmentControls"),
      ui_legend("legendControls"),
      ui_scale("scaleControls"),
      ui_scaleBar("scaleBarControls"),
      ui_clusterTitle("clusterTitleControls"),
      ui_clusterLabel("clusterLabelControls"),
      ui_color("colorControls"),
      ui_sequence("sequenceControls"),
      ui_coordinates("coordinatesControls"),
      ui_links("linksControls")
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title       = "Gene Cluster Chart",
        status      = "primary",
        solidHeader = TRUE,
        width       = 12,
        GC_chartOutput("gcChart", width="100%", height="400px")
      )
    )
  )
)

