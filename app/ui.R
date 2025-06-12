library(shiny)
library(shinydashboard)
library(geneviewer)

box::use(
  ./views/view_gene_controls[ui_genes],
  ./views/view_label_controls[ui_labels],
  ./views/view_scale_controls[ui_scale],
  ./views/view_scaleBar_controls[ui_scaleBar]
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
      ui_scale("scaleControls"),
      ui_scaleBar("scaleBarControls")
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

