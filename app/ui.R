library(shiny)
library(shinydashboard)
library(geneviewer)

box::use(
  ./views/view_gene_controls[ui_genes],
  ./views/view_label_controls[ui_labels]
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Gene Cluster Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      ui_genes("geneControls"),
      ui_labels("labelControls")
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

