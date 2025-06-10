library(shiny)
library(shinydashboard)
library(geneviewer)

box::use(
  ./views/view_gene_controls[ui_genes]
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Gene Cluster Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      ui_genes("gene_controls")
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

