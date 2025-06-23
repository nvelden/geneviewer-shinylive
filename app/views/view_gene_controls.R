# view_gene_controls.R

#' Gene controls module
#'
#' The \code{view_gene_controls} module provides the UI and server components
#' for selecting gene marker style and size in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, observe, selectInput, div, icon, reactive, updateSelectInput, req],
  shinydashboard[menuItem]
)

#' Shiny UI for gene controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard box containing marker and size inputs
#' @export
ui_genes <- function(id) {
  ns <- NS(id)
  menuItem(
    text = "Genes",
    icon = icon("sliders-h"),
    tabName = "genes",
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId  = ns("geneGroup"),
          label = "Group",
          choices = NULL
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId  = ns("marker"),
          label    = "Marker",
          choices  = c("arrow","boxarrow","box","cbox","rbox"),
          selected = "arrow"
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId  = ns("markerSize"),
          label    = "Size",
          choices  = c("small","medium","large"),
          selected = "medium"
        )
    ),
    # extra space at the bottom of the menuItem
    div(style="height:20px;")
  )
}

#' Shiny server for gene controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_genes <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(r$cluster_data)
      updateSelectInput(
        session,
        "geneGroup",
        choices = names(r$cluster_data),
        selected = names(r$cluster_data)[1]
      )
    })

    reactive(list(
      geneGroup = input$geneGroup,
      marker      = input$marker,
      markerSize = input$markerSize
    ))
  })
}
