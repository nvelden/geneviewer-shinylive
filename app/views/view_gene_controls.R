# view_gene_controls.R

#' Gene controls module
#'
#' The \code{view_gene_controls} module provides the UI and server components
#' for selecting gene marker style and size in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, observe, selectInput, div, icon, reactive, actionLink,
        observeEvent, updateSelectInput, req],
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
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("geneStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
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

    observeEvent(input$geneStyling, {
      r$gene$Styling <- input$geneStyling
    })

    reactive(list(
      marker      = input$marker,
      markerSize = input$markerSize
    ))
  })
}
