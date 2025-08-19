#' Gene label controls module
#'
#' The \code{view_gene_label_controls} module provides the UI and server components
#' for toggling and styling gene labels in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[
    NS, moduleServer, observe, updateSelectInput, reactive,
    checkboxInput, numericInput, selectInput, icon, req,
    textInput, div, actionLink, observeEvent
  ],
  shinydashboard[menuItem]
)

#' Shiny UI for gene label controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing label control inputs
#' @export
ui_labels <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Gene labels",
    icon    = icon("tags"),
    tabName = "labels",
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showLabels"),
          label   = "Show labels",
          value   = FALSE
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("labelGroup"),
          label   = "Group",
          choices = NULL
        )
    ),
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("labelStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
        )
    ),
    ## extra space at the bottom
    div(style="height:20px;")
  )
}

#' Shiny server for gene label controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list containing cluster_data and inputs
#' @export
server_labels <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(r$cluster_data)
      updateSelectInput(
        session, "labelGroup",
        choices  = names(r$cluster_data),
        selected = names(r$cluster_data)[1]
      )
    })

    observeEvent(input$labelStyling, {
      r$label$Styling <- input$labelStyling
    })

    reactive({
      list(
        showLabels  = input$showLabels,
        labelGroup = input$labelGroup
      )
    })
  })
}
