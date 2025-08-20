#' Cluster label controls module
#'
#' The `view_clusterLabel_controls` module provides the UI and server components
#' for controlling the GC cluster label in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, checkboxInput, numericInput, selectInput, icon, div, textInput,
        observeEvent, actionLink],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)

#' Shiny UI for GC cluster label controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing cluster label control inputs
#' @export
ui_clusterLabel <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Cluster label",
    icon    = icon("heading"),
    tabName = "clusterLabel",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showClusterLabel"),
          label   = "Show cluster label",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("clusterLabel"),
          label = "Label text",
          placeholder = "Label 1, Label 2, ..."
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

#' Shiny server for GC cluster label controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_clusterLabel <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    # Handle styling button click
    observeEvent(input$labelStyling, {
      r$clusterLabel$Styling <- input$labelStyling
    })

    reactive({
      list(
        show = input$showClusterLabel,
        text = parse_delimited_input(input$clusterLabel)
      )
    })
  })
}
