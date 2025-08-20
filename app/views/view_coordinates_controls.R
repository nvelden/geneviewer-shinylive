#' Coordinates controls module
#'
#' The \code{view_coordinates_controls} module provides the UI and server components
#' for toggling and formatting the GC_coordinates layer in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, checkboxInput, selectInput,
        textInput, numericInput, icon, div, observeEvent, actionLink],
  shinydashboard[menuItem]
)

#' Shiny UI for GC coordinates controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing coordinates control inputs
#' @export
ui_coordinates <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Coordinates",
    icon    = icon("crosshairs"),
    tabName = "coordinates",
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showCoordinates"),
          label = "Show coordinates",
          value = FALSE
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("ticksFormat"),
          label   = "Tick format",
          choices = c(
            "Comma separated"   = ",.0f",
            "Integer"           = "0",
            "SI (0 decimals)"   = ".0s",
            "SI (2 decimals)"   = ".2s"
          ),
          selected = ""
        )
    ),
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("coordinatesStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
        )
    ),
    ## extra space at the bottom
    div(style="height:20px;")
  )
}

#' Shiny server for GC coordinates controls
#'
#' @param id Namespace id for this module
#' @export
server_coordinates <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    # Handle styling button click
    observeEvent(input$coordinatesStyling, {
      r$coordinates$Styling <- input$coordinatesStyling
    })

    reactive({
      list(
        showCoordinates = input$showCoordinates,
        ticksFormat = input$ticksFormat
      )
    })
  })
}
