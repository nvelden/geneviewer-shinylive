#' Coordinates controls module
#'
#' The \code{view_coordinates_controls} module provides the UI and server components
#' for toggling and formatting the GC_coordinates layer in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, checkboxInput, selectInput,
        textInput, numericInput, icon, div],
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
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("yPositionTop"),
          label   = "Y position top",
          value   = 55,
          min     = 0
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("yPositionBottom"),
          label   = "Y position bottom",
          value   = 45,
          min     = 0
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("tickStrokeWidth"),
          label   = "Tick stroke width",
          value   = 1,
          min     = 0
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("tickLineLength"),
          label   = "Tick line length",
          value   = 5,
          min     = 0
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("tickStroke"),
          label   = "Tick color",
          value   = "black"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("textFill"),
          label   = "Text color",
          value   = "black"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("fontSize"),
          label   = "Font size",
          value   = 12,
          min     = 0
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("fontFamily"),
          label   = "Font family",
          choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
          selected = "sans-serif"
          )
    ),
    div(style="height:20px;")
  )
}

#' Shiny server for GC coordinates controls
#'
#' @param id Namespace id for this module
#' @export
server_coordinates <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        showCoordinates = input$showCoordinates,
        ticksFormat = input$ticksFormat,
        yPositionTop = input$yPositionTop,
        yPositionBottom = input$yPositionBottom,
        tickStroke = input$tickStroke,
        tickStrokeWidth = input$tickStrokeWidth,
        tickLineLength = input$tickLineLength,
        textFill = input$textFill,
        fontSize = input$fontSize,
        fontFamily = input$fontFamily
      )
    })
  })
}
