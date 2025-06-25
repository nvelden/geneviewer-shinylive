#' Graph dimensions controls module
#'
#' The \code{view_dimensions_controls} module provides the UI and server components
#' for controlling the height and width of graphs in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, numericInput, selectInput, checkboxInput, icon, div],
  shinydashboard[menuItem]
)

#' Shiny UI for graph dimensions controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing dimension control inputs
#' @export
ui_dimensions <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Dimensions",
    icon    = icon("expand-arrows-alt"),
    tabName = "dimensions",
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("width"),
          label   = "Width (px)",
          value   = NULL,
          min     = 100,
          max     = 2000,
          step    = 50
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("height"),
          label   = "Height (px)",
          value   = 400,
          min     = 100,
          max     = 3000,
          step    = 50
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("marginTop"),
          label   = "Top margin (px)",
          value   = 5,
          min     = 0,
          max     = 500,
          step    = 5
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("marginBottom"),
          label   = "Bottom margin (px)",
          value   = 5,
          min     = 0,
          max     = 500,
          step    = 5
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("marginLeft"),
          label   = "Left margin (px)",
          value   = 50,
          min     = 0,
          max     = 500,
          step    = 5
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("marginRight"),
          label   = "Right margin (px)",
          value   = 50,
          min     = 0,
          max     = 500,
          step    = 5
        )
    ),
    ## extra space at bottom
    div(style="height:20px;")
  )
}

#' Shiny server for graph dimensions controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_dimensions <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    reactive({

      w <- if (is.null(input$width) || is.na(input$width)) {
        "100%"
      } else {
        paste0(input$width, "px")
      }
      h <- if (is.null(input$height) || is.na(input$height)) {
        "400px"
      } else {
        paste0(input$height, "px")
      }

      list(
        width = w,
        height = h,
        margin = list(
          top = input$marginTop,
          bottom = input$marginBottom,
          left = input$marginLeft,
          right = input$marginRight
        )
      )
    })
  })
}
