# Graph dimensions controls module
#
# The \code{view_dimensions_controls} module provides the UI and server components
# for controlling the height and width of graphs in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, numericInput, selectInput, checkboxInput, icon, div, fluidRow, column],
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

    # Wrap in a scoped div for compact spacing
    div(class = "compact-inputs",

        # Main dimensions in a compact row
        fluidRow(
          column(6,
                 numericInput(
                   inputId = ns("width"),
                   label   = "Width (px)",
                   value   = NULL,
                   min     = 100,
                   max     = 2000,
                   step    = 50
                 )
          ),
          column(6,
                 numericInput(
                   inputId = ns("height"),
                   label   = "Height (px)",
                   value   = 400,
                   min     = 100,
                   max     = 3000,
                   step    = 50
                 )
          )
        ),

        # Margins section
        div(style = "font-weight: bold; font-size: 12px; margin-bottom: 5px; color: #777;",
            "Margins (px)"
        ),
        fluidRow(
          column(6,
                 numericInput(ns("marginTop"), "Top", 5, min = 0, max = 500, step = 5)
          ),
          column(6,
                 numericInput(ns("marginBottom"), "Bottom", 5, min = 0, max = 500, step = 5)
          )
        ),
        fluidRow(
          column(6,
                 numericInput(ns("marginLeft"), "Left", 50, min = 0, max = 500, step = 5)
          ),
          column(6,
                 numericInput(ns("marginRight"), "Right", 50, min = 0, max = 500, step = 5)
          )
        )
    )
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
