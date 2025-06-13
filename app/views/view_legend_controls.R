#' Legend controls module
#'
#' The \code{view_legend_controls} module provides the UI and server components
#' for controlling the GC legend in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, updateSelectInput, textInput,
        checkboxInput, numericInput, icon, div, observe, req, sliderInput],
  shinydashboard[menuItem]
)

#' Shiny UI for GC legend controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing legend control inputs
#' @export
ui_legend <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Legend",
    icon    = icon("list"),
    tabName = "legend",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showLegend"),
          label   = "Show legend",
          value   = TRUE
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("legendPosition"),
          label   = "Position",
          choices = c("top", "bottom"),
          selected = "bottom"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("legendX"),
          label = "X-position",
          value = 0,
          step  = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("legendY"),
          label = "Y-position",
          value = 0,
          step  = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        sliderInput(
          inputId = ns("legendFontSize"),
          label = "Font size (px)",
          min   = 6,
          max   = 32,
          value = 12,
          step  = 1
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("legendFontFamily"),
          label   = "Font family",
          choices = c("sans-serif", "serif", "monospace", "cursive", "fantasy"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("legendFill"),
          label = "Text color",
          value = "black"
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC legend controls
#'
#' @param id Namespace id for this module
#' @export
server_legend <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {

    reactive({
      list(
        showLegend = input$showLegend,
        legendPosition = input$legendPosition,
        legendX = input$legendX,
        legendY = input$legendY,
        legendFontSize = input$legendFontSize,
        legendFontFamily = input$legendFontFamily,
        legendFill = input$legendFill
      )
    })

  })
}
