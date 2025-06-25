#' Cluster title controls module
#'
#' The `view_clusterLabel_controls` module provides the UI and server components
#' for controlling the GC cluster label in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, checkboxInput, numericInput, selectInput, icon, div, textInput],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)

#' Shiny UI for GC cluster title controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing cluster title control inputs
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
          label   = "Show cluster title",
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
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("clusterLabelX"),
          label   = "Title X-position",
          value   = 0,
          step    = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("clusterLabelY"),
          label   = "Title Y-position",
          value   = 5,
          step    = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("clusterLabelPosition"),
          label   = "Position",
          choices = c("left", "right"),
          selected = "left"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("clusterLabelFontSize"),
          label   = "Font size (px)",
          value   = 12,
          min     = 1,
          step    = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("clusterLabelFontStyle"),
          label   = "Font style",
          choices = c("normal", "italic", "oblique"),
          selected = "normal"
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("clusterLabelFontWeight"),
          label   = "Font weight",
          choices = c("normal", "bold", "lighter", "bolder"),
          selected = "bold"
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("clusterLabelFontFamily"),
          label   = "Font family",
          choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("clusterLabelColor"),
          label = "Title color",
          value = "black"
        )
    )
  )
}

#' Shiny server for GC cluster title controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_clusterLabel <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    r$clusterLabel <- reactive({
      list(
        showClusterLabel = input$showClusterLabel,
        clusterLabel = parse_delimited_input(input$clusterLabel),
        clusterLabelX = input$clusterLabelX,
        clusterLabelY = input$clusterLabelY,
        clusterLabelPosition = input$clusterLabelPosition,
        clusterLabelFontSize = paste0(input$clusterLabelFontSize, "px"),
        clusterLabelFontStyle = input$clusterLabelFontStyle,
        clusterLabelFontWeight = input$clusterLabelFontWeight,
        clusterLabelFontFamily = input$clusterLabelFontFamily,
        clusterLabelColor = input$clusterLabelColor
      )
    })
  })
}
