#' Cluster title controls module
#'
#' The \code{view_clusterTitle_controls} module provides the UI and server components
#' for controlling the GC cluster title in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, textInput, numericInput, selectInput, checkboxInput, icon, div],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)

#' Shiny UI for GC cluster title controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing cluster title control inputs
#' @export
ui_clusterTitle <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Cluster title",
    icon    = icon("heading"),
    tabName = "clustertitle",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showTitle"),
          label   = "Show cluster title",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("title"),
          label = "Title text",
          placeholder = "Title 1, Title 2, ..."
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("subtitle"),
          label = "Subtitle text",
          placeholder = "Sub 1, Sub 2, ...",
          value = ""
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("height"),
          label = "Title height",
          value = "40px"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("titleX"),
          label = "Title X-position",
          value = 0,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("titleY"),
          label = "Title Y-position",
          value = 5,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("titleAlign"),
          label = "Title alignment",
          choices = c("left", "center", "right"),
          selected = "center"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("titleSpacing"),
          label = "Title spacing",
          value = 20,
          min = 0,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("titleFontSize"),
          label = "Title font size (px)",
          value = 16,
          min = 8,
          max = 48,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("titleFontWeight"),
          label = "Title font weight",
          choices = c("normal", "bold", "lighter", "bolder"),
          selected = "bold"
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("titleFontFamily"),
          label = "Title font family",
          choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("titleColor"),
          label = "Title color",
          value = "black"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("subtitleFontSize"),
          label = "Subtitle font size (px)",
          value = 14,
          min = 8,
          max = 48,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("subtitleFontWeight"),
          label = "Subtitle font weight",
          choices = c("normal", "bold", "lighter", "bolder"),
          selected = "normal"
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("subtitleFontFamily"),
          label = "Subtitle font family",
          choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("subtitleColor"),
          label = "Subtitle color",
          value = "black"
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC cluster title controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_clusterTitle <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        show = input$showTitle,
        title = parse_delimited_input(input$title),
        subtitle = parse_delimited_input(input$subtitle),
        height = input$height,
        x = input$titleX,
        y = input$titleY,
        align = input$titleAlign,
        spacing = input$titleSpacing,
        titleFont = list(
          fontSize = paste0(input$titleFontSize, "px"),
          fontWeight = input$titleFontWeight,
          fontFamily = input$titleFontFamily,
          fill = input$titleColor,
          fontStyle = "normal",
          textDecoration = "normal",
          cursor = "default"
        ),
        subtitleFont = list(
          fontSize = paste0(input$subtitleFontSize, "px"),
          fontWeight = input$subtitleFontWeight,
          fontFamily = input$subtitleFontFamily,
          fill = input$subtitleColor,
          fontStyle = "normal",
          textDecoration = "normal",
          cursor = "default"
        )
      )
    })
  })
}
