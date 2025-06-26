#' Cluster footer controls module
#'
#' The \code{view_clusterFooter_controls} module provides the UI and server components
#' for controlling the GC cluster footer in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, textInput, numericInput, selectInput, checkboxInput, icon, div],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)

#' Shiny UI for GC cluster footer controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing cluster footer control inputs
#' @export
ui_clusterFooter <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Cluster footer",
    icon    = icon("heading"),
    tabName = "clusterfooter",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showFooter"),
          label   = "Show cluster footer",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("footer"),
          label = "Footer text",
          placeholder = "Footer 1, Footer 2, ..."
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("footerSubtitle"),
          label = "Footer subtitle",
          placeholder = "Sub 1, Sub 2, ...",
          value = ""
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("height"),
          label = "Footer height",
          value = "40px"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("x"),
          label = "X-position",
          value = 0,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("y"),
          label = "Y-position",
          value = 5,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("align"),
          label = "Alignment",
          choices = c("left", "center", "right"),
          selected = "center"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("spacing"),
          label = "Spacing",
          value = 20,
          min = 0,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("footerFontSize"),
          label = "Footer font size (px)",
          value = 14,
          min = 8,
          max = 48,
          step = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("footerFontWeight"),
          label = "Footer font weight",
          choices = c("normal", "bold", "lighter", "bolder"),
          selected = "normal"
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("footerFontFamily"),
          label = "Footer font family",
          choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("footerColor"),
          label = "Footer color",
          value = "black"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("subtitleFontSize"),
          label = "Subtitle font size (px)",
          value = 12,
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
          value = "grey"
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC cluster footer controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_clusterFooter <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        show         = input$showFooter,
        title        = parse_delimited_input(input$footer),
        subtitle     = parse_delimited_input(input$footerSubtitle),
        height       = input$height,
        x            = input$x,
        y            = input$y,
        align        = input$align,
        spacing      = input$spacing,
        titleFont    = list(
          fontSize      = paste0(input$footerFontSize, "px"),
          fontWeight    = input$footerFontWeight,
          fontFamily    = input$footerFontFamily,
          fill          = input$footerColor
        ),
        subtitleFont = list(
          fontSize      = paste0(input$subtitleFontSize, "px"),
          fontWeight    = input$subtitleFontWeight,
          fontFamily    = input$subtitleFontFamily,
          fill          = input$subtitleColor
        )
      )
    })
  })
}
