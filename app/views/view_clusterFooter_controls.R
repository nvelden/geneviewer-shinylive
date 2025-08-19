#' Cluster footer controls module
#'
#' The \code{view_clusterFooter_controls} module provides the UI and server components
#' for controlling the GC cluster footer in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, textInput, numericInput, selectInput, checkboxInput, icon, div,
        observeEvent, actionLink],
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
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("footerStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
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

    observeEvent(input$footerStyling, {
      r$clusterFooter$Styling <- input$footerStyling
    })

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
