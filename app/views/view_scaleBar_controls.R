#' Scale bar controls module
#'
#' The \code{view_scaleBar_controls} module provides the UI and server components
#' for controlling the GC scale bar in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, textInput, numericInput, selectInput,
        observeEvent, actionLink, checkboxInput, icon, div],
  shinydashboard[menuItem]
)
#' Shiny UI for GC scale bar controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing scale bar control inputs
#' @export
ui_scaleBar <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Scale bar",
    icon    = icon("ruler"),
    tabName = "scalebar",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showScaleBar"),
          label   = "Show scale bar",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("scaleBarTitle"),
          label = "Scale bar title",
          value = "1 kb"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("scaleBarUnit"),
          label = "Scale bar unit",
          value = 1000,
          min = 1,
          step = 100
        )
    ),
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("scaleBarStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
        )
    ),
    div(style = "height:20px;")
  )
}
#' Shiny server for GC scale controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_scaleBar <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$scaleBarStyling, {
      r$scaleBar$Styling <- input$scaleBarStyling
    })
    reactive({
      list(
        showScaleBar = input$showScaleBar,
        scaleBarTitle = input$scaleBarTitle,
        scaleBarUnit = input$scaleBarUnit
      )
    })
  })
}
