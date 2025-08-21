#' Scale controls module
#'
#' The \code{view_scale_controls} module provides the UI and server components
#' for controlling the GC scale in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, checkboxInput,
        icon, div, observeEvent, actionLink],
  shinydashboard[menuItem]
)


#' Shiny UI for GC scale controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing scale control inputs
#' @export
ui_scale <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Scale",
    icon    = icon("arrows-alt-h"),
    tabName = "scale",
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("scaleHidden"),
          label = "Hide axis",
          value = TRUE
        )
    ),
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("scaleReverse"),
          label   = "Reverse scale",
          value   = FALSE
        )
    ),
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("scaleBreaks"),
          label   = "Scale breaks",
          value   = FALSE
        )
    ),
    # Add styling button
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("scaleStyling"),
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
server_scale <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    # Handle styling button click
    observeEvent(input$scaleStyling, {
      r$scale$Styling <- input$scaleStyling
    })

    reactive({
      list(
        scaleHidden = input$scaleHidden,
        scaleReverse = input$scaleReverse,
        scaleBreaks = input$scaleBreaks
      )
    })
  })
}
