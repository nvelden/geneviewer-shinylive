#' Cluster title controls module
#'
#' The \code{view_clusterTitle_controls} module provides the UI and server components
#' for controlling the GC cluster title in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, textInput, checkboxInput, icon, div,
        observeEvent, actionLink],
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
    div(style = "padding-top: 10px;",
        actionLink(
          inputId = ns("titleStyling"),
          label   = "Styling",
          class   = "btn-info",
          style   = "background-color:transparent;"
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
    observeEvent(input$titleStyling, {
      r$clusterTitle$Styling <- input$titleStyling
    })

    reactive({
      list(
        show = input$showTitle,
        title = parse_delimited_input(input$title),
        subtitle = parse_delimited_input(input$subtitle)
      )
    })
  })
}
