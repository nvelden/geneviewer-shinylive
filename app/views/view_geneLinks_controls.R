#' GC Links controls module
#'
#' The `view_GC_links_controls` module provides the UI and server components
#' for controlling the GC_links function parameters in the Gene Cluster Dashboard.
'.__module__.'
box::use(
  shiny[NS, moduleServer, reactive, checkboxInput, req, numericInput,
        selectInput, icon, div, textInput, updateSelectInput, observe],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)

#' Shiny UI for GC links controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing GC links control inputs
#' @export
ui_links <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Gene links",
    icon    = icon("link"),
    tabName = "gcLinks",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("show"),
          label   = "Show GC links",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("colorByGroup"),
          label   = "Color by Group",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("curve"),
          label   = "Curved Links",
          value   = TRUE
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId  = ns("group"),
          label = "Group",
          choices = NULL
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("value1"),
          label = "Values 1",
          placeholder = "genID1, genID2, ..."
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("value2"),
          label = "Values 2",
          placeholder = "genID3, genID4, ..."
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("linkOpacity"),
          label   = "Link opacity",
          value   = 1,
          min     = 0,
          max     = 1,
          step    = 0.1
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC links controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_links <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(r$cluster_data)
      updateSelectInput(
        session,
        "group",
        choices = names(r$cluster_data),
        selected = names(r$cluster_data)[1]
      )
    })

    reactive(list(
      show = input$show,
      group = input$group,
      curve = input$curve,
      value1 = parse_delimited_input(input$value1),
      value2 = parse_delimited_input(input$value2),
      colorByGroup = input$colorByGroup,
      opacity = input$linkOpacity
    ))

  })
}
