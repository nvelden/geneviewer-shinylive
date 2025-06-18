#' Scale bar controls module
#'
#' The \code{view_color_controls} module provides the UI and server components
#' for controlling the GC color in the Gene Cluster Dashboard.
'.__module__.'


box::use(
  shiny[NS, moduleServer, observe, selectInput, div, icon, reactive, textInput],
  shinydashboard[menuItem],
  ../logic/utils[parse_delimited_input]
)


#' Shiny UI for GC scale bar controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing scale bar control inputs
#' @export
ui_color <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Color",
    icon    = icon("palette"),
    tabName = "scalebar",
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId  = ns("colorScheme"),
          label    = "Color scheme",
          choices  = c(
            "Category10"   = "schemeCategory10",
            "Tableau10"    = "schemeTableau10",
            "Accent"       = "schemeAccent",
            "Dark2"        = "schemeDark2",
            "Paired"       = "schemePaired",
            "Pastel1"      = "schemePastel1",
            "Pastel2"      = "schemePastel2",
            "Set1"         = "schemeSet1",
            "Set2"         = "schemeSet2",
            "Set3"         = "schemeSet3"
          ),
          selected = "Category10"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("customColors"),
          label = "Custom colors",
          placeholder = "red, green, ..."
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
server_color <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        colorScheme = input$colorScheme,
        customColors = parse_delimited_input(input$customColors)
      )
    })
  })
}
