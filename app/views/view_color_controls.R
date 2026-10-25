#' Scale bar controls module
#'
#' The \code{view_color_controls} module provides the UI and server components
#' for controlling the GC color in the Gene Cluster Dashboard.
'.__module__.'


box::use(
  shiny[NS, moduleServer, observe, selectInput, div, icon, reactive, textInput,
        checkboxInput, numericInput, conditionalPanel],
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
          choices  = list(
            "geneviewer palettes" = c(
              "Okabe-Ito (colour-blind safe)" = "okabeIto",
              "Classic"      = "classic",
              "Vivid"        = "vivid",
              "Dark"         = "dark",
              "Vintage"      = "vintage",
              "Infographic"  = "infographic",
              "Greys"        = "greys",
              "Blues"        = "blues"
            ),
            "d3 schemes" = c(
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
            )
          ),
          selected = "schemeCategory10"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("customColors"),
          label = "Custom colors",
          placeholder = "red, green, ..."
        )
    ),
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showPatterns"),
          label   = "Patterns",
          value   = FALSE
        )
    ),
    conditionalPanel(
      condition = "input.showPatterns",
      ns = ns,
      div(style = "margin-bottom:-20px;",
          textInput(
            inputId = ns("patternColor"),
            label = "Pattern color",
            value = "black"
          )
      ),
      div(style = "margin-bottom:-20px;",
          selectInput(
            inputId  = ns("patternBackground"),
            label    = "Pattern background",
            choices  = c("White" = "white", "Gene colors" = "none"),
            selected = "white"
          )
      ),
      div(style = "margin-bottom:-20px;",
          numericInput(
            inputId = ns("patternSize"),
            label = "Pattern size",
            value = 6,
            min = 2,
            step = 1
          )
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

#' Shiny server for GC pattern controls
#'
#' @param id Namespace id for this module (the color controls)
#' @export
server_pattern <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        show = isTRUE(input$showPatterns),
        color = input$patternColor,
        # "none" draws the patterns on top of the gene colours
        background = if (identical(input$patternBackground, "none")) NULL else input$patternBackground,
        size = input$patternSize
      )
    })
  })
}
