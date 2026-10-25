#' Theme controls module
#'
#' The \code{view_theme_controls} module provides the UI and server components
#' for setting the GC theme in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput, numericInput, icon, div],
  shinydashboard[menuItem]
)

#' Shiny UI for GC theme controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing theme control inputs
#' @export
ui_theme <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Theme",
    icon    = icon("brush"),
    tabName = "theme",
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId  = ns("theme"),
          label    = "Theme",
          choices  = c(
            "None"         = "",
            "Default"      = "default",
            "Classic"      = "classic",
            "Dark"         = "dark",
            "Vintage"      = "vintage",
            "Infographic"  = "infographic",
            "Publication"  = "publication",
            "Black & white" = "bw"
          ),
          selected = ""
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("textScale"),
          label   = "Text scale",
          value   = 1,
          min     = 0.5,
          max     = 3,
          step    = 0.1
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId  = ns("fontFamily"),
          label    = "Font family",
          choices  = c("Theme font" = "", "sans-serif", "serif", "monospace",
                       "Arial", "Helvetica", "Times New Roman"),
          selected = ""
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("textColor"),
          label = "Text color",
          placeholder = "e.g. black"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("lineColor"),
          label = "Line color",
          placeholder = "e.g. grey"
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId = ns("background"),
          label = "Background",
          placeholder = "e.g. white"
        )
    ),
    div(style = "padding: 25px 15px 0 15px; white-space: normal; font-size: 12px;",
        "With a theme, only the options you change elsewhere override it."
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC theme controls
#'
#' @param id Namespace id for this module
#' @export
server_theme <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {
    reactive({
      # Empty inputs leave the setting to the theme
      blank_null <- function(x) if (is.null(x) || is.na(x) || !nzchar(trimws(x))) NULL else trimws(x)
      text_scale <- input$textScale
      if (is.null(text_scale) || is.na(text_scale) || text_scale == 1) text_scale <- NULL

      settings <- list(
        theme      = blank_null(input$theme),
        textScale  = text_scale,
        fontFamily = blank_null(input$fontFamily),
        textColor  = blank_null(input$textColor),
        lineColor  = blank_null(input$lineColor),
        background = blank_null(input$background)
      )
      settings <- Filter(Negate(is.null), settings)

      list(
        active   = length(settings) > 0,
        settings = settings
      )
    })
  })
}
