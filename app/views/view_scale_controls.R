#' Gene label controls module
#'
#' The \code{view_gene_label_controls} module provides the UI and server components
#' for toggling and styling gene labels in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, numericInput, selectInput, checkboxInput, icon, div],
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
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("ticksFormat"),
          label   = "Tick format",
          choices = c(
            "Comma separated"   = ",.0f",
            "Integer"           = "0",
            "SI (0 decimals)"   = ".0s",
            "SI (2 decimals)"   = ".2s"
          ),
          selected = ""
        )
    ),
    div(style="margin-bottom:-20px;",
        numericInput(
          inputId = ns("ticksCount"),
          label   = "Number of ticks",
          value   = 10,
          min     = 1,
          step    = 1
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("axisPosition"),
          label   = "Axis position",
          choices = c("bottom", "top"),
          selected = "bottom"
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("axisType"),
          label   = "Axis type",
          choices = c("position", "range"),
          selected = "position"
        )
    ),
    # div(style="margin-bottom:-20px;",
    #     numericInput(
    #       inputId = ns("start"),
    #       label   = "Start",
    #       value   = NULL,
    #       min     = NA,
    #       step    = 1
    #     )
    # ),
    # div(style="margin-bottom:-20px;",
    #     numericInput(
    #       inputId = ns("end"),
    #       label   = "End",
    #       value   = NULL,
    #       min     = NA,
    #       step    = 1
    #     )
    # ),

    ## extra space at bottom
    div(style="height:20px;")
  )
}

#' Shiny server for GC scale controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_scale <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      fmt <- if (nzchar(input$ticksFormat)) input$ticksFormat else NULL
      list(
        scaleHidden = input$scaleHidden,
        scaleReverse = input$scaleReverse,
        scaleBreaks = input$scaleBreaks,
        # start = if (!is.null(input$start) && !is.na(input$start)) input$start else NULL,
        # end = if (!is.null(input$end) && !is.na(input$end)) input$end   else NULL,
        ticksFormat = fmt,
        ticksCount = input$ticksCount,
        axisPosition = input$axisPosition,
        axisType = input$axisType
      )
    })
  })
}
