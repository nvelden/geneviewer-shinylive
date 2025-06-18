#' Sequence controls module
#'
#' The \code{view_sequence_controls} module provides the UI and server components
#' for controlling the GC_sequence layer in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, checkboxInput,
        numericInput, textInput, div, icon, conditionalPanel],
  shinydashboard[menuItem]
)


#' Shiny UI for GC sequence controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing sequence control inputs
#' @export
ui_sequence <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Sequence",
    icon    = icon("dna"),
    tabName = "sequence",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showSequence"),
          label   = "Show sequence",
          value   = TRUE
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("sequenceY"),
          label   = "Y-position",
          value   = 50,
          step    = 1
        )
    ),
    div(style = "margin-bottom:-20px;",
        textInput(
          inputId   = ns("seqStroke"),
          label     = "Sequence stroke color",
          value     = "grey"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("seqStrokeWidth"),
          label   = "Sequence stroke width",
          value   = 1,
          step    = 0.1
        )
    ),
    div(style = "height:20px;"),
    conditionalPanel(
      condition = "input['scaleControls-scaleBreaks'] == true",
      # ns = ns,
      div(style = "margin-bottom:-20px;",
          textInput(
            inputId   = ns("markerStroke"),
            label     = "Marker stroke color",
            value     = "grey"
          )
      ),
      div(style = "margin-bottom:-20px;",
          numericInput(
            inputId = ns("markerStrokeWidth"),
            label   = "Marker stroke width",
            value   = 1,
            step    = 0.1
          )
      ),
      div(style = "height:20px;")
    )
  )
}

#' Shiny server for GC sequence controls
#'
#' @param id Namespace id for this module
#' @export
server_sequence <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        showSequence = input$showSequence,
        sequenceY = input$sequenceY,
        sequenceStroke = input$seqStroke,
        sequenceStrokeWidth = input$seqStrokeWidth,
        sequenceMarkerStroke = input$markerStroke,
        sequenceMarkerStrokeWidth = input$markerStrokeWidth
        )
    })
  })
}
