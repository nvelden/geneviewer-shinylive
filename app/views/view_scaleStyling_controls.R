#' Scale Styling controls module
#'
#' The \code{scaleStyling_controls} module provides the UI and server components
#' for controlling scale styling (axis, ticks, and text) in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for scale styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_scaleStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("scaleStylingPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(30vw); top:calc(50vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Scale Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_scaleStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # General settings
      padding = 2,
      axisType = "position",
      axisPosition = "bottom",
      ticksCount = 10,
      ticksFormat = ",.0f",
      scaleBreakThreshold = 20,
      scaleBreakPadding = 1,
      # Tick styling values
      tickStroke = "grey",
      tickStrokeWidth = 1,
      tickLineLength = 6,
      # Text styling values
      textFill = "black",
      textFontSize = 10,
      textFontFamily = "Arial",
      textYPosition = NULL,
      # Line styling values
      lineStroke = "grey",
      lineStrokeWidth = 1,
      # Y position
      yPosition = NULL
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$scale$Styling, {
      boxVisible(!boxVisible())
    }, ignoreInit = TRUE)

    observeEvent(input$styleBox$visible, {
      if(!input$styleBox$visible) return(boxVisible(!boxVisible()))
    }, ignoreInit = TRUE)

    # 3) Render the box (uses persisted values as defaults)
    output$styleBoxUI <- renderUI({
      if (!boxVisible()) return(NULL)

      shinyjqui::jqui_draggable(
        shinydashboardPlus::box(
          width = 12,
          title = actionLink(ns("optionsTitle"), "Scale Styling"),
          status = "primary",
          color = "black",
          solidHeader = TRUE,
          align = "left",
          id = ns("styleBox"),
          class = "controlBox",
          collapsible = FALSE,
          closable = TRUE,
          # Use tabs to organize different aspects
          tabsetPanel(
            id = ns("stylingTabs"),

            # General Settings Tab
            tabPanel(
              "General",
              div(style = "padding: 10px;",
                  h4("Axis Settings"),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("axisPosition"),
                             label = "Axis Position",
                             choices = c("bottom", "top"),
                             selected = isolate(vals$axisPosition)
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("axisType"),
                             label = "Axis Type",
                             choices = c("position", "range"),
                             selected = isolate(vals$axisType)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("ticksCount"),
                             label = "Number of Ticks",
                             value = isolate(vals$ticksCount),
                             min = 1,
                             step = 1
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("ticksFormat"),
                             label = "Tick Format",
                             choices = c(
                               "Comma separated" = ",.0f",
                               "Integer" = "0",
                               "SI (0 decimals)" = ".0s",
                               "SI (2 decimals)" = ".2s"
                             ),
                             selected = isolate(vals$ticksFormat)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("padding"),
                             label = "Padding",
                             value = isolate(vals$padding),
                             min = 0,
                             step = 1
                           )
                    )
                  ),
                  h4("Scale Break Settings"),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("scaleBreakThreshold"),
                             label = "Break Threshold (%)",
                             value = isolate(vals$scaleBreakThreshold),
                             min = 1,
                             max = 100,
                             step = 1
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("scaleBreakPadding"),
                             label = "Break Padding",
                             value = isolate(vals$scaleBreakPadding),
                             min = 0,
                             step = 0.5
                           )
                    )
                  )
              )
            ),

            # Tick Styling Tab
            tabPanel(
              "Ticks",
              div(style = "padding: 10px;",
                  h4("Tick Styling"),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("tickStroke"),
                             label = "Tick Color",
                             value = isolate(vals$tickStroke),
                             placeholder = "e.g., grey, #808080"
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("tickStrokeWidth"),
                             label = "Stroke Width",
                             value = isolate(vals$tickStrokeWidth),
                             min = 0.5,
                             max = 10,
                             step = 0.5
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("tickLineLength"),
                             label = "Tick Length",
                             value = isolate(vals$tickLineLength),
                             min = 1,
                             max = 20,
                             step = 1
                           )
                    )
                  )
              )
            ),

            # Text Styling Tab
            tabPanel(
              "Text",
              div(style = "padding: 10px;",
                  h4("Text Styling"),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("textFill"),
                             label = "Text Color",
                             value = isolate(vals$textFill),
                             placeholder = "e.g., black, #000000"
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("textFontSize"),
                             label = "Font Size",
                             value = isolate(vals$textFontSize),
                             min = 6,
                             max = 24,
                             step = 1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("textFontFamily"),
                             label = "Font Family",
                             choices = c("Arial", "Helvetica", "Times New Roman",
                                         "Courier New", "Georgia", "Verdana",
                                         "sans-serif", "serif", "monospace"),
                             selected = isolate(vals$textFontFamily)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("textYPosition"),
                             label = "Text Y Position",
                             value = isolate(vals$textYPosition),
                             min = -50,
                             max = 50,
                             step = 1
                           )
                    )
                  ),
                  div(style = "margin-top: 10px;",
                      "Leave Text Y Position empty to use default"
                  )
              )
            ),

            # Axis Line Styling Tab
            tabPanel(
              "Axis Line",
              div(style = "padding: 10px;",
                  h4("Axis Line Styling"),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("lineStroke"),
                             label = "Line Color",
                             value = isolate(vals$lineStroke),
                             placeholder = "e.g., grey, #808080"
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("lineStrokeWidth"),
                             label = "Line Width",
                             value = isolate(vals$lineStrokeWidth),
                             min = 0.5,
                             max = 10,
                             step = 0.5
                           )
                    )
                  )
              )
            ),

            # Position Tab
            tabPanel(
              "Position",
              div(style = "padding: 10px;",
                  h4("Axis Y-Position"),
                  fluidRow(
                    column(12,
                           numericInput(
                             inputId = ns("yPosition"),
                             label = "Axis Y Position (1-100)",
                             value = isolate(vals$yPosition),
                             min = 1,
                             max = 100,
                             step = 1
                           )
                    )
                  ),
                  div(style = "margin-top: 10px;",
                      "Leave empty to use default position"
                  )
              )
            )
          )
        ),
        options = list(
          cancel = ".form-control, .selectize-control, input, textarea, button, .btn, .shiny-input-container",
          stack = ".controlBox",
          scroll = FALSE,
          containment = "window"
        )
      )
    })

    # 4) Update the persisted values whenever any input changes
    observeEvent(input$padding, vals$padding <- input$padding)
    observeEvent(input$axisType, vals$axisType <- input$axisType)
    observeEvent(input$axisPosition, vals$axisPosition <- input$axisPosition)
    observeEvent(input$ticksCount, vals$ticksCount <- input$ticksCount)
    observeEvent(input$ticksFormat, vals$ticksFormat <- input$ticksFormat)
    observeEvent(input$scaleBreakThreshold, vals$scaleBreakThreshold <- input$scaleBreakThreshold)
    observeEvent(input$scaleBreakPadding, vals$scaleBreakPadding <- input$scaleBreakPadding)
    observeEvent(input$tickStroke, vals$tickStroke <- input$tickStroke)
    observeEvent(input$tickStrokeWidth, vals$tickStrokeWidth <- input$tickStrokeWidth)
    observeEvent(input$tickLineLength, vals$tickLineLength <- input$tickLineLength)
    observeEvent(input$textFill, vals$textFill <- input$textFill)
    observeEvent(input$textFontSize, vals$textFontSize <- input$textFontSize)
    observeEvent(input$textFontFamily, vals$textFontFamily <- input$textFontFamily)
    observeEvent(input$textYPosition, vals$textYPosition <- input$textYPosition)
    observeEvent(input$lineStroke, vals$lineStroke <- input$lineStroke)
    observeEvent(input$lineStrokeWidth, vals$lineStrokeWidth <- input$lineStrokeWidth)
    observeEvent(input$yPosition, vals$yPosition <- input$yPosition)

    # 5) Return the reactive with current inputs
    reactive({
      # Build textStyle list conditionally
      textStyleList <- list(
        fill = vals$textFill,
        fontSize = paste0(vals$textFontSize, "px"),
        fontFamily = vals$textFontFamily
      )

      # Add dy only if textYPosition is not NULL
      if (!is.null(vals$textYPosition) && !is.na(vals$textYPosition)) {
        textStyleList$dy = vals$textYPosition
      }

      list(
        padding = vals$padding,
        axisType = vals$axisType,
        axisPosition = vals$axisPosition,
        ticksCount = vals$ticksCount,
        ticksFormat = vals$ticksFormat,
        scaleBreakThreshold = vals$scaleBreakThreshold,
        scaleBreakPadding = vals$scaleBreakPadding,
        y = vals$yPosition,
        tickStyle = list(
          stroke = vals$tickStroke,
          strokeWidth = vals$tickStrokeWidth,
          lineLength = vals$tickLineLength
        ),
        textStyle = textStyleList,
        lineStyle = list(
          stroke = vals$lineStroke,
          strokeWidth = vals$lineStrokeWidth
        )
      )
    })
  })
}
