#' Coordinates Styling controls module
#'
#' The \code{coordinatesStyling_controls} module provides the UI and server components
#' for controlling coordinates styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for coordinates styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_coordinatesStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("coordinatesPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(30vw); top:calc(60vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Coordinates Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_coordinatesStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Position settings
      yPositionTop = 55,
      yPositionBottom = 45,
      rotate = -45,
      overlapThreshold = 20,
      # Tick style settings
      tickStroke = "black",
      tickStrokeWidth = 1,
      tickLineLength = 5,
      # Text style settings
      textFill = "black",
      textFontSize = 12,
      textFontFamily = "sans-serif"
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$coordinates$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Coordinates Styling"),
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

            # Position Tab
            tabPanel(
              "Position",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("yPositionTop"),
                             label = "Y Position Top",
                             value = isolate(vals$yPositionTop),
                             min = 0,
                             step = 1
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("yPositionBottom"),
                             label = "Y Position Bottom",
                             value = isolate(vals$yPositionBottom),
                             min = 0,
                             step = 1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("rotate"),
                             label = "Rotation (degrees)",
                             value = isolate(vals$rotate),
                             step = 1
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("overlapThreshold"),
                             label = "Overlap Threshold",
                             value = isolate(vals$overlapThreshold),
                             min = 0,
                             step = 1
                           )
                    )
                  )
              )
            ),

            # Tick Style Tab
            tabPanel(
              "Tick Style",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("tickStroke"),
                             label = "Stroke Color",
                             value = isolate(vals$tickStroke)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("tickStrokeWidth"),
                             label = "Stroke Width",
                             value = isolate(vals$tickStrokeWidth),
                             min = 0,
                             step = 0.1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("tickLineLength"),
                             label = "Line Length",
                             value = isolate(vals$tickLineLength),
                             min = 0,
                             step = 1
                           )
                    )
                  )
              )
            ),

            # Text Style Tab
            tabPanel(
              "Text Style",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("textFill"),
                             label = "Text Color",
                             value = isolate(vals$textFill)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("textFontSize"),
                             label = "Font Size (px)",
                             value = isolate(vals$textFontSize),
                             min = 8,
                             max = 48,
                             step = 1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("textFontFamily"),
                             label = "Font Family",
                             choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
                             selected = isolate(vals$textFontFamily)
                           )
                    )
                  )
              )
            )
          )
        ),
        options = list(
          cancel = ".form-control, .selectize-control, input, textarea, button, .btn, .shiny-input-container"
        )
      )
    })

    # 4) Update persisted values when inputs change
    observeEvent(input$yPositionTop, ignoreInit = TRUE, {
      if (!is.null(input$yPositionTop)) vals$yPositionTop <- input$yPositionTop
    })
    observeEvent(input$yPositionBottom, ignoreInit = TRUE, {
      if (!is.null(input$yPositionBottom)) vals$yPositionBottom <- input$yPositionBottom
    })
    observeEvent(input$rotate, ignoreInit = TRUE, {
      if (!is.null(input$rotate)) vals$rotate <- input$rotate
    })
    observeEvent(input$overlapThreshold, ignoreInit = TRUE, {
      if (!is.null(input$overlapThreshold)) vals$overlapThreshold <- input$overlapThreshold
    })
    observeEvent(input$tickStroke, ignoreInit = TRUE, {
      if (!is.null(input$tickStroke)) vals$tickStroke <- input$tickStroke
    })
    observeEvent(input$tickStrokeWidth, ignoreInit = TRUE, {
      if (!is.null(input$tickStrokeWidth)) vals$tickStrokeWidth <- input$tickStrokeWidth
    })
    observeEvent(input$tickLineLength, ignoreInit = TRUE, {
      if (!is.null(input$tickLineLength)) vals$tickLineLength <- input$tickLineLength
    })
    observeEvent(input$textFill, ignoreInit = TRUE, {
      if (!is.null(input$textFill)) vals$textFill <- input$textFill
    })
    observeEvent(input$textFontSize, ignoreInit = TRUE, {
      if (!is.null(input$textFontSize)) vals$textFontSize <- input$textFontSize
    })
    observeEvent(input$textFontFamily, ignoreInit = TRUE, {
      if (!is.null(input$textFontFamily)) vals$textFontFamily <- input$textFontFamily
    })

    # 5) Expose current settings - coordinates styling settings
    reactive({
      list(
        # Position parameters (passed via ... in GC_coordinates)
        yPositionTop     = input$yPositionTop %||% vals$yPositionTop,
        yPositionBottom  = input$yPositionBottom %||% vals$yPositionBottom,
        rotate           = input$rotate %||% vals$rotate,
        overlapThreshold = input$overlapThreshold %||% vals$overlapThreshold,
        # Tick style (passed as tickStyle list)
        tickStyle = list(
          stroke      = input$tickStroke %||% vals$tickStroke,
          strokeWidth = input$tickStrokeWidth %||% vals$tickStrokeWidth,
          lineLength  = input$tickLineLength %||% vals$tickLineLength
        ),
        # Text style (passed as textStyle list)
        textStyle = list(
          fill       = input$textFill %||% vals$textFill,
          fontSize   = paste0(input$textFontSize %||% vals$textFontSize, "px"),
          fontFamily = input$textFontFamily %||% vals$textFontFamily
        )
      )
    })
  })
}
