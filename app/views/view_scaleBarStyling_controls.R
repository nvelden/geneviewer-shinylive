#' Scale Bar Styling controls module
#'
#' The \code{scaleBarStyling_controls} module provides the UI and server components
#' for controlling GC_scaleBar styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput, checkboxInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for scale bar styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_scaleBarStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("scaleBarStylingPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(35vw); top:calc(45vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Scale Bar Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_scaleBarStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Position
      x = 0,
      y = 10,

      # Label style
      labelPosition = "left",
      fontSize = "10px",
      fontFamily = "sans-serif",
      fill = "black",

      # Line style
      lineStroke = "grey",
      lineStrokeWidth = 1,

      # Tick style
      tickStroke = "grey",
      tickStrokeWidth = 1,

      # General options
      textPadding = 0
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$scaleBar$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Scale Bar Styling"),
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

            # Label Style Tab
            tabPanel(
              "Label Style",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("labelPosition"),
                             label = "Label Position",
                             choices = c("left", "right"),
                             selected = isolate(vals$labelPosition)
                           )
                    ),
                    column(6,
                           textInput(
                             inputId = ns("fontSize"),
                             label = "Font Size",
                             value = isolate(vals$fontSize),
                             placeholder = "e.g., 12px"
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("fontFamily"),
                             label = "Font Family",
                             choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica"),
                             selected = isolate(vals$fontFamily)
                           )
                    ),
                    column(6,
                           textInput(
                             inputId = ns("fill"),
                             label = "Text Color",
                             value = isolate(vals$fill)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("textPadding"),
                             label = "Text Padding",
                             value = isolate(vals$textPadding),
                             step = 1
                           )
                    )
                  )
              )
            ),

            # Line & Tick Style Tab
            tabPanel(
              "Line & Tick Style",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("lineStroke"),
                             label = "Line Color",
                             value = isolate(vals$lineStroke)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("lineStrokeWidth"),
                             label = "Line Width",
                             value = isolate(vals$lineStrokeWidth),
                             min = 0,
                             step = 0.5
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("tickStroke"),
                             label = "Tick Color",
                             value = isolate(vals$tickStroke)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("tickStrokeWidth"),
                             label = "Tick Width",
                             value = isolate(vals$tickStrokeWidth),
                             min = 0,
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
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("x"),
                             label = "X Position",
                             value = isolate(vals$x),
                             step = 1
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("y"),
                             label = "Y Position",
                             value = isolate(vals$y),
                             step = 1
                           )
                    )
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

    # 4) Observers to update reactive values when inputs change
    observeEvent(input$x, { vals$x <- input$x })
    observeEvent(input$y, { vals$y <- input$y })
    observeEvent(input$labelPosition, { vals$labelPosition <- input$labelPosition })
    observeEvent(input$fontSize, { vals$fontSize <- input$fontSize })
    observeEvent(input$fontFamily, { vals$fontFamily <- input$fontFamily })
    observeEvent(input$fill, { vals$fill <- input$fill })
    observeEvent(input$textPadding, { vals$textPadding <- input$textPadding })

    observeEvent(input$lineStroke, { vals$lineStroke <- input$lineStroke })
    observeEvent(input$lineStrokeWidth, { vals$lineStrokeWidth <- input$lineStrokeWidth })
    observeEvent(input$tickStroke, { vals$tickStroke <- input$tickStroke })
    observeEvent(input$tickStrokeWidth, { vals$tickStrokeWidth <- input$tickStrokeWidth })

    # 5) Return reactive values for use in main app
    reactive({
      list(
        # Position
        x = vals$x,
        y = vals$y,

        # Label styling
        labelStyle = list(
          labelPosition = vals$labelPosition,
          fontSize = vals$fontSize,
          fontFamily = vals$fontFamily,
          fill = vals$fill
        ),

        # Line styling
        scaleBarLineStyle = list(
          stroke = vals$lineStroke,
          strokeWidth = vals$lineStrokeWidth
        ),

        # Tick styling
        scaleBarTickStyle = list(
          stroke = vals$tickStroke,
          strokeWidth = vals$tickStrokeWidth
        ),

        # General options
        textPadding = vals$textPadding
      )
    })

  })
}
