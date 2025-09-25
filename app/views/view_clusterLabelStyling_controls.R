#' Cluster Label Styling controls module
#'
#' The \code{clusterLabelStyling_controls} module provides the UI and server components
#' for controlling cluster label font and styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput, checkboxInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for cluster label styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_clusterLabelStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("clusterLabelPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(30vw); top:calc(60vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Cluster Label Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_clusterLabelStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Cluster label styling values
      width = "100px",
      x = 0,
      y = 0,
      position = "left",
      wrapLabel = TRUE,
      labelFontSize = 12,
      labelFontWeight = "bold",
      labelFontStyle = "normal",
      labelFontFamily = "sans-serif",
      labelColor = "black"
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$clusterLabel$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Cluster Label Styling"),
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

            # Layout & Position Tab
            tabPanel(
              "Layout",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("width"),
                             label = "Width",
                             value = isolate(vals$width)
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("position"),
                             label = "Position",
                             choices = c("left", "right"),
                             selected = isolate(vals$position)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("x"),
                             label = "X",
                             value = isolate(vals$x),
                             step = 1
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("y"),
                             label = "Y",
                             value = isolate(vals$y),
                             step = 1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           checkboxInput(
                             inputId = ns("wrapLabel"),
                             label = "Wrap Label",
                             value = isolate(vals$wrapLabel)
                           )
                    )
                  )
              )
            ),

            # Label Font Tab
            tabPanel(
              "Label Font",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("labelFontSize"),
                             label = "Size (px)",
                             value = isolate(vals$labelFontSize),
                             min = 8,
                             max = 48,
                             step = 1
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("labelFontWeight"),
                             label = "Weight",
                             choices = c("normal", "bold", "lighter", "bolder"),
                             selected = isolate(vals$labelFontWeight)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("labelFontStyle"),
                             label = "Style",
                             choices = c("normal", "italic", "oblique"),
                             selected = isolate(vals$labelFontStyle)
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("labelFontFamily"),
                             label = "Family",
                             choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
                             selected = isolate(vals$labelFontFamily)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("labelColor"),
                             label = "Color",
                             value = isolate(vals$labelColor)
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

    # 4) Update persisted values when inputs change
    observeEvent(input$width, ignoreInit = TRUE, {
      if (!is.null(input$width)) vals$width <- input$width
    })
    observeEvent(input$x, ignoreInit = TRUE, {
      if (!is.null(input$x)) vals$x <- input$x
    })
    observeEvent(input$y, ignoreInit = TRUE, {
      if (!is.null(input$y)) vals$y <- input$y
    })
    observeEvent(input$position, ignoreInit = TRUE, {
      if (!is.null(input$position)) vals$position <- input$position
    })
    observeEvent(input$wrapLabel, ignoreInit = TRUE, {
      if (!is.null(input$wrapLabel)) vals$wrapLabel <- input$wrapLabel
    })
    observeEvent(input$labelFontSize, ignoreInit = TRUE, {
      if (!is.null(input$labelFontSize)) vals$labelFontSize <- input$labelFontSize
    })
    observeEvent(input$labelFontWeight, ignoreInit = TRUE, {
      if (!is.null(input$labelFontWeight)) vals$labelFontWeight <- input$labelFontWeight
    })
    observeEvent(input$labelFontStyle, ignoreInit = TRUE, {
      if (!is.null(input$labelFontStyle)) vals$labelFontStyle <- input$labelFontStyle
    })
    observeEvent(input$labelFontFamily, ignoreInit = TRUE, {
      if (!is.null(input$labelFontFamily)) vals$labelFontFamily <- input$labelFontFamily
    })
    observeEvent(input$labelColor, ignoreInit = TRUE, {
      if (!is.null(input$labelColor)) vals$labelColor <- input$labelColor
    })

    reactive({
      list(
        width        = input$width %||% vals$width,
        x            = input$x %||% vals$x,
        y            = input$y %||% vals$y,
        position     = input$position %||% vals$position,
        wrapLabel    = input$wrapLabel %||% vals$wrapLabel,
        fontSize     = paste0(input$labelFontSize %||% vals$labelFontSize, "px"),
        fontWeight   = input$labelFontWeight %||% vals$labelFontWeight,
        fontStyle    = input$labelFontStyle %||% vals$labelFontStyle,
        fontFamily   = input$labelFontFamily %||% vals$labelFontFamily,
        fill         = input$labelColor %||% vals$labelColor
      )
    })
  })
}
