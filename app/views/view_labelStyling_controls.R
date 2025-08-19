#' Label Styling controls module
#'
#' The \code{labelStyling_controls} module provides the UI and server components
#' for controlling label font and styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for label styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_labelStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("labelStylingPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(30vw); top:calc(40vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Label Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_labelStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Label styling values
      fontSize = 12,
      fontWeight = "normal",
      fontStyle = "normal",
      fontFamily = "sans-serif",
      fill = "black"
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$label$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Label Styling"),
          status = "primary",
          color = "black",
          solidHeader = TRUE,
          align = "left",
          id = ns("styleBox"),
          class = "controlBox",
          collapsible = FALSE,
          closable = TRUE,

          # Font styling controls
          div(style = "padding: 10px;",
              fluidRow(
                column(6,
                       numericInput(
                         inputId = ns("fontSize"),
                         label = "Font Size (px)",
                         value = isolate(vals$fontSize),
                         min = 1,
                         step = 1
                       )
                ),
                column(6,
                       selectInput(
                         inputId = ns("fontWeight"),
                         label = "Font Weight",
                         choices = c("normal", "bold", "lighter", "bolder"),
                         selected = isolate(vals$fontWeight)
                       )
                )
              ),
              fluidRow(
                column(6,
                       selectInput(
                         inputId = ns("fontStyle"),
                         label = "Font Style",
                         choices = c("normal", "italic", "oblique"),
                         selected = isolate(vals$fontStyle)
                       )
                ),
                column(6,
                       selectInput(
                         inputId = ns("fontFamily"),
                         label = "Font Family",
                         choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
                         selected = isolate(vals$fontFamily)
                       )
                )
              ),
              fluidRow(
                column(12,
                       textInput(
                         inputId = ns("fill"),
                         label = "Text Color",
                         value = isolate(vals$fill),
                         placeholder = "e.g., black, #000000, rgb(0,0,0)"
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

    # 4) Store inputs in vals when they change
    observeEvent(input$fontSize, ignoreInit = TRUE, {
      if (!is.null(input$fontSize)) vals$fontSize <- input$fontSize
    })
    observeEvent(input$fontWeight, ignoreInit = TRUE, {
      if (!is.null(input$fontWeight)) vals$fontWeight <- input$fontWeight
    })
    observeEvent(input$fontStyle, ignoreInit = TRUE, {
      if (!is.null(input$fontStyle)) vals$fontStyle <- input$fontStyle
    })
    observeEvent(input$fontFamily, ignoreInit = TRUE, {
      if (!is.null(input$fontFamily)) vals$fontFamily <- input$fontFamily
    })
    observeEvent(input$fill, ignoreInit = TRUE, {
      if (!is.null(input$fill)) vals$fill <- input$fill
    })

    # 5) Expose current settings
    reactive({
      list(
        fontSize   = paste0(input$fontSize %||% vals$fontSize, "px"),
        fontWeight = input$fontWeight %||% vals$fontWeight,
        fontStyle  = input$fontStyle %||% vals$fontStyle,
        fontFamily = input$fontFamily %||% vals$fontFamily,
        fill       = input$fill %||% vals$fill
      )
    })
  })
}
