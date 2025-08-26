#' Gene Styling controls module
#'
#' The \code{geneStyling_controls} module provides the UI and server components
#' for controlling gene visual styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput, checkboxInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for gene styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_geneStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("geneStylingPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(30vw); top:calc(40vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Gene Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_geneStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      strokeColor = "black",
      strokeWidth = 1,
      arrowheadWidth = NULL,
      arrowheadHeight = NULL,
      y = 50
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$gene$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Gene Styling"),
          status = "primary",
          color = "black",
          solidHeader = TRUE,
          align = "left",
          id = ns("styleBox"),
          class = "controlBox",
          collapsible = FALSE,
          closable = TRUE,
          tabsetPanel(
            id = ns("geneTabs"),

            # Appearance Tab
            tabPanel("Appearance",
                     div(style = "padding: 10px;",
                         fluidRow(
                           column(6,
                                  numericInput(
                                    inputId = ns("strokeWidth"),
                                    label = "Border Width:",
                                    value = isolate(vals$strokeWidth),
                                    min = 0,
                                    step = 0.5
                                  )
                           ),
                           column(6,
                                  textInput(
                                    inputId = ns("strokeColor"),
                                    label = "Border Color:",
                                    value = isolate(vals$strokeColor),
                                    placeholder = "e.g., #000000, black"
                                  )
                           )
                         )
                     )
            ),

            # Marker Tab
            tabPanel("Marker",
                     div(style = "padding: 10px;",
                         fluidRow(
                           column(6,
                                  numericInput(
                                    inputId = ns("arrowheadWidth"),
                                    label = "Arrowhead Width:",
                                    value = isolate(vals$arrowheadWidth),
                                    min = 1,
                                    step = 1
                                  )
                           ),
                           column(6,
                                  numericInput(
                                    inputId = ns("arrowheadHeight"),
                                    label = "Arrowhead Height:",
                                    value = isolate(vals$arrowheadHeight),
                                    min = 1,
                                    step = 1
                                  )
                           )
                         )
                     )
            ),

            # Position Tab
            tabPanel("Position",
                     div(style = "padding: 10px;",
                         fluidRow(
                           column(12,
                                  numericInput(
                                    inputId = ns("y"),
                                    label = "Y Position:",
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

    # 4) Observe changes and update vals
    observeEvent(input$strokeColor, { vals$strokeColor <- input$strokeColor })
    observeEvent(input$strokeWidth, { vals$strokeWidth <- input$strokeWidth })
    observeEvent(input$arrowheadWidth, { vals$arrowheadWidth <- input$arrowheadWidth })
    observeEvent(input$arrowheadHeight, { vals$arrowheadHeight <- input$arrowheadHeight })
    observeEvent(input$y, { vals$y <- input$y })

    # 5) Return reactive values for the main application
    reactive({
      list(
        strokeColor = input$strokeColor,
        strokeWidth = input$strokeWidth,
        arrowheadWidth = input$arrowheadWidth,
        arrowheadHeight = input$arrowheadHeight,
        y = input$y
      )
    })
  })
}
