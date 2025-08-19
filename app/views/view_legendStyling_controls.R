#' Legend Styling controls module
#'
#' The \code{legendStyling_controls} module provides the UI and server components
#' for controlling legend font and fill styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for Legend Styling controls
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder (to drop anywhere in the app)
#' @export
ui_legendStyling <- function(id) {
  ns <- NS(id)
    div(
      id = ns("labelOptionsPlaceholder"),
      style = "position: absolute; z-index:100; width: calc(30vw); top:calc(60vh);",
      uiOutput(ns("stylingBoxUI"))
    )

}

#' Shiny server for Legend Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_legendStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      size   = 12,
      family = "sans-serif",
      fill   = "black"
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$legend$Styling, {
      boxVisible(!boxVisible())
    }, ignoreInit = TRUE)

    observeEvent(input$stylingBox$visible, {
      if(!input$stylingBox$visible) return(boxVisible(!boxVisible()))
    }, ignoreInit = TRUE)

    # 3) Render the box (uses persisted values as defaults)
    output$stylingBoxUI <- renderUI({
      if (!boxVisible()) return(NULL)

      shinyjqui::jqui_draggable(
        shinydashboardPlus::box(
          width = 12,
          title = actionLink(ns("optionsTitle"), "Styling options"),
          status = "primary",
          color = "black",
          solidHeader = TRUE,
          align = "left",
          id = ns("stylingBox"),
          class = "controlBox",
          collapsible = FALSE,
          closable = TRUE,
          # Controls use the cached values so they restore on reopen
          div(style = "margin-bottom:-15px;",
              numericInput(ns("legendFontSize"), "Font size (px)", value = isolate(vals$size), step = 1)),
          div(style = "margin-bottom:-15px;",
              selectInput(ns("legendFontFamily"), "Font family",
                          choices = c("sans-serif","serif","monospace","cursive","fantasy"),
                          selected = isolate(vals$family))),
          div(style = "margin-bottom:-15px;",
              textInput(ns("legendFill"), "Text color", value = isolate(vals$fill)))
        ),
        options = list(
          cancel = ".form-control, .selectize-control, input, textarea, button, .btn, .shiny-input-container"
        ),
      )
    })

    # 4) Keep the cache updated as the user changes inputs
    observeEvent(input$legendFontSize, ignoreInit = TRUE, {
      if (!is.null(input$legendFontSize)) vals$size <- input$legendFontSize
    })
    observeEvent(input$legendFontFamily, ignoreInit = TRUE, {
      if (!is.null(input$legendFontFamily)) vals$family <- input$legendFontFamily
    })
    observeEvent(input$legendFill, ignoreInit = TRUE, {
      if (!is.null(input$legendFill)) vals$fill <- input$legendFill
    })

    # 5) Expose current settings
    reactive({
      list(
        legendFontSize   = input$legendFontSize %||% vals$size,
        legendFontFamily = input$legendFontFamily %||% vals$family,
        legendFill       = input$legendFill %||% vals$fill
      )
    })
  })
}
