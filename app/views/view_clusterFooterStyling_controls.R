#' Legend Styling controls module
#'
#' The \code{legendStyling_controls} module provides the UI and server components
#' for controlling legend font and fill styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_clusterFooterStyling <- function(id) {
  ns <- NS(id)
    div(
      id = ns("clusterFooterPlaceholder"),
      style = "position: absolute; z-index:100; width: calc(30vw); top:calc(60vh);",
      uiOutput(ns("styleBoxUI"))
    )

}

#' Shiny server for Legend Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_clusterFooterStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Footer styling values
      footerSubtitle = "",
      height = "40px",
      x = 0,
      y = 5,
      align = "center",
      spacing = 20,
      footerFontSize = 14,
      footerFontWeight = "normal",
      footerFontFamily = "sans-serif",
      footerColor = "black",
      subtitleFontSize = 12,
      subtitleFontWeight = "normal",
      subtitleFontFamily = "sans-serif",
      subtitleColor = "grey"
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$clusterFooter$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Footer Styling"),
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
                             inputId = ns("height"),
                             label = "Height",
                             value = isolate(vals$height)
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("align"),
                             label = "Alignment",
                             choices = c("left", "center", "right"),
                             selected = isolate(vals$align)
                           )
                    )
                  ),
                  fluidRow(
                    column(4,
                           numericInput(
                             inputId = ns("x"),
                             label = "X",
                             value = isolate(vals$x),
                             step = 1
                           )
                    ),
                    column(4,
                           numericInput(
                             inputId = ns("y"),
                             label = "Y",
                             value = isolate(vals$y),
                             step = 1
                           )
                    ),
                    column(4,
                           numericInput(
                             inputId = ns("spacing"),
                             label = "Spacing",
                             value = isolate(vals$spacing),
                             min = 0,
                             step = 1
                           )
                    )
                  )
              )
            ),

            # Footer Font Tab
            tabPanel(
              "Footer Font",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("footerFontSize"),
                             label = "Size (px)",
                             value = isolate(vals$footerFontSize),
                             min = 8,
                             max = 48,
                             step = 1
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("footerFontWeight"),
                             label = "Weight",
                             choices = c("normal", "bold", "lighter", "bolder"),
                             selected = isolate(vals$footerFontWeight)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("footerFontFamily"),
                             label = "Family",
                             choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
                             selected = isolate(vals$footerFontFamily)
                           )
                    ),
                    column(6,
                           textInput(
                             inputId = ns("footerColor"),
                             label = "Color",
                             value = isolate(vals$footerColor)
                           )
                    )
                  )
              )
            ),

            # Subtitle Font Tab
            tabPanel(
              "Subtitle Font",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("subtitleFontSize"),
                             label = "Size (px)",
                             value = isolate(vals$subtitleFontSize),
                             min = 8,
                             max = 48,
                             step = 1
                           )
                    ),
                    column(6,
                           selectInput(
                             inputId = ns("subtitleFontWeight"),
                             label = "Weight",
                             choices = c("normal", "bold", "lighter", "bolder"),
                             selected = isolate(vals$subtitleFontWeight)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           selectInput(
                             inputId = ns("subtitleFontFamily"),
                             label = "Family",
                             choices = c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman"),
                             selected = isolate(vals$subtitleFontFamily)
                           )
                    ),
                    column(6,
                           textInput(
                             inputId = ns("subtitleColor"),
                             label = "Color",
                             value = isolate(vals$subtitleColor)
                           )
                    )
                  )
              )
            )
          )
        ),
        options = list(
          cancel = ".form-control, .selectize-control, input, textarea, button, .btn, .shiny-input-container"
        ),
      )
    })

    # 4) Keep the cache updated as the user changes inputs
    # Footer inputs
    observeEvent(input$height, ignoreInit = TRUE, {
      if (!is.null(input$height)) vals$height <- input$height
    })
    observeEvent(input$x, ignoreInit = TRUE, {
      if (!is.null(input$x)) vals$x <- input$x
    })
    observeEvent(input$y, ignoreInit = TRUE, {
      if (!is.null(input$y)) vals$y <- input$y
    })
    observeEvent(input$align, ignoreInit = TRUE, {
      if (!is.null(input$align)) vals$align <- input$align
    })
    observeEvent(input$spacing, ignoreInit = TRUE, {
      if (!is.null(input$spacing)) vals$spacing <- input$spacing
    })
    observeEvent(input$footerFontSize, ignoreInit = TRUE, {
      if (!is.null(input$footerFontSize)) vals$footerFontSize <- input$footerFontSize
    })
    observeEvent(input$footerFontWeight, ignoreInit = TRUE, {
      if (!is.null(input$footerFontWeight)) vals$footerFontWeight <- input$footerFontWeight
    })
    observeEvent(input$footerFontFamily, ignoreInit = TRUE, {
      if (!is.null(input$footerFontFamily)) vals$footerFontFamily <- input$footerFontFamily
    })
    observeEvent(input$footerColor, ignoreInit = TRUE, {
      if (!is.null(input$footerColor)) vals$footerColor <- input$footerColor
    })
    observeEvent(input$subtitleFontSize, ignoreInit = TRUE, {
      if (!is.null(input$subtitleFontSize)) vals$subtitleFontSize <- input$subtitleFontSize
    })
    observeEvent(input$subtitleFontWeight, ignoreInit = TRUE, {
      if (!is.null(input$subtitleFontWeight)) vals$subtitleFontWeight <- input$subtitleFontWeight
    })
    observeEvent(input$subtitleFontFamily, ignoreInit = TRUE, {
      if (!is.null(input$subtitleFontFamily)) vals$subtitleFontFamily <- input$subtitleFontFamily
    })
    observeEvent(input$subtitleColor, ignoreInit = TRUE, {
      if (!is.null(input$subtitleColor)) vals$subtitleColor <- input$subtitleColor
    })

    # 5) Expose current settings - footer settings only
    reactive({
      list(
        # Footer settings
        height       = input$height %||% vals$height,
        x            = input$x %||% vals$x,
        y            = input$y %||% vals$y,
        align        = input$align %||% vals$align,
        spacing      = input$spacing %||% vals$spacing,
        titleFont    = list(
          fontSize      = paste0(input$footerFontSize %||% vals$footerFontSize, "px"),
          fontWeight    = input$footerFontWeight %||% vals$footerFontWeight,
          fontFamily    = input$footerFontFamily %||% vals$footerFontFamily,
          fill          = input$footerColor %||% vals$footerColor
        ),
        subtitleFont = list(
          fontSize      = paste0(input$subtitleFontSize %||% vals$subtitleFontSize, "px"),
          fontWeight    = input$subtitleFontWeight %||% vals$subtitleFontWeight,
          fontFamily    = input$subtitleFontFamily %||% vals$subtitleFontFamily,
          fill          = input$subtitleColor %||% vals$subtitleColor
        )
      )
    })
  })
}
