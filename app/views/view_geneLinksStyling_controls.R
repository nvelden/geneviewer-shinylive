#' Links Styling controls module
#'
#' The \code{linksStyling_controls} module provides the UI and server components
#' for controlling GC_links styling in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, selectInput, textInput, checkboxInput,
        numericInput, uiOutput, renderUI, div, observeEvent,
        actionLink, reactiveVal, reactiveValues, isolate, fluidRow, column,
        h4, hr, tabsetPanel, tabPanel],
  shinydashboardPlus[box],
  shinyjqui[jqui_draggable]
)

#' Shiny UI for links styling box
#'
#' @param id Namespace id for this module
#' @return A UI output placeholder that renders the styling box
#' @export
ui_geneLinksStyling <- function(id) {
  ns <- NS(id)
  div(
    id = ns("linksStylingPlaceholder"),
    style = "position: absolute; z-index:100; width: calc(35vw); top:calc(45vh);",
    uiOutput(ns("styleBoxUI"))
  )
}

#' Shiny server for Links Styling controls
#'
#' @param id Namespace id for this module
#' @export
server_geneLinksStyling <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Persist the last chosen values (used when reopening)
    vals <- reactiveValues(
      # Link appearance
      normalColor = "#969696",
      invertedColor = "#d62728",
      curve = TRUE,
      linkWidth = 1,
      useGroupColors = FALSE,

      # Link style
      linkStroke = "black",
      linkStrokeWidth = 0.5,
      linkOpacity = 1
    )

    # 2) Track visibility (start hidden)
    boxVisible <- reactiveVal(FALSE)
    observeEvent(r$links$Styling, {
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
          title = actionLink(ns("optionsTitle"), "Links Styling"),
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

            # Link Appearance Tab
            tabPanel(
              "Link Appearance",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           checkboxInput(
                             inputId = ns("curve"),
                             label = "Curved Links",
                             value = isolate(vals$curve)
                           )
                    ),
                    column(6,
                           checkboxInput(
                             inputId = ns("useGroupColors"),
                             label = "Use Group Colors",
                             value = isolate(vals$useGroupColors)
                           )
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("normalColor"),
                             label = "Normal Color",
                             value = isolate(vals$normalColor)
                           )
                    ),
                    column(6,
                           textInput(
                             inputId = ns("invertedColor"),
                             label = "Inverted Color",
                             value = isolate(vals$invertedColor)
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("linkWidth"),
                             label = "Link Width",
                             value = isolate(vals$linkWidth),
                             min = 0,
                             max = 1,
                             step = 0.1
                           )
                    )
                  )
              )
            ),

            # Link Style Tab
            tabPanel(
              "Link Style",
              div(style = "padding: 10px;",
                  fluidRow(
                    column(6,
                           textInput(
                             inputId = ns("linkStroke"),
                             label = "Stroke Color",
                             value = isolate(vals$linkStroke)
                           )
                    ),
                    column(6,
                           numericInput(
                             inputId = ns("linkStrokeWidth"),
                             label = "Stroke Width",
                             value = isolate(vals$linkStrokeWidth),
                             min = 0,
                             step = 0.1
                           )
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput(
                             inputId = ns("linkOpacity"),
                             label = "Link Opacity",
                             value = isolate(vals$linkOpacity),
                             min = 0,
                             max = 1,
                             step = 0.1
                           )
                    )
                  )
              )
            )
          )
        ),
        options = list(
          stack = ".controlBox",
          scroll = FALSE,
          containment = "window"
        )
      )
    })

    # 4) Observers to update reactive values when inputs change
    observeEvent(input$normalColor, { vals$normalColor <- input$normalColor })
    observeEvent(input$invertedColor, { vals$invertedColor <- input$invertedColor })
    observeEvent(input$curve, { vals$curve <- input$curve })
    observeEvent(input$linkWidth, { vals$linkWidth <- input$linkWidth })
    observeEvent(input$useGroupColors, { vals$useGroupColors <- input$useGroupColors })

    observeEvent(input$linkStroke, { vals$linkStroke <- input$linkStroke })
    observeEvent(input$linkStrokeWidth, { vals$linkStrokeWidth <- input$linkStrokeWidth })
    observeEvent(input$linkOpacity, { vals$linkOpacity <- input$linkOpacity })

    # 5) Return reactive values for use in main app
    reactive({
      list(
        # Link appearance
        normal_color = vals$normalColor,
        inverted_color = vals$invertedColor,
        curve = vals$curve,
        linkWidth = vals$linkWidth,
        use_group_colors = vals$useGroupColors,

        # Styles
        linkStyle = list(
          stroke = vals$linkStroke,
          strokeWidth = vals$linkStrokeWidth,
          opacity = vals$linkOpacity
        )
      )
    })

  })
}
