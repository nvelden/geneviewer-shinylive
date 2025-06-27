#' Gene label controls module
#'
#' The \code{view_gene_label_controls} module provides the UI and server components
#' for toggling and styling gene labels in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[
    NS, moduleServer, observe, updateSelectInput, reactive,
    checkboxInput, numericInput, selectInput, icon, req,
    textInput, div
  ],
  shinydashboard[menuItem]
)

#' Shiny UI for gene label controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing label control inputs
#' @export
ui_labels <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Gene labels",
    icon    = icon("tags"),
    tabName = "labels",
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showLabels"),
          label   = "Show labels",
          value   = FALSE
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("labelGroup"),
          label   = "Group",
          choices = NULL
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("fontWeight"),
          label   = "Font weight",
          choices = c("normal", "bold"),
          selected = "normal"
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("fontStyle"),
          label   = "Font style",
          choices = c("normal", "italic"),
          selected = "normal"
        )
    ),
    div(style="margin-bottom:-20px;",
        textInput(
          inputId = ns("fill"),
          label   = "Label color",
          value   = "black"
        )
    ),
    div(style="margin-bottom:-20px;",
        selectInput(
          inputId = ns("fontFamily"),
          label   = "Font family",
          choices = c("sans-serif", "serif", "monospace", "cursive", "fantasy"),
          selected = "sans-serif"
        )
    ),
    div(style = "margin-bottom:-20px;",
        numericInput(
          inputId = ns("fontSize"),
          label = "Font size (px)",
          value = 12,
          step  = 1
        )
    ),
    ## extra space at the bottom
    div(style="height:20px;")
  )
}

#' Shiny server for gene label controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list containing cluster_data and inputs
#' @export
server_labels <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(r$cluster_data)
      updateSelectInput(
        session, "labelGroup",
        choices  = names(r$cluster_data),
        selected = names(r$cluster_data)[1]
      )
    })

    reactive({
      list(
        showLabels  = input$showLabels,
        labelGroup = input$labelGroup,
        fontSize = input$fontSize,
        fontStyle = input$fontStyle,
        fontWeight = input$fontWeight,
        fill = input$fill,
        fontFamily = input$fontFamily
      )
    })
  })
}
