# Align controls module
#
# The \code{view_align_controls} module provides the UI and server components
# for specifying parameters for the GC_align function in the Gene Cluster Dashboard.
#' Align controls module
#'
#' The \code{view_align_controls} module provides the UI and server components
#' for specifying the data column, item ID, and alignment for GC_align.
#' @module view_align_controls

box::use(
  shiny[NS, moduleServer, reactive, textInput, selectInput, eventReactive, reactiveVal,
        icon, div, updateSelectInput, req, observe, observeEvent, checkboxInput],
  shinydashboard[menuItem]
)

#' Shiny UI for GC align controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing align control inputs
#' @export
ui_align <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Gene alignment",
    icon    = icon("align-justify"),
    tabName = "align",
    div(style="margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("alignGenes"),
          label   = "Align genes",
          value   = FALSE
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId  = ns("idColumn"),
          label = "Group",
          choices = NULL
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("id"),
          label = "ID",
          choices = NULL
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectInput(
          inputId = ns("align"),
          label   = "Alignment",
          choices = c(
            "Left"   = "left",
            "Center" = "center",
            "Right"  = "right"
          ),
          selected = "left"
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for GC align controls
#'
#' @param id Namespace id for this module
#' @param r  A reactive values list to which the settings will be applied
#' @export
server_align <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    observe({
    req(r$cluster_data)
    updateSelectInput(
        session,
        "idColumn",
        choices = names(r$cluster_data),
        selected = names(r$cluster_data)[1]
      )
    })

    # Populate ID choices based on selected column
    observeEvent(input$idColumn, {
      req(r$cluster_data)

      vals <- unique(r$cluster_data[[input$idColumn]])
      updateSelectInput(
        session,
        "id",
        choices  = unique(r$cluster_data[[input$idColumn]]),
        selected = unique(r$cluster_data[[input$idColumn]])[1]
      )

    }, ignoreInit = TRUE)

    aligned <- reactive({
      req(input$id, input$align, input$idColumn)
      list(
        alignGenes = input$alignGenes,
        idColumn = input$idColumn,
        id = input$id,
        align = input$align
      )
    })

    ## Return that reactive
    aligned

  })
}
