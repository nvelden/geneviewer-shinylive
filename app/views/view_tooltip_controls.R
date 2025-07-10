#' Tooltip controls module
#'
#' The \code{view_tooltip_controls} module provides the UI and server components
#' for controlling the tooltip display in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, div, icon, checkboxInput, selectizeInput,
        updateSelectizeInput, observeEvent, reactive, observe, req],
  shinydashboard[menuItem]
)

#' Shiny UI for tooltip controls
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing tooltip control inputs
#' @export
ui_tooltip <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Tooltip",
    icon    = icon("info-circle"),
    tabName = "tooltip",
    div(style = "margin-bottom:-20px;",
        checkboxInput(
          inputId = ns("showTooltip"),
          label   = "Show tooltip",
          value   = TRUE
        )
    ),
    div(style = "margin-bottom:-20px;",
        selectizeInput(
          inputId = ns("tooltipColumns"),
          label = "Show in tooltip",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select values")
        )
    ),
    div(style = "height:20px;")
  )
}

#' Shiny server for tooltip controls
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list to store inputs
#' @export
server_tooltip <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {

    # Update choices when data is loaded
    observe({
      req(r$cluster_data)

      selected_cols <- Filter(
        function(x) !is.null(x) && nzchar(x),
        c(r$input[["start"]], r$input[["end"]])
      )

      updateSelectizeInput(
        session,
        "tooltipColumns",
        choices  = names(r$cluster_data),
        selected = selected_cols,
        server   = TRUE
      )
    })

    # Watch for selection changes
    observeEvent(input$tooltipColumns, {
      r$tooltipColumns <- input$tooltipColumns
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Watch for checkbox changes
    observeEvent(input$showTooltip, {
      r$showTooltip <- input$showTooltip
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Return a reactive list with the tooltip configuration
    tooltip_config <- reactive({
      cols <- input$tooltipColumns
      show <- input$showTooltip

      formatter <- NULL
      if (isTRUE(show) && !is.null(cols) && length(cols) > 0) {
        formatter <- paste(
          sprintf("<b>%s:</b> {%s}", cols, cols),
          collapse = "<br>"
        )
      }

      list(
        show = show,
        columns = cols,
        formatter = formatter
      )
    })

    return(tooltip_config)
  })
}
