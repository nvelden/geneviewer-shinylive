#' In-place guidance and shared colour-group column selection
'.__module__.'
box::use(
  shiny[NS, tags, moduleServer, observe, observeEvent, renderUI, uiOutput],
  shinyblocks[block_select, update_block_select],
  ./ui_helpers[field, NONE],
  ./utils[opt]
)

#' @export
column_has_values <- function(data, column) {
  if (length(column) != 1L || is.na(column) || !column %in% names(data)) return(FALSE)
  values <- as.character(data[[column]])
  any(!is.na(values) & nzchar(trimws(values)))
}

#' @export
setup_notice <- function(title, description = NULL, ...) {
  tags$div(class = "gv-setup-notice", role = "status", `aria-live` = "polite",
           tags$strong(title), if (!is.null(description)) tags$p(description), ...)
}

#' @export
setup_action <- function(id, label) {
  tags$button(type = "button", class = "gv-button-outline",
              onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", id),
              label)
}

#' Two views of the Data group mapping; these are not independent preset settings.
#' @export
ui_group_picker <- function(id, data) {
  ns <- NS(id)
  cols <- names(data)
  tags$div(
    class = "gv-group-picker",
    uiOutput(NS("groupSetup")(paste0(id, "Hint"))),
    field(ns("groupColumn"), "Colour genes by",
          block_select(ns("groupColumn"), choices = c("None" = NONE, stats::setNames(cols, cols)),
                       selected = if ("class" %in% cols) "class" else NONE, size = "sm"),
          "Shared with Data, Colors and Legend. Each value defines a colour group and legend entry.",
          stacked = TRUE)
  )
}

#' @export
server_group_setup <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    observe({
      cols <- names(r$cluster_data)
      selected <- r$input[["group"]]
      if (length(selected) != 1L || !selected %in% cols) selected <- NONE
      for (view in c("legend", "colors")) {
        update_block_select(session$rootScope(), paste0(view, "-groupColumn"),
                            choices = c("None" = NONE, stats::setNames(cols, cols)),
                            selected = selected, notify = FALSE)
      }
    })
    # Explicit user events avoid treating initial values and synchronisation
    # updates as edits, including while uploads and examples replace the data.
    observeEvent(input$choice, {
      if (!is.null(r$pending)) return()
      value <- input$choice$column
      if (length(value) != 1L || !value %in% c(NONE, names(r$cluster_data))) return()
      r$input[["group"]] <- opt(value)
      update_block_select(session$rootScope(), "data-group", selected = value)
    })
    for (view in c("legend", "colors")) local({
      view <- view
      output[[paste0(view, "Hint")]] <- renderUI({
        group <- r$input[["group"]]
        if (column_has_values(r$cluster_data, group)) return(NULL)
        empty <- length(group) == 1L && group %in% names(r$cluster_data)
        setup_notice(
          if (empty) "This column has no group values" else if (view == "legend")
            "Choose a column to show a legend" else "Choose a column to colour genes by group",
          if (empty) "Choose another column below, or add group values to your data." else
            "Choose a column below. Genes with the same value share a colour, and those values become legend entries."
        )
      })
    })
  })
}
