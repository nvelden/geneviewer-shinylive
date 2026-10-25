#' Data section
#'
#' Upload of gene cluster files (CSV or GenBank) and the columns that hold the
#' start, end, cluster, group and strand of each gene.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observeEvent, req, tags, uiOutput, renderUI],
  shinyblocks[block_file_input, block_combobox, update_block_select,
              update_block_combobox, block_icon, show_toast],
  ../logic/logic_load_controls[set_inputs_from_columns, load_gene_data],
  ../logic/ui_helpers[field, select_field, field_group, NONE],
  ../logic/utils[opt]
)

COLUMNS <- c(start = "Start", end = "End", cluster = "Cluster", group = "Group", strand = "Strand")

column_choices <- function(cols) c("None" = NONE, stats::setNames(cols, cols))

#' @export
ui_data <- function(id, data) {
  ns <- NS(id)
  cols <- names(data)
  defaults <- list(start = "start", end = "end", cluster = "cluster", group = "class", strand = NONE)

  tags$div(
    class = "gv-sections",
    tags$section(
      class = "gv-group",
      tags$h4(class = "gv-group-title", "Dataset"),
      uiOutput(ns("summary")),
      block_file_input(
        ns("files"),
        variant = "dropzone",
        multiple = TRUE,
        accept = c(".csv", ".gbk", ".gb"),
        dropzone_icon = "upload",
        dropzone_label = "Drop files or click to upload",
        dropzone_hint = "CSV, or GenBank files with one cluster each"
      ),
      tags$a(class = "gv-button-outline", href = "gene_cluster_example.csv", download = NA,
             block_icon("download", size = "sm"), "Example CSV")
    ),
    field_group(
      "Columns",
      lapply(names(COLUMNS), function(col) {
        select_field(ns(col), COLUMNS[[col]], column_choices(cols), defaults[[col]])
      })
    ),
    field_group(
      "Clusters",
      field(ns("order"), "Show and order",
            block_combobox(ns("order"), choices = unique(as.character(data$cluster)),
                           multiple = TRUE, placeholder = "All clusters",
                           search_placeholder = "Search clusters", size = "sm"),
            "Pick clusters to show only those, in that order.", stacked = TRUE)
    )
  )
}

#' @export
server_data <- function(id, r, toaster_id) {
  moduleServer(id, function(input, output, session) {

    update_cluster_choices <- function() {
      col <- r$input[["cluster"]]
      clusters <- if (!is.null(col) && col %in% names(r$cluster_data)) {
        unique(as.character(r$cluster_data[[col]]))
      } else {
        character(0)
      }
      update_block_combobox(session, "order", choices = clusters, selected = character(0))
    }

    output$summary <- renderUI({
      data <- r$cluster_data
      col <- r$input[["cluster"]]
      n_clusters <- if (!is.null(col) && col %in% names(data)) length(unique(data[[col]])) else 1
      tags$div(
        class = "gv-dataset",
        tags$div(class = "gv-dataset-icon", block_icon("database", size = "sm")),
        tags$div(
          class = "gv-dataset-text",
          tags$div(class = "gv-dataset-name", r$source_name %||% "Example: ophA clusters"),
          tags$div(class = "gv-dataset-meta",
                   sprintf("%d cluster%s \u00b7 %d genes \u00b7 %d columns",
                           n_clusters, if (n_clusters == 1) "" else "s",
                           nrow(data), ncol(data)))
        )
      )
    })

    observeEvent(input$files, {
      req(input$files)
      tryCatch({
        data <- load_gene_data(input$files)
        r$cluster_data <- data
        r$source_name <- if (nrow(input$files) == 1) {
          input$files$name
        } else {
          sprintf("%s and %d more", input$files$name[1], nrow(input$files) - 1)
        }
        r$input[["select_order"]] <- NULL
        set_inputs_from_columns(data, r, names(COLUMNS))

        for (col in names(COLUMNS)) {
          update_block_select(
            session, col,
            choices = column_choices(names(data)),
            selected = r$input[[col]] %||% NONE
          )
        }
        update_cluster_choices()

        # The toaster is on the page, outside this module
        show_toast(
          session = session$rootScope(),
          toaster_id = toaster_id,
          title = "Data loaded",
          description = sprintf("%d genes from %d file%s", nrow(data), nrow(input$files),
                                if (nrow(input$files) == 1) "" else "s"),
          variant = "success", icon = "check-circle"
        )
      },
      error = function(e) {
        show_toast(session = session$rootScope(), toaster_id = toaster_id,
                   title = "Could not load the data",
                   description = conditionMessage(e), variant = "destructive",
                   icon = "alert-circle", duration = 8000)
      })
    })

    for (col in names(COLUMNS)) {
      local({
        col <- col
        observeEvent(input[[col]], {
          r$input[[col]] <- opt(input[[col]])
          if (col == "cluster") update_cluster_choices()
        }, ignoreInit = TRUE)
      })
    }

    observeEvent(input$order, {
      r$input[["select_order"]] <- if (length(input$order) > 0) input$order else NULL
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })
}
