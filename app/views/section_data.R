#' Data section
#'
#' Upload of gene cluster files (CSV or GenBank) and the columns that hold the
#' start, end, cluster, group and strand of each gene.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observeEvent, req, tags, isolate],
  shinyblocks[block_file_input, block_combobox, update_block_select,
              update_block_combobox, block_badge, block_cluster, show_toast],
  ../logic/logic_load_controls[set_inputs_from_columns, load_gene_data],
  ../logic/ui_helpers[field, select_field, field_row, field_group, NONE],
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
    class = "gv-stack",
    block_file_input(
      ns("files"),
      variant = "dropzone",
      multiple = TRUE,
      accept = c(".csv", ".gbk", ".gb"),
      dropzone_label = "Drop CSV or GenBank files here, or click to browse",
      dropzone_hint = "Several GenBank files are read as one cluster each"
    ),
    tags$a(class = "gv-link", href = "gene_cluster_example.csv", target = "_blank",
           "Download an example CSV file"),
    field_group(
      "Columns",
      field_row(
        lapply(c("start", "end"), function(col) {
          select_field(ns(col), COLUMNS[[col]], column_choices(cols), defaults[[col]])
        })
      ),
      field_row(
        lapply(c("cluster", "group", "strand"), function(col) {
          select_field(ns(col), COLUMNS[[col]], column_choices(cols), defaults[[col]])
        })
      ),
      field(ns("order"), "Clusters to show",
            block_combobox(ns("order"), choices = unique(as.character(data$cluster)),
                           multiple = TRUE, placeholder = "All clusters, in data order",
                           search_placeholder = "Search clusters"),
            "Pick clusters to show them in that order.")
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

    observeEvent(input$files, {
      req(input$files)
      tryCatch({
        data <- load_gene_data(input$files)
        r$cluster_data <- data
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
