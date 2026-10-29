#' Data section
#'
#' Upload of gene cluster files (CSV or GenBank) and the columns that hold the
#' start, end, cluster, group and strand of each gene.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observeEvent, req, tags, uiOutput, renderUI],
  shinyblocks[block_file_input, block_combobox, update_block_select,
              update_block_combobox, update_block_file_input, block_icon, show_toast],
  ../logic/logic_load_controls[detect_columns, load_gene_data],
  ../logic/ui_helpers[field, select_field, field_group, register_input, NONE],
  ../logic/utils[opt, pick_choice],
  ../logic/setup[setup_notice],
  ../logic/examples[EXAMPLES]
)

COLUMNS <- c(start = "Start", end = "End", cluster = "Cluster", group = "Group", strand = "Strand")

#' @export
column_choices <- function(cols) c("None" = NONE, stats::setNames(cols, cols))

#' The examples to offer: all but the one that is loaded
#' @export
example_choices <- function(loaded = NULL) {
  keys <- setdiff(names(EXAMPLES), loaded)
  stats::setNames(keys, vapply(EXAMPLES[keys], `[[`, "", "label"))
}

#' @export
ui_data <- function(id, data) {
  ns <- NS(id)
  cols <- names(data)
  defaults <- list(start = "start", end = "end", cluster = "cluster", group = "class", strand = NONE)
  register_input(ns("order"), "combobox")

  tags$div(
    class = "gv-sections",
    tags$section(
      class = "gv-group",
      tags$h4(class = "gv-group-title", "Dataset"),
      uiOutput(ns("summary")),
      shinyblocks::block_select(ns("example"), example_choices("ophA"), selected = NULL,
                                placeholder = "Load an example...", size = "sm"),
      block_file_input(
        ns("files"),
        variant = "dropzone",
        multiple = TRUE,
        append = TRUE,
        accept = c(".csv", ".gbk", ".gb", ".json"),
        dropzone_icon = "upload",
        dropzone_label = "Drop files or click to upload",
        dropzone_hint = paste("CSV, or GenBank files with one cluster each. Drop more to add them.",
                              "Drop a saved project (.json) to open it.")
      ),
      tags$div(
        class = "gv-dataset-actions",
        tags$a(class = "gv-button-outline", href = "gene_cluster_example.csv", download = NA,
               block_icon("download", size = "sm"), "Example CSV"),
        tags$button(type = "button", class = "gv-button-outline",
                    title = "Save the data, settings and edits, to open the chart again later",
                    onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
                                      ns("saveProject")),
                    block_icon("save", size = "sm"), "Save project")
      )
    ),
    field_group(
      "Columns",
      uiOutput(ns("setup")),
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
server_data <- function(id, r, toaster_id, load_dataset, open_project) {
  moduleServer(id, function(input, output, session) {

    # Offer the examples that are not loaded; uploaded data offers all
    observeEvent(r$example, {
      update_block_select(session, "example", choices = example_choices(r$example),
                          selected = NULL)
      if (!identical(r$example, "upload")) update_block_file_input(session, "files", reset = TRUE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # The clusters of the cluster column; while a load is pending, its order
    observeEvent(input$cluster, {
      col <- opt(input$cluster)
      clusters <- if (!is.null(col) && col %in% names(r$cluster_data)) {
        unique(as.character(r$cluster_data[[col]]))
      } else {
        character(0)
      }
      # A combobox needs at least one option: without clusters, clear the selection
      if (length(clusters) > 0) {
        update_block_combobox(session, "order", choices = clusters,
                              selected = pick_choice(r, session$ns("order"), input$order,
                                                     clusters, character(0)))
      } else {
        update_block_combobox(session, "order", selected = character(0))
      }
    }, ignoreInit = TRUE)

    output$setup <- renderUI({
      missing <- c("start", "end")[!vapply(c("start", "end"), function(key) {
        col <- opt(input[[key]])
        length(col) == 1L && col %in% names(r$cluster_data)
      }, logical(1))]
      if (length(missing)) setup_notice(
        "Choose the gene position columns",
        paste0("Select ", paste(missing, collapse = " and "),
               " columns below so genes can be placed on the chart. Values should be positions in base pairs."))
    })
    output$summary <- renderUI({
      data <- r$cluster_data
      col <- opt(input$cluster)
      n_clusters <- if (!is.null(col) && col %in% names(data)) length(unique(data[[col]])) else 1
      tags$div(
        class = "gv-dataset",
        tags$div(class = "gv-dataset-icon", block_icon("database", size = "sm")),
        tags$div(
          class = "gv-dataset-text",
          tags$div(class = "gv-dataset-name", r$source_name %||% EXAMPLES$ophA$label),
          tags$div(class = "gv-dataset-meta",
                   sprintf("%d cluster%s \u00b7 %d genes \u00b7 %d columns",
                           n_clusters, if (n_clusters == 1) "" else "s",
                           nrow(data), ncol(data)))
        )
      )
    })

    observeEvent(input$files, {
      req(input$files)
      # A project replaces the chart; the last one dropped is opened
      projects <- tolower(tools::file_ext(input$files$name)) == "json"
      if (any(projects)) {
        file <- input$files[max(which(projects)), ]
        update_block_file_input(session, "files", reset = TRUE)
        tryCatch({
          open_project(file$datapath, file$name)
          show_toast(session = session$rootScope(), toaster_id = toaster_id,
                     title = "Project opened", description = file$name,
                     variant = "success", icon = "check-circle")
        }, error = function(e) {
          show_toast(session = session$rootScope(), toaster_id = toaster_id,
                     title = "Could not open the project", description = conditionMessage(e),
                     variant = "destructive", icon = "alert-circle", duration = 8000)
        })
        return()
      }
      tryCatch({
        data <- load_gene_data(input$files)
        # Files dropped on an upload are added to it (the input appends), and
        # keep the user's settings; a first upload starts a new chart
        keep <- identical(r$example, "upload")
        columns <- detect_columns(data, names(COLUMNS))
        for (col in names(COLUMNS)) {
          if (keep && isTRUE(input[[col]] %in% c(NONE, names(data)))) columns[[col]] <- input[[col]]
        }
        values <- stats::setNames(lapply(columns, `%||%`, NONE), paste0("data-", names(COLUMNS)))
        if (!keep) values[["tooltip-columns"]] <- unlist(columns[c("start", "end")])

        r$link_data <- NULL
        r$link_code <- NULL
        r$link_name <- NULL
        r$source_files <- input$files$name
        r$example <- "upload"
        r$source_name <- if (nrow(input$files) == 1) {
          input$files$name
        } else {
          sprintf("%s and %d more", input$files$name[1], nrow(input$files) - 1)
        }
        load_dataset(data, values, keep = keep)

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
  })
}
