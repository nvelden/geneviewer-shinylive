#' Gene cluster data load module
#'
#' The \code{view_load_controls} module provides the UI and server components
#' for uploading gene cluster data to be used in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, fileInput, reactive, req, div, br, icon, outputOptions,
        observeEvent, showNotification, conditionalPanel, selectInput, selectizeInput, updateSelectizeInput],
  shinydashboard[menuItem],
  ../logic/logic_load_controls[set_inputs_from_columns, synonyms, load_gene_data],
  utils[read.csv],
  stats[setNames]
)

#' Shiny UI for gene cluster data upload
#'
#' @param id Namespace id for this module
#' @return A shinydashboard menuItem containing the file upload input
#' @export
ui_load <- function(id) {
  ns <- NS(id)
  menuItem(
    text    = "Load gene data",
    icon    = icon("upload"),
    tabName = "load",
    div(style = "margin-bottom:-20px;",
        fileInput(
          inputId = ns("geneDataFile"),
          label   = "Upload gene cluster file",
          multiple = TRUE,
          accept  = NULL  # Accept any file type
        )
    ),
    # Add note about supported file types
    div(style = "padding: 0px 15px 10px 15px; font-size: 11px; color: #666;",
        "Supports: CSV and GBK (.gbk, .gb) files"
    ),
    shiny::tags$a(
      href = "gene_cluster_example.csv",
      target = "_blank",
      style = "padding: 5px 15px 10px 15px; display: block;",
      "Download example file"
    ),
    shiny::conditionalPanel(
      condition = "output.fileUploaded",
      ns = ns,
      div(style = "margin-top: 0px;",
          div(style = "margin-bottom: -20px;",
              selectInput(
                inputId = ns("select_start"),
                label   = "Start",
                choices = c("-- None selected --" = " "),
                selected = " "
              )
          ),
          div(style = "margin-bottom: -20px;",
              selectInput(
                inputId = ns("select_end"),
                label   = "End",
                choices = c("-- None selected --" = " "),
                selected = " "
              )
          ),
          div(style = "margin-bottom: -20px;",
              selectInput(
                inputId = ns("select_cluster"),
                label   = "Cluster",
                choices = c("-- None selected --" = " "),
                selected = " "
              )
          ),
          div(style = "margin-bottom: -20px;",
              shiny::conditionalPanel(
                condition ="input['select_cluster'] !== ' '",
                ns = ns,
                selectizeInput(
                  inputId = ns("select_order"),
                  label   = "Select clusters",
                  multiple = TRUE,
                  choices = NULL,
                  options = list(placeholder = 'Select a cluster value')
                )
              )
          ),
          div(style = "margin-bottom: -20px;",
              selectInput(
                inputId = ns("select_group"),
                label   = "Group",
                choices = c("-- None selected --" = " "),
                selected = " "
              )
          ),
          div(style = "margin-bottom: -20px;",
              selectInput(
                inputId = ns("select_strand"),
                label   = "Strand",
                choices = c("-- None selected --" = " "),
                selected = " "
              )
          ),
          div(style = "height: 20px;")
      )
    )
  )
}

#' Shiny server logic for gene cluster data upload
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list where cluster data will be stored
#' @export
#' Shiny server logic for gene cluster data upload
#'
#' @param id Namespace id for this module
#' @param r A reactiveValues list where cluster data will be stored
#' @export
server_load <- function(id, r = r) {
  moduleServer(id, function(input, output, session) {

    # Output for conditionalPanel
    output$fileUploaded <- reactive({
      !is.null(input$geneDataFile)
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

    observeEvent(input$geneDataFile, {
      req(input$geneDataFile)

      tryCatch({

        r$cluster_data <- load_gene_data(input$geneDataFile)

        r$input[["cluster"]] <- NULL

        set_inputs_from_columns(
          data = r$cluster_data,
          r = r,
          input_names = c("start", "end", "cluster", "group", "strand")
        )

        # Update selectInputs
        colnames_available <- colnames(r$cluster_data)

        for (input_name in c("start", "end", "cluster", "group", "strand")) {

          # Clear selections
          shiny::updateSelectInput(
            session,
            paste0("select_", input_name),
            choices = NULL,
            selected = NULL
          )

          shiny::updateSelectInput(
            session,
            paste0("select_", input_name),
            choices = c("-- None selected --" = " ", setNames(colnames_available, colnames_available)),
            selected = if (!is.null(r$input[[input_name]])) r$input[[input_name]] else " "
          )
        }

        # Check column exists
        if (!is.null(r$cluster_data) && r$input[["cluster"]] %in% names(r$cluster_data)) {
          clusters <- unique(r$cluster_data[[r$input[["cluster"]]]])
        } else {
          clusters <- NULL
        }

        # Update selectizeInput
        updateSelectizeInput(
          session,
          "select_order",
          choices = NULL,
          selected = NULL,
          server = TRUE
        )

        updateSelectizeInput(
          session,
          "select_order",
          choices = clusters,
          selected = NULL,
          server = TRUE
        )

      },
      error = function(e) {
        showNotification(
          paste("Error loading gene cluster data:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      },
      warning = function(w) {
        showNotification(
          paste("Warning:", conditionMessage(w)),
          type = "warning",
          duration = 5
        )
        invokeRestart("muffleWarning")
      })
    })

    # Observe Start select input
    observeEvent(input$select_start, {
      r$input[["start"]] <- if (!is.null(input$select_start) && nzchar(trimws(input$select_start))) input$select_start else NULL
    }, ignoreInit = TRUE)

    # Observe End select input
    observeEvent(input$select_end, {
      r$input[["end"]] <- if (!is.null(input$select_end) && nzchar(trimws(input$select_end))) input$select_end else NULL
    }, ignoreInit = TRUE)

    observeEvent(input$select_cluster, {
      if (!is.null(input$select_cluster) && nzchar(trimws(input$select_cluster)) && input$select_cluster != " ") {

        # Update stored value
        r$input[["cluster"]] <- input$select_cluster

        # Clear select_order
        updateSelectizeInput(
          session,
          "select_order",
          selected = NULL,
          server = TRUE
        )

        # Load new cluster values
        if (!is.null(r$cluster_data) && r$input[["cluster"]] %in% names(r$cluster_data)) {
          clusters <- unique(r$cluster_data[[r$input[["cluster"]]]])
        } else {
          clusters <- " "
        }

        # Update with new cluster values
        updateSelectizeInput(
          session,
          "select_order",
          choices = clusters,
          selected = NULL,
          server = TRUE
        )

      } else {
        # Clear stored value
        r$input[["cluster"]] <- NULL

        # Clear select_order if cluster deselected
        updateSelectizeInput(
          session,
          "select_order",
          choices = character(0),
          selected = character(0),
          server = TRUE
        )
      }
    }, ignoreInit = TRUE)

    # Observe Group select input
    observeEvent(input$select_group, {
      r$input[["group"]] <- if (!is.null(input$select_group) && nzchar(trimws(input$select_group))) input$select_group else NULL
    }, ignoreInit = TRUE)

    # Observe Strand select input
    observeEvent(input$select_strand, {
      r$input[["strand"]] <- if (!is.null(input$select_strand) && nzchar(trimws(input$select_strand))) input$select_strand else NULL
    }, ignoreInit = TRUE)

    observeEvent(input$select_order, {
      r$input[["select_order"]] <-
        if (!is.null(input$select_order) && length(input$select_order) > 0) {
          input$select_order
        } else {
          NULL
        }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

  })
}
