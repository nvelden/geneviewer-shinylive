#' Gene cluster data load module
#'
#' The \code{view_load_controls} module provides the UI and server components
#' for uploading gene cluster data to be used in the Gene Cluster Dashboard.
'.__module__.'

box::use(
  shiny[NS, moduleServer, fileInput, reactive, req, div, icon, outputOptions,
        observeEvent, showNotification, conditionalPanel, selectInput],
  shinydashboard[menuItem],
  ../logic/logic_load_controls[set_inputs_from_columns, synonyms],
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
          accept  = NULL  # Accept any file type
        )
    ),
    shiny::conditionalPanel(
      condition = "output.fileUploaded",
      ns = ns,
      div(style = "margin-top: 10px;",

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

    observeEvent(input$geneDataFile, {
      req(input$geneDataFile)
      tryCatch({

        r$cluster_data <- read.csv(input$geneDataFile$datapath, stringsAsFactors = FALSE)

        set_inputs_from_columns(
          data = r$cluster_data,
          r = r,
          input_names = c("start", "end", "cluster", "group", "strand")
        )

        # Output for conditionalPanel
        output$fileUploaded <- reactive({
          !is.null(input$geneDataFile)
        })
        outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

        # Update selectInputs
        colnames_available <- colnames(r$cluster_data)

        for (input_name in c("start", "end", "cluster", "group", "strand")) {
          shiny::updateSelectInput(
            session,
            paste0("select_", input_name),
            choices = c("-- None selected --" = " ", setNames(colnames_available, colnames_available)),
            selected = if (!is.null(r$input[[input_name]])) r$input[[input_name]] else " "
          )
        }
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

    # Observe Cluster select input
    observeEvent(input$select_cluster, {
      r$input[["cluster"]] <- if (!is.null(input$select_cluster) && nzchar(trimws(input$select_cluster))) input$select_cluster else NULL
    }, ignoreInit = TRUE)

    # Observe Group select input
    observeEvent(input$select_group, {
      r$input[["group"]] <- if (!is.null(input$select_group) && nzchar(trimws(input$select_group))) input$select_group else NULL
    }, ignoreInit = TRUE)

    # Observe Strand select input
    observeEvent(input$select_strand, {
      r$input[["strand"]] <- if (!is.null(input$select_strand) && nzchar(trimws(input$select_strand))) input$select_strand else NULL
    }, ignoreInit = TRUE)

  })
}
