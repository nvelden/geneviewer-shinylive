#' Gene links, alignment, tooltip and size sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observe, observeEvent, isolate, req, tags,
        conditionalPanel, uiOutput, renderUI, renderText, textOutput, outputOptions],
  shinyblocks[block_combobox, update_block_select, update_block_combobox,
              block_file_input, update_block_file_input, show_toast],
  ../logic/ui_helpers[color_field, field, text_field, num_field, select_field, switch_field,
                      textarea_field, field_group, sections, when_on, register_input,
                      ALIGNMENTS, NONE],
  ../logic/setup[setup_notice, setup_action, column_has_values],
  ../logic/utils[num, txt, compact, parse_delimited_input, pick_choice],
  ../logic/alignment_data[validate_alignment]
)

#' @export
ui_links <- function(id, data) {
  ns <- NS(id)
  matching <- function(...) conditionalPanel(
    sprintf("input['%s'] !== 'alignment'", ns("source")), ...)
  alignment <- function(...) conditionalPanel(
    sprintf("input['%s'] === 'alignment'", ns("source")), ...)
  custom_colors <- function(...) conditionalPanel(
    sprintf("input['%s'] === 'alignment' || !input['%s']", ns("source"), ns("useGroupColors")), ...)
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Link regions between clusters"),
      when_on(ns, "show",
        select_field(ns("source"), "Link source",
                     c("Matching gene values" = "matching", "Alignment blocks" = "alignment"), "matching"),
        uiOutput(ns("setup")),
        matching(
          select_field(ns("group"), "Match on", names(data), "class",
                       description = "Genes with the same value in this column are linked."),
          text_field(ns("value1"), "Values (first)", placeholder = "gene1, gene2, ...", stacked = TRUE),
          text_field(ns("value2"), "Values (second)", placeholder = "gene3, gene4, ...", stacked = TRUE)
        ),
        alignment(
          uiOutput(ns("alignmentSummary")),
          block_file_input(ns("alignmentFile"), accept = ".csv", multiple = FALSE),
          tags$p(class = "gv-switch-description",
                 "CSV columns: cluster1, start1, end1, cluster2, start2, end2. Optional identity or similarity (%). Keep start > end for inverted blocks.")
        )
      )
    ),
    when_on(ns, "show",
      field_group(
        "Measure",
        select_field(ns("measure"), "Color by",
                     c("Identity (%)" = "identity", "Similarity (%)" = "similarity", "Strand only" = "none"), "identity"),
        matching(switch_field(ns("label"), "Show measure labels", TRUE)),
        tags$div(style = "display:none", textOutput(ns("colorBarAvailable"))),
        custom_colors(conditionalPanel(
          sprintf("output['%s'] === 'true'", ns("colorBarAvailable")),
          select_field(ns("colorBar"), "Color bar", c("Show" = "show", "Hide" = "hide"),
                       default = "show")))
      ),
      field_group(
        "Colors",
        matching(switch_field(ns("useGroupColors"), "Use the gene colors",
          description = "When on, link fills follow the gene colors set under Colors. Turn off to set strand and measure colors here.")),
        custom_colors(
          color_field(ns("normalColor"), "Same strand", default = "#969696"),
          color_field(ns("invertedColor"), "Opposite strand", default = "#d62728"),
          num_field(ns("vmin"), "Min measure (%)", min = 0, max = 100, step = 1, default = 0),
          num_field(ns("vmax"), "Max measure (%)", min = 0, max = 100, step = 1, default = 100)
        )
      ),
      field_group(
        "Shape",
        switch_field(ns("curve"), "Curved links", TRUE),
        num_field(ns("linkWidth"), "Width", min = 0, max = 1, step = 0.1, default = 1,
                  description = "Share of the region length, from 0 to 1."),
        num_field(ns("opacity"), "Opacity", min = 0, max = 1, step = 0.1, default = 0.8),
        color_field(ns("stroke"), "Border color", default = "none"),
        num_field(ns("strokeWidth"), "Border width", min = 0, step = 0.1, default = 1)
      )
    )
  )
}

#' @export
server_links <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(r$cluster_data, isolate({
      req(r$cluster_data)
      cols <- names(r$cluster_data)
      update_block_select(session, "group", choices = cols,
                          selected = pick_choice(r, session$ns("group"), input$group, cols, cols[1]))
      update_block_file_input(session, "alignmentFile", reset = TRUE)
    }))
    has_measure <- reactive({
      measure <- input$measure %||% "identity"
      data <- if (identical(input$source, "alignment")) r$link_data else r$cluster_data
      if (measure == "none" || is.null(data) || !measure %in% names(data)) return(FALSE)
      values <- suppressWarnings(as.numeric(as.character(data[[measure]])))
      any(is.finite(values))
    })
    output$colorBarAvailable <- renderText(if (has_measure()) "true" else "false")
    outputOptions(output, "colorBarAvailable", suspendWhenHidden = FALSE)
    output$alignmentSummary <- renderUI({
      if (is.null(r$link_data)) tags$p("No alignment loaded.") else {
        tags$p(sprintf("%d blocks: %s", nrow(r$link_data), r$link_name))
      }
    })
    observeEvent(input$alignmentFile, {
      if (is.null(input$alignmentFile)) {
        # A manual clear removes uploaded data, but a reset while loading a
        # preset must not remove its built-in alignment.
        if (is.call(r$link_code)) {
          r$link_data <- NULL
          r$link_code <- NULL
          r$link_name <- NULL
        }
        return()
      }
      tryCatch({
        data <- utils::read.csv(input$alignmentFile$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        data <- validate_alignment(data)
        r$link_data <- data
        r$link_name <- input$alignmentFile$name
        r$link_code <- call("read.csv", input$alignmentFile$name, stringsAsFactors = FALSE)
      }, error = function(e) {
        show_toast(session = session$rootScope(), toaster_id = "toasts",
                   title = "Could not load the alignment", description = conditionMessage(e),
                   variant = "destructive", icon = "alert-circle", duration = 8000)
      })
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    settings <- reactive({
      external <- identical(input$source, "alignment")
      gene_colors <- !external && isTRUE(input$useGroupColors)
      measure <- input$measure %||% "identity"
      problem <- if (external && is.null(r$link_data)) {
        "Upload an alignment CSV, or choose matching gene values."
      } else if (external && measure != "none" && !measure %in% names(r$link_data)) {
        paste("The alignment has no", measure, "column. Choose Strand only or upload that measure.")
      }
      if (!external && !column_has_values(r$cluster_data, input$group)) {
        problem <- "Choose a matching column with gene names or other values shared between clusters."
      }
      if (external && is.null(problem)) {
        cluster_col <- r$input[["cluster"]]
        known <- if (!is.null(cluster_col) && cluster_col %in% names(r$cluster_data)) {
          unique(as.character(r$cluster_data[[cluster_col]]))
        } else "cluster"
        unknown <- setdiff(unique(c(r$link_data$cluster1, r$link_data$cluster2)), known)
        if (length(unknown)) problem <- paste("Alignment cluster names do not match the gene data:",
                                             paste(unknown, collapse = ", "))
      }
      list(
        useAlignment = external,
        ready = is.null(problem), problem = problem,
        args = compact(list(
          show = isTRUE(input$show),
          group = if (!external) input$group,
          data = if (external) r$link_data,
          measure = measure,
          label = !external && isTRUE(input$label),
          value1 = if (!external) parse_delimited_input(txt(input$value1)),
          value2 = if (!external) parse_delimited_input(txt(input$value2))
        )),
        style = compact(list(
          use_group_colors = gene_colors,
          color_bar = if (!gene_colors) {
            if (has_measure()) switch(input$colorBar %||% NONE, show = TRUE, hide = FALSE, NULL) else FALSE
          },
          normal_color = if (!gene_colors) txt(input$normalColor),
          inverted_color = if (!gene_colors) txt(input$invertedColor),
          vmin = if (!gene_colors) num(input$vmin), vmax = if (!gene_colors) num(input$vmax),
          curve = isTRUE(input$curve), linkWidth = num(input$linkWidth),
          linkStyle = compact(list(stroke = txt(input$stroke),
                                   strokeWidth = num(input$strokeWidth), fillOpacity = num(input$opacity)))
        ))
      )
    })
    output$setup <- renderUI({
      value <- settings()
      if (!isTRUE(input$show) || is.null(value$problem)) return(NULL)
      missing_measure <- value$useAlignment && !is.null(r$link_data) &&
        value$args$measure != "none" && !value$args$measure %in% names(r$link_data)
      mismatch <- value$useAlignment && !is.null(r$link_data) && !missing_measure
      setup_notice("Links need more information", value$problem,
                   if (missing_measure) setup_action(session$ns("strandOnly"), "Use strand only"),
                   if (mismatch) setup_action(session$ns("chooseCluster"), "Check cluster column"))
    })
    observeEvent(input$strandOnly, update_block_select(session, "measure", selected = "none"))
    observeEvent(input$chooseCluster, {
      root <- session$rootScope()
      shinyblocks::update_block_nav(root, "section", selected = "data")
      root$sendCustomMessage("gv-open-settings", list(focus = "data-cluster"))
    })
    settings
  })
}

#' @export
ui_alignment <- function(id, data) {
  ns <- NS(id)
  register_input(ns("id"), "combobox")
  sections(
    field_group(
      NULL,
      switch_field(ns("align"), "Align the clusters on a gene",
                   description = "The clusters are shifted, so all keep the same scale."),
      when_on(ns, "align",
        select_field(ns("idColumn"), "Gene column", names(data), "class"),
        field(ns("id"), "Gene",
              block_combobox(ns("id"), choices = unique(as.character(data$class)),
                             selected = as.character(data$class[1]),
                             search_placeholder = "Search genes", size = "sm")),
        select_field(ns("position"), "Align on", ALIGNMENTS, "left")
      )
    )
  )
}

#' @export
server_alignment <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    # Only when the data changes: reading the selection here would make the
    # update of the picker trigger this observer again
    observeEvent(r$cluster_data, isolate({
      req(r$cluster_data)
      cols <- names(r$cluster_data)
      update_block_select(session, "idColumn", choices = cols,
                          selected = pick_choice(r, session$ns("idColumn"), input$idColumn, cols, cols[1]))
    }))
    observeEvent(list(input$idColumn, r$cluster_data), {
      req(r$cluster_data, input$idColumn %in% names(r$cluster_data))
      ids <- unique(as.character(r$cluster_data[[input$idColumn]]))
      ids <- ids[!is.na(ids) & nzchar(ids)]
      # A combobox needs at least one option: without any, clear the selection
      if (length(ids) == 0) {
        update_block_combobox(session, "id", selected = character(0))
      } else {
        update_block_combobox(session, "id", choices = ids,
                              selected = pick_choice(r, session$ns("id"), input$id, ids, ids[1]))
      }
    }, ignoreInit = TRUE)
    reactive({
      list(
        align = isTRUE(input$align),
        id_column = input$idColumn,
        id = input$id,
        position = input$position
      )
    })
  })
}

#' @export
ui_tooltip <- function(id, data) {
  ns <- NS(id)
  register_input(ns("columns"), "combobox")
  sections(
    field_group(
      "Tooltip",
      switch_field(ns("show"), "Show a tooltip on genes", TRUE),
      when_on(ns, "show",
        uiOutput(ns("setup")),
        field(ns("columns"), "Columns",
              block_combobox(ns("columns"), choices = names(data), selected = c("start", "end"),
                             multiple = TRUE, placeholder = "Pick columns",
                             search_placeholder = "Search columns", size = "sm"),
              stacked = TRUE),
        textarea_field(ns("formatter"), "Custom text", rows = 2,
                       placeholder = "<b>{gene}</b> {product}<br>{start} - {end}",
                       description = "HTML with {column} for the values of a gene. Replaces the columns above."))
    ),
    field_group(
      "Position",
      switch_field(ns("trackMouse"), "Show the position under the mouse")
    )
  )
}

#' @export
server_tooltip <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(r$cluster_data, {
      cols <- names(r$cluster_data)
      selected <- pick_choice(r, session$ns("columns"), NULL, cols,
                              intersect(c(r$input[["start"]], r$input[["end"]]), cols))
      update_block_combobox(session, "columns", choices = cols, selected = selected)
    }, ignoreInit = TRUE)
    output$setup <- renderUI({
      selected <- intersect(input$columns, names(r$cluster_data))
      if (isTRUE(input$show) && length(selected) == 0L && is.null(txt(input$formatter))) {
        setup_notice("Choose what to show in the tooltip",
                     "Pick columns below, or enter custom text, to show information when you hover a gene.")
      }
    })
    reactive({
      cols <- input$columns
      formatter <- txt(input$formatter) %||% if (length(cols) > 0) {
        paste(sprintf("<b>%s:</b> {%s}", cols, cols), collapse = "<br>")
      }
      list(
        args = compact(list(show = isTRUE(input$show), formatter = formatter)),
        trackMouse = isTRUE(input$trackMouse)
      )
    })
  })
}

#' @export
ui_size <- function(id) {
  ns <- NS(id)
  sections(
    field_group(
      "Chart",
      num_field(ns("width"), "Width (px)", min = 100, step = 10,
                description = "Empty fills the preview. Wider charts scroll horizontally."),
      num_field(ns("height"), "Height (px)", 400, min = 100, step = 10)
    ),
    field_group(
      "Margins (px)",
      num_field(ns("top"), "Top", 5, min = 0, step = 5),
      num_field(ns("bottom"), "Bottom", 5, min = 0, step = 5),
      num_field(ns("left"), "Left", 50, min = 0, step = 5),
      num_field(ns("right"), "Right", 50, min = 0, step = 5)
    )
  )
}

#' @export
server_size <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      width <- num(input$width)
      height <- num(input$height)
      list(
        width = if (is.null(width)) "100%" else paste0(width, "px"),
        height = if (is.null(height)) "400px" else paste0(height, "px"),
        margin = compact(list(top = num(input$top), bottom = num(input$bottom),
                              left = num(input$left), right = num(input$right)))
      )
    })
  })
}
