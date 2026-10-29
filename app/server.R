library(shiny)

box::use(
  ./views/section_data[server_data],
  ./views/section_theme[server_theme],
  ./views/section_genes[server_genes, server_colors],
  ./views/section_labels[server_labels, server_legend],
  ./views/section_titles[server_titles],
  ./views/section_element[server_element],
  ./views/section_scale[server_scale, server_coordinates, server_sequence],
  ./views/section_compare[server_links, server_alignment, server_tooltip, server_size],
  ./logic/setup[server_group_setup],
  ./logic/utils[make_named_color_list, order_cluster_data, cluster_values, opt],
  ./logic/code_gen[chart_code, data_code],
  ./logic/examples[EXAMPLES],
  ./logic/ui_helpers[INPUT_TYPES, COLOR_INPUTS, NONE],
  ./views/section_data[column_choices],
)

function(input, output, session) {

  observeEvent(input$gvColorPick, {
    pick <- input$gvColorPick
    req(is.character(pick$id), length(pick$id) == 1,
        exists(pick$id, envir = COLOR_INPUTS, inherits = FALSE),
        is.character(pick$value), length(pick$value) == 1)
    shinyblocks::update_block_input(session, pick$id, value = pick$value)
  })

  r <- reactiveValues(
    cluster_data = geneviewer::ophA_clusters,
    example = "ophA"
  )

  # The columns the chart uses and the clusters it shows, as picked in the
  # Data section; NULL where none is picked
  mapping <- reactive({
    column <- function(col) opt(input[[paste0("data-", col)]])
    list(start = column("start"), end = column("end"), cluster = column("cluster"),
         group = column("group"), strand = column("strand"),
         order = if (length(input[["data-order"]]) > 0) input[["data-order"]])
  })

  server_group_setup("groupSetup", r = r, mapping = mapping)
  theme_inputs <- server_theme("theme")
  gene_inputs <- server_genes("genes")
  color_inputs <- server_colors("colors")
  label_inputs <- server_labels("labels", r = r)
  legend_inputs <- server_legend("legend", r = r, mapping = mapping)
  title_inputs <- server_titles("titles")
  scale_inputs <- server_scale("scale")
  coordinate_inputs <- server_coordinates("coordinates")
  sequence_inputs <- server_sequence("sequence")
  link_inputs <- server_links("links", r = r, mapping = mapping)
  alignment_inputs <- server_alignment("alignment", r = r)
  tooltip_inputs <- server_tooltip("tooltip", r = r)
  size_inputs <- server_size("size")

  # The geneviewer calls behind the chart, shown as R code under it
  calls <- NULL

  # Run a geneviewer function on the chart and record the call. Arguments
  # come as lists; NULL and empty ones are left out unless keep_null is set.
  add <- function(chart, name, ..., keep_null = FALSE, record = TRUE, code_args = NULL) {
    # Run the earlier calls of a pipe first, so the calls are recorded in order
    force(chart)
    safely(chart, function(chart) {
      args <- do.call(c, list(...)) %||% list()
      if (!keep_null) {
        keep <- function(a) !is.null(a) && !(is.list(a) && length(a) == 0)
        args <- Filter(keep, args)
        if (!is.null(code_args)) code_args <- Filter(keep, code_args)
      }
      result <- do.call(getExportedValue("geneviewer", name), c(list(chart), args))
      # Failed steps leave both the chart and its generated code unchanged.
      if (record) calls[[length(calls) + 1]] <<- list(name = name, args = code_args %||% args)
      result
    })
  }

  # Components switched off do nothing; leave them out of the code
  add_shown <- function(chart, name, args, ...) {
    if (identical(args$show, FALSE)) return(chart)
    add(chart, name, args, ...)
  }

  # Each message is shown once, so a warning does not repeat on every change
  shown <- new.env()
  notify <- function(message, variant = "warning") {
    if (exists(message, envir = shown, inherits = FALSE)) return(invisible())
    # While an example's settings arrive the settings are a mix of the old and
    # the new ones; their warnings are not meaningful
    if (variant == "warning" && !is.null(isolate(r$pending))) return(invisible())
    assign(message, TRUE, envir = shown)
    shinyblocks::show_toast(
      toaster_id = "toasts",
      title = if (variant == "destructive") "Something went wrong" else "Note",
      description = message,
      variant = variant,
      icon = if (variant == "destructive") "alert-circle" else "alert-triangle",
      duration = 8000
    )
  }

  # Run a step of the chart; on an error keep the chart as it was
  safely <- function(chart, step) {
    withCallingHandlers(
      tryCatch(step(chart), error = function(e) {
        notify(conditionMessage(e), "destructive")
        chart
      }),
      warning = function(w) {
        notify(conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  # Examples ---------------------------------------------------------------

  # The value of every setting at the start of the session is its default
  defaults <- NULL
  session$onFlushed(function() {
    defaults <<- isolate(reactiveValuesToList(input))
  }, once = TRUE)

  # Is a setting already at a value (empty number fields are NA or "")
  same_setting <- function(current, target) {
    empty <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")
    if (empty(current) && empty(target)) return(TRUE)
    if (is.numeric(current) && is.numeric(target)) return(isTRUE(all.equal(current, target)))
    identical(current, target)
  }

  # The choices of settings that list columns or values of the data
  data_choices <- function(id, data, values) {
    cols <- names(data)
    # Values as choices: no NA, and not "" (the empty choice of shinyblocks)
    values_of <- function(col) {
      v <- unique(as.character(data[[col]]))
      v[!is.na(v) & nzchar(v)]
    }
    switch(id,
      "labels-label" = c("None" = NONE, cols),
      "links-group" = , "alignment-idColumn" = cols,
      "tooltip-columns" = cols,
      "titles-label_titleColumn" = , "titles-label_subtitleColumn" = c("Manual text" = NONE, cols),
      "labels-styleBy" = , "genes-styleBy" = c("Group column" = NONE, cols),
      "data-start" = , "data-end" = , "data-cluster" = , "data-group" = , "data-strand" =
        column_choices(cols),
      "data-order" = {
        col <- values[["data-cluster"]] %||% NONE
        if (col %in% cols) values_of(col) else character(0)
      },
      "alignment-id" = {
        col <- values[["alignment-idColumn"]] %||% defaults[["alignment-idColumn"]]
        if (isTRUE(col %in% cols)) values_of(col) else character(0)
      },
      NULL
    )
  }

  # Set one setting to the value of the example, or back to its default
  set_setting <- function(id, targets, data) {
    type <- get(id, envir = INPUT_TYPES)
    target <- targets[[id]]
    choices <- data_choices(id, data, targets)
    if (is.null(choices) && same_setting(isolate(input[[id]]), target)) return(invisible())
    empty <- is.null(target) || length(target) == 0 || all(is.na(target))
    switch(type,
      text = , number = shinyblocks::update_block_input(
        session, id, value = if (empty) "" else target),
      textarea = shinyblocks::update_block_textarea(
        session, id, value = if (empty) "" else target),
      switch = shinyblocks::update_block_switch(session, id, checked = isTRUE(target)),
      toggle = shinyblocks::update_block_toggle_group(session, id, selected = target),
      select = if (is.null(choices)) {
        shinyblocks::update_block_select(session, id, selected = target)
      } else {
        if (!isTRUE(target %in% choices)) target <- if (NONE %in% choices) NONE else choices[1]
        shinyblocks::update_block_select(session, id, choices = choices, selected = target)
      },
      combobox = {
        if (!is.null(choices)) target <- intersect(target, choices)
        # A combobox needs at least one option: without any, clear the selection
        if (is.null(choices) || length(choices) == 0) {
          shinyblocks::update_block_combobox(session, id, selected = target %||% character(0))
        } else {
          shinyblocks::update_block_combobox(session, id, choices = choices,
                                             selected = target %||% character(0))
        }
      }
    )
  }

  # Set every setting; one that fails does not stop the others
  apply_settings <- function(targets, data) {
    failed <- character()
    for (id in ls(INPUT_TYPES)) {
      tryCatch(set_setting(id, targets, data), error = function(e) failed <<- c(failed, id))
    }
    if (length(failed) > 0) {
      notify(paste("Some settings could not be set:", paste(failed, collapse = ", ")))
    }
  }

  # Loading data. The settings reach the browser as a series of updates and
  # come back one by one. To draw the chart once, with all of them:
  # 1. the settings are "pending": the chart waits, pickers that get new
  #    choices take the pending values, and warnings are held back;
  # 2. all settings are updated, then a marker; the browser handles updates in
  #    order, so the marker comes back after all the settings;
  # 3. when the marker comes back the chart is drawn, with the load's token,
  #    and the browser lifts the cover on its native rendered event.
  # Every setting takes its value in `values`, or else its default; with
  # keep = TRUE (files added to an upload) its current value instead.
  load_token <- 0
  load_dataset <- function(data, values, keep = FALSE) {
    load_token <<- load_token + 1
    # Reactive, so that every load draws the chart again with its token, also
    # one that leaves the data as it was (a file uploaded a second time)
    r$load_token <- load_token
    session$sendCustomMessage("gv-loading", list(token = load_token))
    current <- if (keep) isolate(reactiveValuesToList(input)) else defaults
    targets <- sapply(ls(INPUT_TYPES), function(id) {
      if (id %in% names(values)) values[[id]] else current[[id]]
    }, simplify = FALSE)

    r$pending <- list(token = load_token, targets = targets)
    r$cluster_data <- data

    tryCatch(
      apply_settings(targets, data),
      error = function(e) notify(paste("The data could not be loaded:", conditionMessage(e)),
                                 "destructive")
    )
  }

  observeEvent(input[["data-example"]], {
    key <- input[["data-example"]]
    req(key %in% names(EXAMPLES))
    example <- EXAMPLES[[key]]
    data <- example$data()
    values <- example$inputs(data)
    for (col in c("data-start", "data-end", "data-cluster", "data-group", "data-strand")) {
      if (!col %in% names(values)) values[[col]] <- NONE
    }

    r$link_data <- if (is.function(example$links)) example$links() else NULL
    r$link_code <- example$link_code
    r$link_name <- if (is.function(example$links)) paste(example$label, "alignment") else NULL
    r$source_files <- NULL
    r$example <- key
    r$source_name <- example$label
    load_dataset(data, values)
  }, ignoreInit = TRUE)

  server_data("data", r = r, toaster_id = "toasts", load_dataset = load_dataset)

  # The marker goes last: this observer runs after all others of the flush,
  # also after the pickers that refill with the new data
  observeEvent(r$pending$token, {
    shinyblocks::update_block_input(session, "gvMarker", value = as.character(r$pending$token))
  }, priority = -1000)

  observeEvent(input$gvMarker, {
    if (identical(input$gvMarker, as.character(r$pending$token))) r$pending <- NULL
  })

  # A click on a gene or label opens it in the Element section, a click on
  # another part of the chart opens its settings. Legend labels and markers
  # are edited individually. Dragging moves text and scales; it never opens
  # an editor of its own (the widget tells a click from a drag).
  open_section <- function(section, tab = NULL) {
    shinyblocks::update_block_nav(session, "section", selected = section)
    if (!is.null(tab)) shinyblocks::update_block_tabs(session, paste0(section, "-tab"), selected = tab)
    session$sendCustomMessage("gv-open-settings", TRUE)
  }
  element_editor <- server_element("element", data_key = reactive(list(
    r$cluster_data, mapping()[c("start", "end", "cluster", "order", "group")]
  )), open_section = open_section, section = reactive(input$section),
     # Genes on the element card are named by the chart's label column
     label_column = reactive(label_inputs()$args$label))
  click_sections <- list(
    transcript = "genes", coordinates = "coordinates", links = "links", legend = "legend",
    sequence = "sequence",
    scale = c("scale", "axis"), scaleBar = c("scale", "scalebar"),
    title = c("titles", "chart"), clusterTitle = c("titles", "title"), clusterFooter = c("titles", "footer"),
    clusterLabel = c("titles", "label")
  )
  # Elements that can be selected follow the selection (below), so a second
  # click on one, which ends its selection, does not open it again
  observeEvent(input$gcChart_click, {
    req(!element_editor$selectable(input$gcChart_click))
    target <- click_sections[[input$gcChart_click$component %||% ""]]
    req(target)
    open_section(target[[1]], if (length(target) == 2) target[[2]])
  })
  observeEvent(input$gcChart_drag, element_editor$move(input$gcChart_drag))
  # The chart's reset, or undoing or redoing it, moves several elements at once
  observeEvent(input$gcChart_moves, element_editor$place(input$gcChart_moves$moves))
  # Enter on a selectable element that has keyboard focus
  observeEvent(input$gcChart_dblclick, element_editor$open(input$gcChart_dblclick))
  # Several selected elements (Shift-click or a box in the chart) are edited
  # together; one selected element as below
  observeEvent(input$gcChart_selection, element_editor$open_group(input$gcChart_selection),
               ignoreNULL = FALSE, ignoreInit = TRUE)
  # Selecting an element opens it; a second click on it, a click elsewhere
  # or Escape ends the selection
  observeEvent(input$gcChart_select, {
    if (is.null(input$gcChart_select)) element_editor$clear()
    else element_editor$open(input$gcChart_select, navigate = FALSE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # The chart and its code
  chart <- reactive({
    columns <- mapping()
    data <- order_cluster_data(
      data = r$cluster_data,
      cluster_col = columns$cluster,
      order_vec = columns$order
    )
    group <- columns$group
    theme <- theme_inputs()
    size <- size_inputs()
    genes <- gene_inputs()
    colors <- color_inputs()
    labels <- label_inputs()
    legend <- legend_inputs()
    titles <- title_inputs()
    if (!is.null(titles$label$titleColumn)) {
      titles$label$args$title <- cluster_values(data, columns$cluster,
                                              titles$label$titleColumn)
    }
    if (!is.null(titles$label$subtitleColumn)) {
      titles$label$args$subtitle <- cluster_values(data, columns$cluster,
                                                 titles$label$subtitleColumn)
    }
    scale <- scale_inputs()
    coordinates <- coordinate_inputs()
    sequence <- sequence_inputs()
    tooltip <- tooltip_inputs()
    links <- link_inputs()
    alignment <- alignment_inputs()

    calls <<- list()
    chart_args <- Filter(Negate(is.null), list(
      start = columns$start,
      end = columns$end,
      cluster = columns$cluster,
      group = group,
      strand = columns$strand,
      width = size$width,
      height = size$height
    ))
    calls[[1]] <<- list(name = "GC_chart", args = chart_args)
    # GC_grid() only when the margins are changed: with the default margins it
    # just makes each cluster a few pixels lower
    default_margin <- list(top = 5, bottom = 5, left = 50, right = 50)
    margin_changed <- !isTRUE(all.equal(
      lapply(size$margin[names(default_margin)], as.numeric), default_margin
    ))
    chart <- safely(NULL, function(chart) {
      do.call(geneviewer::GC_chart, c(list(data), chart_args))
    })
    if (is.null(chart)) {
      # No chart: tell the browser the load is over, so it lifts the cover
      session$sendCustomMessage("gv-loaded", list(token = r$load_token))
      req(FALSE)
    }

    if (margin_changed) chart <- add(chart, "GC_grid", list(margin = size$margin))

    if (theme$active) {
      chart <- add(chart, "GC_theme", theme$settings)
    }

    color_scheme <- colors$style$colorScheme
    # Colours of named groups, or a list of colours for all groups in order
    custom_colors <- colors$namedColors %||% if (!is.null(group) && group %in% names(data)) {
      make_named_color_list(data, group, colors$customColors)
    }

    chart <- chart |>
      add_shown("GC_title", c(titles$chart$args, titles$chart$style)) |>
      add("GC_genes", list(group = group), genes$args, genes$style) |>
      add_shown("GC_labels", c(labels$args, labels$style)) |>
      add("GC_scale", scale$args, scale$style) |>
      add_shown("GC_scaleBar", c(scale$bar$args, scale$bar$style)) |>
      add_shown("GC_clusterTitle", c(titles$title$args, titles$title$style)) |>
      add_shown("GC_clusterFooter", c(titles$footer$args, titles$footer$style)) |>
      add_shown("GC_clusterLabel", c(titles$label$args, titles$label$style))
    # Without a group the legend has nothing to show, and GC_legend() stops
    if (!is.null(group)) {
      chart <- add(chart, "GC_legend", list(group = group), legend$args, legend$style)
    }
    chart <- chart |>
      add_shown("GC_coordinates", c(coordinates$args, coordinates$style)) |>
      add("GC_sequence", sequence$args, sequence$style) |>
      add("GC_color",
          list(colorScheme = if (is.null(colors$customColors)) color_scheme,
               customColors = custom_colors)) |>
      add("GC_tooltip", tooltip$args)

    if (colors$patterns$show) {
      # background = NULL draws the patterns on top of the gene colours
      chart <- add(chart, "GC_pattern", colors$patterns$args, keep_null = TRUE)
    }
    if (tooltip$trackMouse) {
      chart <- add(chart, "GC_trackMouse")
    }
    # App interaction and loading events are not part of the exported code.
    chart <- add(chart, "GC_events", list(
      click = TRUE, highlight = TRUE, dblclick = TRUE, select = TRUE, drag = TRUE,
      rendered = TRUE, render_id = r$load_token
    ), record = FALSE)

    if (alignment$align && !is.null(alignment$id)) {
      chart <- add(chart, "GC_align", list(id_column = alignment$id_column, id = alignment$id,
                                          align = alignment$position))
    }

    if (links$args$show) {
      if (!links$ready) {
        notify(links$problem)
      } else {
        args <- c(links$args[names(links$args) != "show"], links$style)
        recorded <- args
        if (links$useAlignment) recorded$data <- quote(alignment)
        chart <- add(chart, "GC_links", args, code_args = recorded)
      }
    }

    # Styles of single genes and labels (the Element section)
    for (edit in element_editor$edits()) {
      chart <- add(chart, "GC_element_style", c(list(component = edit$component), edit$id, edit$style))
    }

    # The code is extra: a problem with it must not stop the chart
    code <- tryCatch(
      chart_code(calls, data_code(
        files = r$source_files,
        cluster_col = columns$cluster,
        order = columns$order,
        example_code = EXAMPLES[[r$example %||% ""]]$code,
        link_code = if (links$args$show && links$useAlignment && links$ready) r$link_code
      ), trim = is.null(chart$x$theme)),
      error = function(e) paste("# The code could not be written:", conditionMessage(e))
    )
    list(widget = chart, code = code)
  })

  # While the settings of a load arrive, keep the chart and code that are shown
  output$gcChart <- geneviewer::renderGC_chart({
    req(is.null(r$pending), cancelOutput = TRUE)
    chart()$widget
  })

  output$chartCode <- renderUI({
    req(is.null(r$pending), cancelOutput = TRUE)
    shinyblocks::block_code(chart()$code, language = "r", line_numbers = FALSE)
  })
}
