library(shiny)

box::use(
  ./views/section_data[server_data],
  ./views/section_theme[server_theme],
  ./views/section_genes[server_genes, server_colors],
  ./views/section_labels[server_labels, server_legend],
  ./views/section_titles[server_titles],
  ./views/section_scale[server_scale, server_coordinates, server_sequence],
  ./views/section_compare[server_links, server_alignment, server_tooltip, server_size],
  ./logic/utils[make_named_color_list, order_cluster_data],
  ./logic/code_gen[chart_code, data_code],
  ./logic/examples[EXAMPLES],
  ./logic/ui_helpers[INPUT_TYPES, NONE],
  ./views/section_data[column_choices],
)

function(input, output, session) {

  r <- reactiveValues(
    input = list(
      start = "start",
      end = "end",
      cluster = "cluster",
      group = "class"
    ),
    cluster_data = geneviewer::ophA_clusters,
    example = "ophA"
  )

  server_data("data", r = r, toaster_id = "toasts")
  theme_inputs <- server_theme("theme")
  gene_inputs <- server_genes("genes", r = r)
  color_inputs <- server_colors("colors")
  label_inputs <- server_labels("labels", r = r)
  legend_inputs <- server_legend("legend")
  title_inputs <- server_titles("titles")
  scale_inputs <- server_scale("scale")
  coordinate_inputs <- server_coordinates("coordinates")
  sequence_inputs <- server_sequence("sequence")
  link_inputs <- server_links("links", r = r)
  alignment_inputs <- server_alignment("alignment", r = r)
  tooltip_inputs <- server_tooltip("tooltip", r = r)
  size_inputs <- server_size("size")

  # The geneviewer calls behind the chart, shown as R code under it
  calls <- NULL
  code <- reactiveVal("")

  # Run a geneviewer function on the chart and record the call. Arguments
  # come as lists; NULL and empty ones are left out unless keep_null is set.
  add <- function(chart, name, ..., keep_null = FALSE, record = TRUE) {
    # Run the earlier calls of a pipe first, so the calls are recorded in order
    force(chart)
    args <- do.call(c, list(...)) %||% list()
    if (!keep_null) {
      args <- Filter(function(a) !is.null(a) && !(is.list(a) && length(a) == 0), args)
    }
    if (record) calls[[length(calls) + 1]] <<- list(name = name, args = args)
    do.call(getExportedValue("geneviewer", name), c(list(chart), args))
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
      "labels-label" = , "links-group" = , "alignment-idColumn" = cols,
      "tooltip-columns" = cols,
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

  # Loading an example. The settings reach the browser as a series of updates
  # and come back one by one. To draw the chart once, with all of them:
  # 1. the example's settings are "pending": the chart waits, pickers that get
  #    new choices take the pending values, and warnings are held back;
  # 2. all settings are updated, then a marker; the browser handles updates in
  #    order, so the marker comes back after all the settings;
  # 3. when the marker comes back the chart is drawn, with the load's token,
  #    and the browser lifts the loading cover when that chart arrives.
  load_token <- 0
  observeEvent(input[["data-example"]], {
    key <- input[["data-example"]]
    req(key %in% names(EXAMPLES))
    example <- EXAMPLES[[key]]
    load_token <<- load_token + 1
    session$sendCustomMessage("gv-loading", list(token = load_token))

    data <- example$data()
    values <- example$inputs(data)
    for (col in c("data-start", "data-end", "data-cluster", "data-group", "data-strand")) {
      if (!col %in% names(values)) values[[col]] <- NONE
    }
    # Every setting: the example's value, or back to its default
    targets <- sapply(ls(INPUT_TYPES), function(id) {
      if (id %in% names(values)) values[[id]] else defaults[[id]]
    }, simplify = FALSE)

    r$pending <- list(token = load_token, targets = targets)
    r$cluster_data <- data
    r$source_files <- NULL
    r$example <- key
    r$source_name <- example$label
    for (col in c("start", "end", "cluster", "group", "strand")) {
      value <- values[[paste0("data-", col)]]
      r$input[[col]] <- if (identical(value, NONE)) NULL else value
    }
    r$input[["select_order"]] <- NULL

    tryCatch(
      apply_settings(targets, data),
      error = function(e) notify(paste("The example could not be loaded:", conditionMessage(e)),
                                 "destructive")
    )
  }, ignoreInit = TRUE)

  # The marker goes last: this observer runs after all others of the flush,
  # also after the pickers that refill with the new data
  observeEvent(r$pending$token, {
    shinyblocks::update_block_input(session, "gvMarker", value = as.character(r$pending$token))
  }, priority = -1000)

  observeEvent(input$gvMarker, {
    if (identical(input$gvMarker, as.character(r$pending$token))) r$pending <- NULL
  })

  # Click to edit: a click on a part of the chart opens its settings
  click_sections <- list(
    genes = "genes", transcript = "genes", labels = "labels",
    coordinates = "coordinates", links = "links", legend = "legend",
    sequence = "sequence",
    scale = c("scale", "axis"), scaleBar = c("scale", "scalebar"),
    clusterTitle = c("titles", "title"), clusterFooter = c("titles", "footer"),
    clusterLabel = c("titles", "label")
  )
  observeEvent(input$gcChart_click, {
    req(isTRUE(input$editMode))
    target <- click_sections[[input$gcChart_click$component %||% ""]]
    req(target)
    shinyblocks::update_block_nav(session, "section", selected = target[[1]])
    if (length(target) == 2) {
      shinyblocks::update_block_tabs(session, paste0(target[[1]], "-tab"), selected = target[[2]])
    }
    session$sendCustomMessage("gv-open-settings", TRUE)
  })

  output$gcChart <- geneviewer::renderGC_chart({
    # While an example's settings arrive, keep the chart that is shown
    req(is.null(r$pending), cancelOutput = TRUE)
    data <- order_cluster_data(
      data = r$cluster_data,
      cluster_col = r$input[["cluster"]],
      order_vec = r$input[["select_order"]]
    )
    group <- r$input[["group"]]
    theme <- theme_inputs()
    size <- size_inputs()
    genes <- gene_inputs()
    colors <- color_inputs()
    labels <- label_inputs()
    legend <- legend_inputs()
    titles <- title_inputs()
    scale <- scale_inputs()
    coordinates <- coordinate_inputs()
    sequence <- sequence_inputs()
    tooltip <- tooltip_inputs()
    links <- link_inputs()
    alignment <- alignment_inputs()

    calls <<- list()
    chart_args <- Filter(Negate(is.null), list(
      start = r$input[["start"]],
      end = r$input[["end"]],
      cluster = r$input[["cluster"]],
      group = group,
      strand = r$input[["strand"]],
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
      chart <- do.call(geneviewer::GC_chart, c(list(data), chart_args))
      if (margin_changed) chart <- add(chart, "GC_grid", list(margin = size$margin))
      chart
    })
    if (is.null(chart)) {
      # No chart: tell the browser the load is over, so it lifts the cover
      session$sendCustomMessage("gv-loaded", list(token = load_token))
      req(FALSE)
    }

    chart <- safely(chart, function(chart) {
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
        add_shown("GC_clusterLabel", c(titles$label$args, titles$label$style)) |>
        add("GC_legend", list(group = group), legend$args, legend$style) |>
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
      # Click to edit is for the app only, so not part of the code
      if (isTRUE(input$editMode)) {
        chart <- add(chart, "GC_events", list(highlight = TRUE), record = FALSE)
      }
      chart
    })

    if (alignment$align && !is.null(alignment$id)) {
      chart <- safely(chart, function(chart) {
        add(chart, "GC_align", list(id_column = alignment$id_column, id = alignment$id,
                                    align = alignment$position))
      })
    }

    if (links$args$show) {
      chart <- safely(chart, function(chart) {
        add(chart, "GC_links", links$args[names(links$args) != "show"],
            links$style)
      })
    }

    # The code is extra: a problem with it must not stop the chart
    code(tryCatch(
      chart_code(calls, data_code(
        files = r$source_files,
        cluster_col = r$input[["cluster"]],
        order = r$input[["select_order"]],
        example_code = EXAMPLES[[r$example %||% ""]]$code
      ), trim = !theme$active),
      error = function(e) paste("# The code could not be written:", conditionMessage(e))
    ))
    # The browser lifts the loading cover when the chart of its load arrives
    chart$x$loadToken <- load_token
    chart
  })

  output$chartCode <- renderUI({
    shinyblocks::block_code(code(), language = "r", line_numbers = FALSE)
  })
}
