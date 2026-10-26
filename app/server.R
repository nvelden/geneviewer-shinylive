library(shiny)

box::use(
  ./views/section_data[server_data],
  ./views/section_theme[server_theme],
  ./views/section_genes[server_genes, server_colors],
  ./views/section_labels[server_labels, server_legend],
  ./views/section_titles[server_titles],
  ./views/section_scale[server_scale, server_coordinates, server_sequence],
  ./views/section_compare[server_links, server_alignment, server_tooltip, server_size],
  ./logic/utils[make_named_color_list, order_cluster_data, drop_unchanged, call_gc],
)

function(input, output, session) {

  r <- reactiveValues(
    input = list(
      start = "start",
      end = "end",
      cluster = "cluster",
      group = "class"
    ),
    cluster_data = geneviewer::ophA_clusters
  )

  server_data("data", r = r, toaster_id = "toasts")
  theme_inputs <- server_theme("theme")
  gene_inputs <- server_genes("genes")
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

  # The first value of each set of style options is its default. With a theme,
  # options left at their default are dropped, so the theme sets them.
  style_defaults <- new.env()
  themed <- function(name, x) {
    if (!exists(name, envir = style_defaults, inherits = FALSE)) {
      assign(name, x, envir = style_defaults)
    }
    if (!isTRUE(theme_inputs()$active)) return(x)
    drop_unchanged(x, get(name, envir = style_defaults)) %||% list()
  }

  # Add a GC_ component; arguments come as lists, NULL ones are left out
  add <- function(chart, fun, ...) {
    do.call(call_gc, c(list(fun, chart), ...))
  }

  # Each message is shown once, so a warning does not repeat on every change
  shown <- new.env()
  notify <- function(message, variant = "warning") {
    if (exists(message, envir = shown, inherits = FALSE)) return(invisible())
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

  output$dataBadges <- renderUI({
    data <- r$cluster_data
    col <- r$input[["cluster"]]
    n_clusters <- if (!is.null(col) && col %in% names(data)) length(unique(data[[col]])) else 1
    shinyblocks::block_cluster(
      gap = "sm",
      shinyblocks::block_badge(sprintf("%d clusters", n_clusters), variant = "outline"),
      shinyblocks::block_badge(sprintf("%d genes", nrow(data)), variant = "outline")
    )
  })

  output$gcChart <- geneviewer::renderGC_chart({
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

    chart <- safely(NULL, function(chart) {
      geneviewer::GC_chart(
        data,
        start = r$input[["start"]],
        end = r$input[["end"]],
        cluster = r$input[["cluster"]],
        group = group,
        strand = r$input[["strand"]],
        width = size$width,
        height = size$height
      ) |>
        geneviewer::GC_grid(margin = size$margin)
    })
    req(chart)

    chart <- safely(chart, function(chart) {
      if (theme$active) {
        chart <- do.call(geneviewer::GC_theme, c(list(chart), theme$settings))
      }

      color_scheme <- themed("colors", colors$style)$colorScheme
      custom_colors <- if (!is.null(group) && group %in% names(data)) {
        make_named_color_list(data, group, colors$customColors)
      }

      chart <- chart |>
        add(geneviewer::GC_genes, list(group = group), genes$args, themed("genes", genes$style)) |>
        add(geneviewer::GC_labels, labels$args, themed("labels", labels$style)) |>
        add(geneviewer::GC_scale, scale$args, themed("scale", scale$style)) |>
        add(geneviewer::GC_scaleBar, scale$bar$args, themed("scaleBar", scale$bar$style)) |>
        add(geneviewer::GC_clusterTitle, titles$title$args, themed("title", titles$title$style)) |>
        add(geneviewer::GC_clusterFooter, titles$footer$args, themed("footer", titles$footer$style)) |>
        add(geneviewer::GC_clusterLabel, titles$label$args, themed("clusterLabel", titles$label$style)) |>
        add(geneviewer::GC_legend, list(group = group), legend$args, themed("legend", legend$style)) |>
        add(geneviewer::GC_coordinates, coordinates$args, themed("coordinates", coordinates$style)) |>
        add(geneviewer::GC_sequence, sequence$args, themed("sequence", sequence$style)) |>
        add(geneviewer::GC_color,
            list(colorScheme = if (is.null(custom_colors)) color_scheme,
                 customColors = custom_colors)) |>
        add(geneviewer::GC_tooltip, tooltip$args)

      if (colors$patterns$show) {
        chart <- geneviewer::GC_pattern(
          chart,
          color = colors$patterns$color,
          # NULL draws the patterns on top of the gene colours
          background = colors$patterns$background,
          size = colors$patterns$size
        )
      }
      if (tooltip$trackMouse) {
        chart <- geneviewer::GC_trackMouse(chart)
      }
      if (isTRUE(input$editMode)) {
        chart <- geneviewer::GC_events(chart, highlight = TRUE)
      }
      chart
    })

    if (alignment$align && !is.null(alignment$id)) {
      chart <- safely(chart, function(chart) {
        geneviewer::GC_align(chart, id_column = alignment$id_column, id = alignment$id,
                             align = alignment$position)
      })
    }

    if (links$args$show) {
      chart <- safely(chart, function(chart) {
        add(chart, geneviewer::GC_links, links$args[names(links$args) != "show"],
            themed("links", links$style))
      })
    }

    chart
  })
}
