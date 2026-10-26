#' Titles section
#'
#' The title and footer above and below each cluster, and the cluster labels
#' beside them.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observeEvent, req, tags, conditionalPanel],
  shinyblocks[block_tabs, block_tab, update_block_select],
  ../logic/ui_helpers[text_field, num_field, select_field, switch_field,
                      field_group, sections, when_on, ALIGNMENTS, FONT_STYLES, NONE],
  ../logic/utils[num, txt, opt, px, compact, parse_delimited_input, pick_choice],
  ./section_labels[font_fields, font_values]
)

#' Title or footer fields (prefix "title_" or "footer_")
heading_fields <- function(ns, prefix, what, defaults) {
  id <- function(x) paste0(prefix, x)
  sections(
    field_group(
      NULL,
      switch_field(ns(id("show")), paste("Show cluster", what)),
      when_on(ns, id("show"),
        text_field(ns(id("title")), "Text", placeholder = "Cluster 1, Cluster 2, ...",
                   description = "One per cluster, separated by commas.", stacked = TRUE),
        text_field(ns(id("subtitle")), "Subtitle", placeholder = "Subtitle 1, Subtitle 2, ...",
                   stacked = TRUE)
      )
    ),
    when_on(ns, id("show"),
      field_group(
        "Placement",
        select_field(ns(id("align")), "Align", ALIGNMENTS, default = defaults$align),
        if (!is.null(defaults$height)) text_field(ns(id("height")), "Height", default = defaults$height),
        num_field(ns(id("x")), "X offset", step = 1, default = 0),
        num_field(ns(id("y")), "Y offset", step = 1, default = defaults$y),
        num_field(ns(id("spacing")), "Line spacing", min = 0, step = 1, default = defaults$spacing)
      ),
      field_group("Text font", font_fields(ns, id("t_"), size = defaults$size,
                                           weight = "bold", show_style = FALSE)),
      field_group("Subtitle font", font_fields(ns, id("s_"), size = defaults$subSize,
                                               show_style = FALSE))
    )
  )
}

heading_values <- function(input, prefix) {
  get <- function(x) input[[paste0(prefix, x)]]
  list(
    args = compact(list(
      show = isTRUE(get("show")),
      title = parse_delimited_input(txt(get("title"))),
      subtitle = parse_delimited_input(txt(get("subtitle")))
    )),
    style = compact(list(
      align = opt(get("align")),
      height = txt(get("height")),
      x = num(get("x")),
      y = num(get("y")),
      spacing = num(get("spacing")),
      titleFont = font_values(input, paste0(prefix, "t_")),
      subtitleFont = font_values(input, paste0(prefix, "s_"))
    ))
  )
}

#' @export
ui_titles <- function(id, data) {
  ns <- NS(id)
  block_tabs(
    id = ns("tab"),
    variant = "line",
    class = "gv-tabs",
    block_tab("Chart", value = "chart",
      sections(
        field_group(
          NULL,
          switch_field(ns("chart_show"), "Show a chart title"),
          when_on(ns, "chart_show",
            text_field(ns("chart_title"), "Title", placeholder = "Title of the chart",
                       description = "HTML works, e.g. <i>italic</i>.", stacked = TRUE),
            text_field(ns("chart_subtitle"), "Subtitle", stacked = TRUE)
          )
        ),
        when_on(ns, "chart_show",
          field_group(
            "Placement",
            select_field(ns("chart_align"), "Align", ALIGNMENTS, default = "center"),
            text_field(ns("chart_height"), "Height", default = "Automatic")
          ),
          field_group("Title font", font_fields(ns, "chart_t_", size = 16, weight = "bold")),
          field_group("Subtitle font", font_fields(ns, "chart_s_", size = 14))
        )
      )
    ),
    block_tab("Title", value = "title",
      heading_fields(ns, "title_", "title",
                     list(align = "center", height = "Automatic", y = "Automatic",
                          spacing = "Automatic", size = 16, subSize = 14))
    ),
    block_tab("Footer", value = "footer",
      heading_fields(ns, "footer_", "footer",
                     list(align = "left", y = 0, spacing = "Automatic", size = 12, subSize = 10))
    ),
    block_tab("Cluster label", value = "label",
      sections(
        field_group(
          NULL,
          switch_field(ns("label_show"), "Show cluster labels"),
          when_on(ns, "label_show",
            text_field(ns("label_title"), "Text", placeholder = "Label 1, Label 2, ...",
                       description = "Empty uses the cluster names.", stacked = TRUE),
            select_field(ns("label_subtitleColumn"), "Subtitle from column",
                         c("Manual text" = NONE, names(data)), NONE,
                         description = "Uses the first value per cluster; follows filtering and ordering."),
            conditionalPanel(sprintf("input['%s'] === '%s'", ns("label_subtitleColumn"), NONE),
              text_field(ns("label_subtitle"), "Manual subtitle", placeholder = "Species 1, Species 2, ...",
                         stacked = TRUE))
          )
        ),
        when_on(ns, "label_show",
          field_group(
            "Placement",
            select_field(ns("label_position"), "Side", c("Left" = "left", "Right" = "right"),
                         default = "left"),
            select_field(ns("label_align"), "Align", ALIGNMENTS, default = "center"),
            text_field(ns("label_width"), "Width", default = "Left margin"),
            num_field(ns("label_x"), "X offset", step = 1, default = 0),
            num_field(ns("label_y"), "Y offset", step = 1, default = 0),
            switch_field(ns("label_wrap"), "Wrap long labels", TRUE)
          ),
          field_group("Font", font_fields(ns, "label_", weight = "bold")),
          field_group(
            "Subtitle",
            num_field(ns("label_subSize"), "Size (px)", min = 4, max = 72, step = 1, default = 11),
            select_field(ns("label_subStyle"), "Style", FONT_STYLES, default = "normal"),
            num_field(ns("label_spacing"), "Spacing", min = 0, step = 1, default = "Automatic")
          )
        )
      )
    )
  )
}

#' @export
server_titles <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    observeEvent(r$cluster_data, {
      cols <- names(r$cluster_data)
      update_block_select(session, "label_subtitleColumn", choices = c("Manual text" = NONE, cols),
                          selected = pick_choice(r, session$ns("label_subtitleColumn"),
                                                 input$label_subtitleColumn, c(NONE, cols), NONE))
    })
    reactive({
      list(
        chart = list(
          args = compact(list(
            show = isTRUE(input$chart_show),
            title = txt(input$chart_title),
            subtitle = txt(input$chart_subtitle)
          )),
          style = compact(list(
            align = opt(input$chart_align),
            height = txt(input$chart_height),
            titleFont = font_values(input, "chart_t_"),
            subtitleFont = font_values(input, "chart_s_")
          ))
        ),
        title = heading_values(input, "title_"),
        footer = heading_values(input, "footer_"),
        label = list(
          subtitleColumn = opt(input$label_subtitleColumn),
          args = compact(list(
            show = isTRUE(input$label_show),
            title = parse_delimited_input(txt(input$label_title)),
            subtitle = parse_delimited_input(txt(input$label_subtitle))
          )),
          style = c(
            compact(list(
              position = opt(input$label_position),
              align = opt(input$label_align),
              width = txt(input$label_width),
              spacing = num(input$label_spacing),
              x = num(input$label_x),
              y = num(input$label_y),
              wrapLabel = isTRUE(input$label_wrap),
              subtitleFont = compact(list(fontSize = px(input$label_subSize),
                                          fontStyle = opt(input$label_subStyle)))
            )),
            font_values(input, "label_")
          )
        )
      )
    })
  })
}
