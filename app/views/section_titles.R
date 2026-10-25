#' Titles section
#'
#' The title and footer above and below each cluster, and the cluster labels
#' beside them.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  shinyblocks[block_tabs, block_tab],
  ../logic/ui_helpers[text_field, num_field, select_field, switch_field,
                      field_group, sections, when_on, ALIGNMENTS],
  ../logic/utils[num, txt, px, compact, parse_delimited_input],
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
        select_field(ns(id("align")), "Align", ALIGNMENTS, defaults$align),
        text_field(ns(id("height")), "Height", defaults$height),
        num_field(ns(id("x")), "X offset", 0, step = 1),
        num_field(ns(id("y")), "Y offset", defaults$y, step = 1),
        num_field(ns(id("spacing")), "Line spacing", defaults$spacing, min = 0, step = 1)
      ),
      field_group("Text font", font_fields(ns, id("t_"), size = defaults$size,
                                           weight = defaults$weight, show_style = FALSE)),
      field_group("Subtitle font", font_fields(ns, id("s_"), size = defaults$subSize,
                                               color = defaults$subColor, show_style = FALSE))
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
      align = get("align"),
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
ui_titles <- function(id) {
  ns <- NS(id)
  block_tabs(
    id = ns("tab"),
    variant = "line",
    class = "gv-tabs",
    block_tab("Title", value = "title",
      heading_fields(ns, "title_", "title",
                     list(align = "center", height = "40px", y = 5, spacing = 20,
                          size = 16, weight = "bold", subSize = 14, subColor = "black"))
    ),
    block_tab("Footer", value = "footer",
      heading_fields(ns, "footer_", "footer",
                     list(align = "center", height = "40px", y = 5, spacing = 20,
                          size = 14, weight = "normal", subSize = 12, subColor = "grey"))
    ),
    block_tab("Cluster label", value = "label",
      sections(
        field_group(
          NULL,
          switch_field(ns("label_show"), "Show cluster labels"),
          when_on(ns, "label_show",
            text_field(ns("label_title"), "Text", placeholder = "Label 1, Label 2, ...",
                       description = "Empty uses the cluster names.", stacked = TRUE),
            text_field(ns("label_subtitle"), "Subtitle", placeholder = "Species 1, Species 2, ...",
                       stacked = TRUE)
          )
        ),
        when_on(ns, "label_show",
          field_group(
            "Placement",
            select_field(ns("label_position"), "Side", c("Left" = "left", "Right" = "right"), "left"),
            select_field(ns("label_align"), "Align", ALIGNMENTS, "center"),
            text_field(ns("label_width"), "Width", "100px"),
            num_field(ns("label_x"), "X offset", 0, step = 1),
            num_field(ns("label_y"), "Y offset", 0, step = 1),
            switch_field(ns("label_wrap"), "Wrap long labels", TRUE)
          ),
          field_group("Font", font_fields(ns, "label_", weight = "bold")),
          field_group(
            "Subtitle",
            num_field(ns("label_subSize"), "Size (px)", 11, min = 4, max = 72, step = 1),
            num_field(ns("label_spacing"), "Spacing", min = 0, step = 1)
          )
        )
      )
    )
  )
}

#' @export
server_titles <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        title = heading_values(input, "title_"),
        footer = heading_values(input, "footer_"),
        label = list(
          args = compact(list(
            show = isTRUE(input$label_show),
            title = parse_delimited_input(txt(input$label_title)),
            subtitle = parse_delimited_input(txt(input$label_subtitle))
          )),
          style = c(
            compact(list(
              position = input$label_position,
              align = input$label_align,
              width = txt(input$label_width),
              spacing = num(input$label_spacing),
              x = num(input$label_x),
              y = num(input$label_y),
              wrapLabel = isTRUE(input$label_wrap),
              subtitleFont = compact(list(fontSize = px(input$label_subSize)))
            )),
            font_values(input, "label_")
          )
        )
      )
    })
  })
}
