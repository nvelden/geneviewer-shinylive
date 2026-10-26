#' Gene labels and legend sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observe, observeEvent, isolate, req, tags, tagList],
  shinyblocks[update_block_select],
  ../logic/ui_helpers[select_field, num_field, text_field, switch_field,
                      field_group, sections, when_on, group_style_fields, FONT_FAMILIES,
                      FONT_WEIGHTS, FONT_STYLES, ALIGNMENTS, NONE],
  ../logic/utils[num, txt, opt, px, compact, parse_group_styles, split_unquoted, pick_choice]
)

#' Font size, weight, style, family and colour fields, empty or "Default";
#' the arguments are geneviewer's defaults, shown in the fields
#' @export
font_fields <- function(ns, prefix = "", size = 12, weight = "normal", style = "normal",
                        family = "sans-serif", color = "black", show_style = TRUE) {
  id <- function(x) ns(paste0(prefix, x))
  tagList(
    num_field(id("fontSize"), "Size (px)", min = 4, max = 72, step = 1, default = size),
    select_field(id("fontWeight"), "Weight", FONT_WEIGHTS, default = weight),
    if (show_style) select_field(id("fontStyle"), "Style", FONT_STYLES, default = style),
    select_field(id("fontFamily"), "Family", FONT_FAMILIES, default = family),
    text_field(id("fill"), "Color", default = color)
  )
}

#' @export
font_values <- function(input, prefix = "") {
  get <- function(x) input[[paste0(prefix, x)]]
  compact(list(
    fontSize = px(get("fontSize")),
    fontWeight = opt(get("fontWeight")),
    fontStyle = opt(get("fontStyle")),
    fontFamily = opt(get("fontFamily")),
    fill = txt(get("fill"))
  ))
}

#' @export
ui_labels <- function(id, data) {
  ns <- NS(id)
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Show gene labels"),
      when_on(ns, "show",
        select_field(ns("label"), "Label column", names(data), names(data)[1]))
    ),
    when_on(ns, "show",
      field_group("Font", font_fields(ns, style = "italic")),
      field_group(
        "Position",
        num_field(ns("y"), "Vertical", step = 1,
                  description = "From 0 (bottom) to 100 (top) of the cluster. Empty: above the genes."),
        text_field(ns("dy"), "Vertical offset", default = "0em"),
        switch_field(ns("adjustLabels"), "Move overlapping labels", TRUE)
      ),
      field_group("Group styles", group_style_fields(ns, data, "tnsA, cas8f: fill = white"))
    )
  )
}

#' @export
server_labels <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    # Only when the data changes: reading the selection here would make the
    # update of the picker trigger this observer again
    observeEvent(r$cluster_data, isolate({
      req(r$cluster_data)
      cols <- names(r$cluster_data)
      update_block_select(session, "label", choices = cols,
                          selected = pick_choice(r, session$ns("label"), input$label, cols, cols[1]))
      update_block_select(session, "styleBy", choices = c("Group column" = NONE, cols),
                          selected = pick_choice(r, session$ns("styleBy"), input$styleBy,
                                                 c(NONE, cols), NONE))
    }))
    reactive({
      list(
        args = compact(list(
          label = input$label, show = isTRUE(input$show),
          style_by = opt(input$styleBy),
          groupStyle = parse_group_styles(input$groupStyle)
        )),
        style = c(
          font_values(input),
          compact(list(
            y = num(input$y),
            dy = txt(input$dy),
            adjustLabels = if (identical(input$adjustLabels, FALSE)) FALSE
          ))
        )
      )
    })
  })
}

#' @export
ui_legend <- function(id) {
  ns <- NS(id)
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Show legend", TRUE)
    ),
    when_on(ns, "show",
      field_group(
        "Placement",
        select_field(ns("position"), "Position", c("Top" = "top", "Bottom" = "bottom"),
                     default = "bottom"),
        select_field(ns("align"), "Align", ALIGNMENTS, default = "left"),
        num_field(ns("x"), "X offset", step = 1, default = 0),
        num_field(ns("y"), "Y offset", step = 1, default = 0)
      ),
      field_group(
        "Text",
        num_field(ns("fontSize"), "Size (px)", min = 4, max = 72, step = 1, default = 12),
        select_field(ns("fontFamily"), "Family", FONT_FAMILIES, default = "sans-serif"),
        text_field(ns("fill"), "Color", default = "black")
      ),
      field_group(
        "Markers",
        num_field(ns("cornerRadius"), "Corner radius", min = 0, step = 1, default = 0)
      ),
      field_group(
        "Order",
        text_field(ns("order"), "Groups in order", placeholder = "Order of the data",
                   description = "Groups separated by commas; the others follow. Quote names with commas.",
                   stacked = TRUE)
      ),
      field_group("Group styles", group_style_fields(ns, NULL, "tnsA, tnsB: fontStyle = italic",
                                                     with_style_by = FALSE))
    )
  )
}

#' @export
server_legend <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        args = compact(list(
          show = isTRUE(input$show),
          order = if (!is.null(txt(input$order))) split_unquoted(input$order),
          groupStyle = parse_group_styles(input$groupStyle)
        )),
        style = compact(list(
          position = opt(input$position),
          align = opt(input$align),
          x = num(input$x),
          y = num(input$y),
          corner_radius = num(input$cornerRadius),
          legendTextOptions = compact(list(
            fontSize = px(input$fontSize),
            fontFamily = opt(input$fontFamily),
            fill = txt(input$fill)
          ))
        ))
      )
    })
  })
}
