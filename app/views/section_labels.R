#' Gene labels and legend sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observe, req, tags],
  shinyblocks[update_block_select],
  ../logic/ui_helpers[select_field, num_field, text_field, switch_field, field_row,
                      field_group, when_on, FONT_FAMILIES, FONT_WEIGHTS, FONT_STYLES,
                      ALIGNMENTS],
  ../logic/utils[num, txt, px, compact]
)

#' Font size, weight, style, family and colour fields
#' @export
font_fields <- function(ns, prefix = "", size = 12, weight = "normal", style = "normal",
                        family = "sans-serif", color = "black", show_style = TRUE) {
  id <- function(x) ns(paste0(prefix, x))
  tags$div(
    class = "gv-stack-sm",
    field_row(
      num_field(id("fontSize"), "Size (px)", size, min = 4, max = 72, step = 1),
      select_field(id("fontWeight"), "Weight", FONT_WEIGHTS, weight)
    ),
    field_row(
      if (show_style) select_field(id("fontStyle"), "Style", FONT_STYLES, style),
      select_field(id("fontFamily"), "Family", FONT_FAMILIES, family)
    ),
    text_field(id("fill"), "Color", color)
  )
}

#' The font of font_fields() as a style list
#' @export
font_values <- function(input, prefix = "") {
  get <- function(x) input[[paste0(prefix, x)]]
  compact(list(
    fontSize = px(get("fontSize")),
    fontWeight = get("fontWeight"),
    fontStyle = get("fontStyle"),
    fontFamily = get("fontFamily"),
    fill = txt(get("fill"))
  ))
}

#' @export
ui_labels <- function(id, data) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    switch_field(ns("show"), "Show gene labels"),
    when_on(ns, "show",
      select_field(ns("label"), "Label column", names(data), names(data)[1]),
      field_group("Font", font_fields(ns))
    )
  )
}

#' @export
server_labels <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(r$cluster_data)
      cols <- names(r$cluster_data)
      update_block_select(session, "label", choices = cols,
                          selected = if (isTRUE(input$label %in% cols)) input$label else cols[1])
    })
    reactive({
      list(
        args = compact(list(label = input$label, show = isTRUE(input$show))),
        style = font_values(input)
      )
    })
  })
}

#' @export
ui_legend <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    switch_field(ns("show"), "Show legend", TRUE),
    when_on(ns, "show",
      field_row(
        select_field(ns("position"), "Position", c("Top" = "top", "Bottom" = "bottom"), "bottom"),
        select_field(ns("align"), "Align", ALIGNMENTS, "left")
      ),
      field_row(
        num_field(ns("x"), "X offset", 0, step = 1),
        num_field(ns("y"), "Y offset", 0, step = 1)
      ),
      field_group(
        "Text",
        field_row(
          num_field(ns("fontSize"), "Size (px)", 12, min = 4, max = 72, step = 1),
          select_field(ns("fontFamily"), "Family", FONT_FAMILIES, "sans-serif")
        ),
        text_field(ns("fill"), "Color", "black")
      ),
      num_field(ns("cornerRadius"), "Marker corner radius", min = 0, step = 1)
    )
  )
}

#' @export
server_legend <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        args = list(show = isTRUE(input$show)),
        style = compact(list(
          position = input$position,
          align = input$align,
          x = num(input$x),
          y = num(input$y),
          corner_radius = num(input$cornerRadius),
          legendTextOptions = compact(list(
            fontSize = px(input$fontSize),
            fontFamily = input$fontFamily,
            fill = txt(input$fill)
          ))
        ))
      )
    })
  })
}
