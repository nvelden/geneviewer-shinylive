#' Scale, scale bar, coordinates and sequence sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  shinyblocks[block_tabs, block_tab],
  ../logic/ui_helpers[text_field, num_field, select_field, switch_field, field_row,
                      field_group, when_on, FONT_FAMILIES, TICK_FORMATS],
  ../logic/utils[num, txt, px, compact]
)

line_fields <- function(ns, prefix, color, width = 1, length = NULL) {
  id <- function(x) ns(paste0(prefix, x))
  field_row(
    text_field(id("stroke"), "Color", color),
    num_field(id("strokeWidth"), "Width", width, min = 0, step = 0.5),
    if (!is.null(length)) num_field(id("lineLength"), "Length", length, min = 0, step = 1)
  )
}

line_values <- function(input, prefix) {
  get <- function(x) input[[paste0(prefix, x)]]
  compact(list(stroke = txt(get("stroke")), strokeWidth = num(get("strokeWidth")),
               lineLength = num(get("lineLength"))))
}

text_fields <- function(ns, prefix, size, family, color = "black") {
  id <- function(x) ns(paste0(prefix, x))
  field_row(
    num_field(id("fontSize"), "Size (px)", size, min = 4, max = 72, step = 1),
    select_field(id("fontFamily"), "Family", FONT_FAMILIES, family),
    text_field(id("fill"), "Color", color)
  )
}

text_values <- function(input, prefix) {
  get <- function(x) input[[paste0(prefix, x)]]
  compact(list(fontSize = px(get("fontSize")), fontFamily = get("fontFamily"),
               fill = txt(get("fill"))))
}

#' @export
ui_scale <- function(id) {
  ns <- NS(id)
  block_tabs(
    id = ns("tab"),
    variant = "line",
    block_tab("Axis", value = "axis",
      tags$div(
        class = "gv-stack",
        switch_field(ns("show"), "Show axis"),
        switch_field(ns("reverse"), "Reverse the scale"),
        switch_field(ns("breaks"), "Break long gaps between genes"),
        when_on(ns, "breaks",
          field_row(
            num_field(ns("breakThreshold"), "Gap threshold (%)", 20, min = 1, max = 100, step = 1),
            num_field(ns("breakPadding"), "Padding", 1, min = 0, step = 0.5),
            select_field(ns("breakPaddingUnit"), "Padding unit",
                         c("% of region" = "%", "Base pairs" = "bp"), "%")
          )
        ),
        field_row(
          select_field(ns("axisType"), "Axis values", c("Position" = "position", "Range" = "range"), "position"),
          select_field(ns("axisPosition"), "Axis side", c("Bottom" = "bottom", "Top" = "top"), "bottom")
        ),
        field_row(
          num_field(ns("ticksCount"), "Ticks", 10, min = 0, step = 1),
          select_field(ns("ticksFormat"), "Tick format", TICK_FORMATS, ",.0f")
        ),
        field_row(
          num_field(ns("padding"), "Padding (%)", 2, min = 0, step = 1),
          num_field(ns("y"), "Vertical position", step = 1)
        ),
        field_group("Ticks", line_fields(ns, "tick_", "grey", length = 6)),
        field_group("Tick text", text_fields(ns, "text_", 10, "Arial")),
        field_group("Axis line", line_fields(ns, "line_", "grey"))
      )
    ),
    block_tab("Scale bar", value = "scalebar",
      tags$div(
        class = "gv-stack",
        switch_field(ns("bar_show"), "Show scale bar"),
        when_on(ns, "bar_show",
          field_row(
            text_field(ns("bar_title"), "Label", "1 kb"),
            num_field(ns("bar_unit"), "Length (bp)", 1000, min = 1, step = 100)
          ),
          field_row(
            num_field(ns("bar_x"), "X offset", 0, step = 1),
            num_field(ns("bar_y"), "Y offset", 10, step = 1),
            select_field(ns("bar_labelPosition"), "Label side", c("Left" = "left", "Right" = "right"), "left")
          ),
          field_group("Label", text_fields(ns, "bar_text_", 10, "sans-serif"),
                      num_field(ns("bar_textPadding"), "Label padding", 0, step = 1)),
          field_group("Line", line_fields(ns, "bar_line_", "grey")),
          field_group("Ticks", line_fields(ns, "bar_tick_", "grey"))
        )
      )
    )
  )
}

#' @export
server_scale <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      padding <- num(input$breakPadding)
      if (!is.null(padding) && identical(input$breakPaddingUnit, "bp")) {
        # A number is a percentage; "500bp" is padding in base pairs
        padding <- paste0(padding, "bp")
      }
      list(
        args = list(
          hidden = !isTRUE(input$show),
          reverse = isTRUE(input$reverse),
          scale_breaks = isTRUE(input$breaks)
        ),
        style = compact(list(
          scale_break_threshold = num(input$breakThreshold),
          scale_break_padding = padding,
          axis_type = input$axisType,
          axis_position = input$axisPosition,
          ticksCount = num(input$ticksCount),
          ticksFormat = input$ticksFormat,
          padding = num(input$padding),
          y = num(input$y),
          tickStyle = line_values(input, "tick_"),
          textStyle = text_values(input, "text_"),
          lineStyle = line_values(input, "line_")
        )),
        bar = list(
          args = compact(list(
            show = isTRUE(input$bar_show),
            title = txt(input$bar_title),
            scaleBarUnit = num(input$bar_unit)
          )),
          style = compact(list(
            x = num(input$bar_x),
            y = num(input$bar_y),
            textPadding = num(input$bar_textPadding),
            labelStyle = c(compact(list(labelPosition = input$bar_labelPosition)),
                           text_values(input, "bar_text_")),
            scaleBarLineStyle = line_values(input, "bar_line_"),
            scaleBarTickStyle = line_values(input, "bar_tick_")
          ))
        )
      )
    })
  })
}

#' @export
ui_coordinates <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    switch_field(ns("show"), "Show gene coordinates"),
    when_on(ns, "show",
      select_field(ns("ticksFormat"), "Number format", TICK_FORMATS, ",.0f"),
      field_row(
        num_field(ns("yPositionTop"), "Top position", 55, step = 1),
        num_field(ns("yPositionBottom"), "Bottom position", 45, step = 1)
      ),
      field_row(
        num_field(ns("rotate"), "Rotation (°)", -45, min = -90, max = 90, step = 5),
        num_field(ns("overlapThreshold"), "Overlap threshold", 20, min = 0, step = 1)
      ),
      field_group("Ticks", line_fields(ns, "tick_", "black", length = 5)),
      field_group("Text", text_fields(ns, "text_", 12, "sans-serif"))
    )
  )
}

#' @export
server_coordinates <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        args = list(show = isTRUE(input$show)),
        style = compact(list(
          ticksFormat = input$ticksFormat,
          yPositionTop = num(input$yPositionTop),
          yPositionBottom = num(input$yPositionBottom),
          rotate = num(input$rotate),
          overlapThreshold = num(input$overlapThreshold),
          tickStyle = line_values(input, "tick_"),
          textStyle = text_values(input, "text_")
        ))
      )
    })
  })
}

#' @export
ui_sequence <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    switch_field(ns("show"), "Show the sequence line", TRUE),
    when_on(ns, "show",
      num_field(ns("y"), "Vertical position", 50, step = 1),
      field_group("Line", line_fields(ns, "seq_", "grey")),
      field_group("Break markers", line_fields(ns, "marker_", "grey"))
    )
  )
}

#' @export
server_sequence <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        args = list(show = isTRUE(input$show)),
        style = compact(list(
          y = num(input$y),
          sequenceStyle = line_values(input, "seq_"),
          markerStyle = line_values(input, "marker_")
        ))
      )
    })
  })
}
