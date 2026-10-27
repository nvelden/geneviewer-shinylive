#' Scale, scale bar, coordinates and sequence sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags, uiOutput, renderUI, observeEvent],
  shinyblocks[block_tabs, block_tab, update_block_switch],
  ../logic/ui_helpers[text_field, num_field, select_field, switch_field, field_row,
                      field_group, sections, when_on, FONT_FAMILIES, TICK_FORMATS],
  ../logic/setup[setup_notice, setup_action],
  ../logic/utils[num, txt, opt, px, compact]
)

line_fields <- function(ns, prefix, color, width = 1, length = NULL) {
  id <- function(x) ns(paste0(prefix, x))
  field_row(
    text_field(id("stroke"), "Color", default = color),
    num_field(id("strokeWidth"), "Width", min = 0, step = 0.5, default = width),
    if (!is.null(length)) num_field(id("lineLength"), "Length", min = 0, step = 1, default = length)
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
    num_field(id("fontSize"), "Size (px)", min = 4, max = 72, step = 1, default = size),
    select_field(id("fontFamily"), "Family", FONT_FAMILIES, default = family),
    text_field(id("fill"), "Color", default = color)
  )
}

text_values <- function(input, prefix) {
  get <- function(x) input[[paste0(prefix, x)]]
  compact(list(fontSize = px(get("fontSize")), fontFamily = opt(get("fontFamily")),
               fill = txt(get("fill"))))
}

#' @export
ui_scale <- function(id) {
  ns <- NS(id)
  block_tabs(
    id = ns("tab"),
    variant = "line",
    class = "gv-tabs",
    block_tab("Axis", value = "axis",
      sections(
        field_group(
          NULL,
          switch_field(ns("show"), "Show axis"),
          uiOutput(ns("axisSetup")),
          switch_field(ns("reverse"), "Reverse the scale")
        ),
        field_group(
          "Scale breaks",
          switch_field(ns("breaks"), "Break long gaps between genes"),
          when_on(ns, "breaks",
            num_field(ns("breakThreshold"), "Threshold (%)", min = 1, max = 100, step = 1,
                      default = 20),
            num_field(ns("breakPadding"), "Padding", min = 0, step = 0.5, default = 1),
            select_field(ns("breakPaddingUnit"), "Padding unit",
                         c("% of region" = "%", "Base pairs" = "bp"), "%")
          )
        ),
        field_group(
          "Range",
          num_field(ns("start"), "Start (bp)", step = 1000, placeholder = "First gene"),
          num_field(ns("end"), "End (bp)", step = 1000, placeholder = "Last gene")
        ),
        field_group(
          "Axis",
          select_field(ns("axisType"), "Values", c("Position" = "position", "Range" = "range"),
                       default = "position"),
          select_field(ns("axisPosition"), "Side", c("Bottom" = "bottom", "Top" = "top"),
                       default = "bottom"),
          num_field(ns("ticksCount"), "Ticks", min = 0, step = 1, default = 10),
          select_field(ns("ticksFormat"), "Format", TICK_FORMATS, default = ",.0f"),
          num_field(ns("padding"), "Padding (%)", min = 0, step = 1, default = 2),
          num_field(ns("y"), "Vertical", step = 1)
        ),
        field_group("Ticks", line_fields(ns, "tick_", "grey", length = 6)),
        field_group("Tick text", text_fields(ns, "text_", 10, "Arial")),
        field_group("Axis line", line_fields(ns, "line_", "grey"))
      )
    ),
    block_tab("Scale bar", value = "scalebar",
      sections(
        field_group(
          NULL,
          switch_field(ns("bar_show"), "Show scale bar"),
          uiOutput(ns("barSetup")),
          when_on(ns, "bar_show",
            text_field(ns("bar_title"), "Label", default = "1 kb"),
            num_field(ns("bar_unit"), "Length (bp)", min = 1, step = 100, default = 1000)
          )
        ),
        when_on(ns, "bar_show",
          field_group(
            "Placement",
            num_field(ns("bar_x"), "X offset", step = 1, default = 0),
            num_field(ns("bar_y"), "Y offset", step = 1, default = 10),
            select_field(ns("bar_labelPosition"), "Label side",
                         c("Left" = "left", "Right" = "right"), default = "left"),
            num_field(ns("bar_textPadding"), "Label padding", step = 1, default = 0)
          ),
          field_group("Label", text_fields(ns, "bar_text_", 10, "sans-serif")),
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
    output$axisSetup <- renderUI({
      if (!isTRUE(input$show)) setup_notice(
        "The position axis is hidden",
        "Turn on the axis to see positions and tick marks on each cluster.",
        setup_action(session$ns("enableAxis"), "Show axis"))
    })
    output$barSetup <- renderUI({
      if (!isTRUE(input$bar_show)) setup_notice(
        "The scale bar is hidden",
        "Show a reference length, such as 1 kb, below the chart.",
        setup_action(session$ns("enableBar"), "Show scale bar"))
    })
    observeEvent(input$enableAxis, update_block_switch(session, "show", checked = TRUE))
    observeEvent(input$enableBar, update_block_switch(session, "bar_show", checked = TRUE))
    reactive({
      padding <- num(input$breakPadding)
      if (!is.null(padding) && identical(input$breakPaddingUnit, "bp")) {
        # A number is a percentage; "500bp" is padding in base pairs
        padding <- paste0(padding, "bp")
      }
      list(
        args = compact(list(
          start = num(input$start),
          end = num(input$end),
          hidden = !isTRUE(input$show),
          reverse = isTRUE(input$reverse),
          scale_breaks = isTRUE(input$breaks)
        )),
        style = compact(list(
          scale_break_threshold = num(input$breakThreshold),
          scale_break_padding = padding,
          axis_type = opt(input$axisType),
          axis_position = opt(input$axisPosition),
          ticksCount = num(input$ticksCount),
          ticksFormat = opt(input$ticksFormat),
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
            labelStyle = c(compact(list(labelPosition = opt(input$bar_labelPosition))),
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
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Show gene coordinates"),
      when_on(ns, "show",
        select_field(ns("ticksFormat"), "Format", TICK_FORMATS, default = ",.0f"))
    ),
    when_on(ns, "show",
      field_group(
        "Placement",
        num_field(ns("yPositionTop"), "Top", step = 1, default = 53),
        num_field(ns("yPositionBottom"), "Bottom", step = 1, default = 48),
        num_field(ns("rotate"), "Rotation (\u00b0)", min = -90, max = 90, step = 5, default = -45),
        num_field(ns("overlapThreshold"), "Overlap limit", min = 0, step = 1, default = 20)
      ),
      field_group("Ticks", line_fields(ns, "tick_", "black", length = 6)),
      field_group("Text", text_fields(ns, "text_", 10, "Arial"))
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
          ticksFormat = opt(input$ticksFormat),
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
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Show the sequence line", TRUE),
      when_on(ns, "show", num_field(ns("y"), "Vertical", step = 1, default = 50))
    ),
    when_on(ns, "show",
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
