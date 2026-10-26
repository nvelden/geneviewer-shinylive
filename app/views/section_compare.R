#' Gene links, alignment, tooltip and size sections
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, observe, observeEvent, isolate, req, tags],
  shinyblocks[block_combobox, update_block_select, update_block_combobox],
  ../logic/ui_helpers[field, text_field, num_field, select_field, switch_field,
                      textarea_field, field_group, sections, when_on, register_input,
                      ALIGNMENTS],
  ../logic/utils[num, txt, compact, parse_delimited_input, pick_choice]
)

#' @export
ui_links <- function(id, data) {
  ns <- NS(id)
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Link genes between clusters"),
      when_on(ns, "show",
        select_field(ns("group"), "Match on", names(data), "class",
                     description = "Genes with the same value in this column are linked."),
        text_field(ns("value1"), "Values (first)", placeholder = "gene1, gene2, ...", stacked = TRUE),
        text_field(ns("value2"), "Values (second)", placeholder = "gene3, gene4, ...", stacked = TRUE)
      )
    ),
    when_on(ns, "show",
      field_group(
        "Colors",
        switch_field(ns("useGroupColors"), "Use the gene colors"),
        text_field(ns("normalColor"), "Same strand", default = "#969696"),
        text_field(ns("invertedColor"), "Opposite strand", default = "#d62728"),
        text_field(ns("lowColor"), "Lowest identity", default = "white"),
        num_field(ns("vmin"), "Min identity (%)", min = 0, max = 100, step = 1, default = 0),
        num_field(ns("vmax"), "Max identity (%)", min = 0, max = 100, step = 1, default = 100)
      ),
      field_group(
        "Shape",
        switch_field(ns("curve"), "Curved links", TRUE),
        num_field(ns("linkWidth"), "Width", min = 0, max = 1, step = 0.1, default = 1,
                  description = "Share of the gene length, from 0 to 1."),
        num_field(ns("opacity"), "Opacity", min = 0, max = 1, step = 0.1, default = 0.8),
        text_field(ns("stroke"), "Border color", default = "none"),
        num_field(ns("strokeWidth"), "Border width", min = 0, step = 0.1, default = 1)
      )
    )
  )
}

#' @export
server_links <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    # Only when the data changes: reading the selection here would make the
    # update of the picker trigger this observer again
    observeEvent(r$cluster_data, isolate({
      req(r$cluster_data)
      cols <- names(r$cluster_data)
      update_block_select(session, "group", choices = cols,
                          selected = pick_choice(r, session$ns("group"), input$group, cols, cols[1]))
    }))
    reactive({
      list(
        args = compact(list(
          show = isTRUE(input$show),
          group = input$group,
          value1 = parse_delimited_input(txt(input$value1)),
          value2 = parse_delimited_input(txt(input$value2))
        )),
        style = compact(list(
          use_group_colors = isTRUE(input$useGroupColors),
          normal_color = txt(input$normalColor),
          inverted_color = txt(input$invertedColor),
          low_color = txt(input$lowColor),
          vmin = num(input$vmin),
          vmax = num(input$vmax),
          curve = isTRUE(input$curve),
          linkWidth = num(input$linkWidth),
          linkStyle = compact(list(
            stroke = txt(input$stroke),
            strokeWidth = num(input$strokeWidth),
            fillOpacity = num(input$opacity)
          ))
        ))
      )
    })
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
                description = "Empty fills the preview."),
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
