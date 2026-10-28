#' Selection section
#'
#' The style of one element on its own: a gene, gene label or coordinate, or
#' the title or subtitle of a cluster title, footer or label. A click on it in
#' the chart opens it here. Changes apply right away and go into the code as
#' GC_element_style() calls.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactiveVal, observeEvent, isolate, renderUI, uiOutput,
        tags, textInput, req],
  shinyblocks[block_input, block_select, block_switch, block_button, block_icon,
              update_block_input],
  ../logic/ui_helpers[color_swatches, field, field_group, sections, NONE, FONT_WEIGHTS, FONT_STYLES,
                      FONT_FAMILIES],
  ../logic/utils[num, txt, opt, px, compact]
)

# Border line styles, as stroke-dasharray
DASHES <- c("Solid" = "none", "Dashed" = "6 3", "Dotted" = "1.5 2")

MARKERS <- c("Arrow" = "arrow", "Box arrow" = "boxarrow", "Box" = "box",
             "Capsule" = "cbox", "Rounded box" = "rbox")

#' @export
ui_element <- function(id) {
  uiOutput(NS(id, "fields"))
}

#' The switch between the selected element and all elements of its kind. It
#' sits above the settings, in the same place in both, while one is selected.
#' @export
ui_element_scope <- function(id) {
  uiOutput(NS(id, "scope"))
}

# The kinds of element that can be selected, by component: their name and
# icon, and the section (and tab) with the settings of all of them
KINDS <- list(
  genes = list(name = "Gene", icon = "layers", all = "All genes", section = "genes"),
  labels = list(name = "Gene label", icon = "tag", all = "All gene labels", section = "labels"),
  coordinates = list(name = "Coordinate", icon = "hash", all = "All coordinates",
                     section = "coordinates"),
  clusterTitle = list(name = "Cluster title", icon = "file-text", all = "All cluster titles",
                      section = "titles", tab = "title"),
  clusterFooter = list(name = "Cluster footer", icon = "file-text", all = "All footers",
                       section = "titles", tab = "footer"),
  clusterLabel = list(name = "Cluster label", icon = "file-text", all = "All cluster labels",
                      section = "titles", tab = "label"),
  legend = list(name = "Legend label", icon = "tag", all = "All legend labels", section = "legend")
)
# Legend parts share the Legend settings section for their global styles.
kind_of <- function(item) {
  if (item$component == "legend" && identical(item$part, "marker")) {
    list(name = "Legend marker", icon = "layers", all = "All legend markers", section = "legend")
  } else KINDS[[item$component]]
}
# Identified by their row; legend parts by group, other text by cluster and part
BY_ROW <- c("genes", "labels", "coordinates")

# The fields are rebuilt for each element, so they are not registered as
# settings of the examples (see register_input())
# Display current values, while keeping a way to restore inherited settings.
text_input <- function(ns, name, label, value, drawn = NULL) {
  field(ns(name), label, block_input(ns(name), value = value))
}

num_input <- function(ns, name, label, value, drawn = NULL, ...) {
  field(ns(name), label, block_input(ns(name), value = value, type = "number", ...))
}

select_input <- function(ns, name, label, choices, value, drawn = NULL) {
  if (nzchar(value) && !value %in% choices) choices <- c(choices, stats::setNames(value, value))
  choices <- c(stats::setNames(NONE, "Use chart settings"), choices)
  field(ns(name), label, block_select(ns(name), choices = choices,
                                      selected = if (nzchar(value)) value else NONE, size = "sm"))
}

# Normalise computed CSS and overrides in the same way for display and comparison.
field_value <- function(name, x) {
  if (length(x) != 1 || is.na(x)) return("")
  if (is.numeric(x)) return(format(x, trim = TRUE, scientific = FALSE, digits = 15))
  x <- as.character(x)
  switch(name,
    fontSize = sub("px$", "", x),
    strokeDasharray = trimws(gsub(" +", " ", gsub(",", " ", gsub("px", "", x)))),
    fontWeight = switch(x, "400" = "normal", "700" = "bold", x),
    fontFamily = gsub("[\"']", "", trimws(strsplit(x, ",")[[1]][1])),
    x)
}

# Individual colour inputs use the app-wide swatches, with this module's updates.
color_input <- function(ns, name, label, value, drawn = NULL) {
  pick <- function(js_value) sprintf(
    "Shiny.setInputValue('%s', {field: '%s', value: %s}, {priority: 'event'})",
    ns("pick"), name, js_value)
  tags$div(class = "gv-color-field",
    text_input(ns, name, label, value, drawn),
    color_swatches(ns(name), label, pick))
}

#' @export
server_element <- function(id, data_key, open_section, section = function() NULL) {
  moduleServer(id, function(input, output, session) {
    edits <- reactiveVal(list())
    target <- reactiveVal(NULL)
    generation <- 0L
    key <- function(item) paste(item$component, item$cluster, item$row_id, item$end, item$part, item$group,
                                sep = ":")

    # Each element shown gets a token. Inputs that still come from the fields
    # of the previous element carry its token, and cannot change this one.
    show <- function(item) {
      generation <<- generation + 1L
      item$token <- as.character(generation)
      target(item)
    }
    # Whether a click or selection event is of an element this section edits
    selectable <- function(event) {
      if (is.null(event) || !isTRUE(event$component %in% names(KINDS))) return(FALSE)
      if (event$component == "legend") {
        length(event$data) == 1 && !is.na(event$data) &&
          isTRUE(event$part %in% c("label", "marker"))
      } else if (event$component %in% BY_ROW) !is.null(event$data$rowID) else !is.null(event$part)
    }
    open <- function(event) {
      if (!selectable(event)) return(FALSE)
      by_row <- event$component %in% BY_ROW
      # The first column that names the gene (of a coordinate), for the card
      gene <- if (event$component == "legend") NULL else if (event$component == "coordinates") event$data$gene else event$data
      gene <- gene[intersect(c("name", "label", "gene", "locus_tag", "product"), names(gene))]
      gene <- Filter(function(x) length(x) == 1 && !is.na(x) && nzchar(x), gene)
      item <- list(component = event$component, cluster = event$cluster,
                   row_id = if (by_row) event$data$rowID,
                   end = if (event$component == "coordinates") event$data$end,
                   part = if (!by_row) event$part,
                   group = if (event$component == "legend") event$data,
                   gene = if (length(gene)) as.character(gene[[1]]),
                   drawn = event$style)
      current <- isolate(target())
      if (is.null(current) || key(current) != key(item)) show(item)
      open_section("element")
      TRUE
    }

    # The name of the element and what it is, for the card and the switch
    name_of <- function(item) {
      text <- item$drawn$text
      if (item$component == "legend") {
        return(if (length(text) == 1 && nzchar(text)) text else as.character(item$group))
      }
      if (item$component %in% c("genes", "labels")) {
        if (is.null(item$gene)) paste("Row", item$row_id) else item$gene
      } else if (length(text) == 1 && nzchar(text)) text else KINDS[[item$component]]$name
    }
    what_of <- function(item) {
      kind <- kind_of(item)$name
      switch(item$component,
        legend = paste(kind, "\u00b7 group", item$group),
        coordinates = paste(if (identical(item$end, "end")) "End" else "Start", "coordinate",
                            if (!is.null(item$gene)) paste("\u00b7 gene", item$gene)),
        paste(c(kind, if (identical(item$part, "subtitle")) "subtitle", "\u00b7 cluster", item$cluster),
              collapse = " "))
    }

    # Row IDs belong to one dataset and mapping. Never carry edits to new data.
    observeEvent(data_key(), {
      edits(list())
      target(NULL)
    }, ignoreInit = TRUE)

    output$fields <- renderUI({
      item <- target()
      ns <- session$ns
      if (is.null(item)) {
        n <- length(edits())
        return(tags$div(
          class = "gv-element-empty",
          block_icon("edit", size = "sm"),
          tags$p("Click a gene, label, coordinate, cluster title or legend part in the preview to style it on its own."),
          if (n > 0) tags$div(
            tags$p(class = "gv-element-count",
                   if (n == 1) "1 element has its own style." else sprintf("%d elements have their own style.", n)),
            block_button("Reset all", variant = "outline", size = "sm", icon = "refresh-cw",
                         id = ns("resetAll"))
          )
        ))
      }
      style <- isolate(edits()[[key(item)]]$style)
      drawn <- function(name) field_value(name, item$drawn[[name]])
      value <- function(name) {
        if (!is.null(style[[name]])) field_value(name, style[[name]]) else drawn(name)
      }
      gene <- item$component == "genes"
      legend_marker <- item$component == "legend" && identical(item$part, "marker")
      # Text can be changed and hidden; a coordinate can only be hidden
      font <- field_group("Font",
        color_input(ns, "fill", "Color", value("fill"), drawn("fill")),
        num_input(ns, "fontSize", "Size (px)", value("fontSize"), min = 4, max = 72, step = 1),
        select_input(ns, "fontWeight", "Weight", FONT_WEIGHTS, value("fontWeight"), drawn("fontWeight")),
        select_input(ns, "fontStyle", "Style", FONT_STYLES, value("fontStyle"), drawn("fontStyle")),
        select_input(ns, "fontFamily", "Family", FONT_FAMILIES, value("fontFamily"),
                     drawn("fontFamily"))
      )
      hide <- function(label, value) tags$div(class = "gv-switch-row",
        block_switch(ns("hide"), label, value = value, size = "sm"))
      text <- field_group("Text",
        text_input(ns, "text", "Text", if (identical(style$text, "")) "" else value("text"),
                   drawn("text")),
        hide(paste("Hide this", if (item$component == "labels") "label" else item$part),
             identical(style$text, ""))
      )

      sections(
        tags$div(class = "gv-element-top", tags$div(
          class = "gv-dataset gv-element-card",
          tags$div(class = "gv-dataset-icon", block_icon(kind_of(item)$icon, size = "sm")),
          tags$div(
            class = "gv-dataset-text",
            tags$div(class = "gv-dataset-name", name_of(item)),
            tags$div(class = "gv-dataset-meta", what_of(item))
          ),
          tags$div(class = "gv-element-reset",
                   block_button("Reset", variant = "ghost", size = "sm", icon = "refresh-cw",
                                id = ns("reset")))
        ),
        tags$p(class = "gv-element-hint",
               "Current values are shown. Changes apply right away; clear a field to use the chart settings.")),
        tags$div(hidden = NA, textInput(ns("token"), NULL, item$token)),
        if (legend_marker) tags$div(
          field_group("Fill",
            color_input(ns, "fill", "Color", value("fill"), drawn("fill")),
            num_input(ns, "opacity", "Opacity", value("opacity"), drawn("opacity"), min = 0, max = 1, step = 0.1)
          ),
          field_group("Border",
            color_input(ns, "stroke", "Color", value("stroke"), drawn("stroke")),
            num_input(ns, "strokeWidth", "Width", value("strokeWidth"), drawn("strokeWidth"), min = 0, step = 0.5),
            select_input(ns, "strokeDasharray", "Line", DASHES, value("strokeDasharray"), drawn("strokeDasharray"))
          ),
          tags$p(class = "gv-element-hint", "These settings change the legend patch. Gene colours are set under Colors.")
        ) else if (gene) tags$div(
          field_group("Fill",
            color_input(ns, "fill", "Color", value("fill"), drawn("fill")),
            num_input(ns, "opacity", "Opacity", value("opacity"), drawn("opacity"), min = 0, max = 1, step = 0.1)
          ),
          field_group("Shape",
            select_input(ns, "marker", "Marker", MARKERS, value("marker"), drawn("marker")),
            select_input(ns, "markerSize", "Size",
                         c("Small" = "small", "Medium" = "medium", "Large" = "large"),
                         value("markerSize"), drawn("markerSize")),
            num_input(ns, "markerHeight", "Height", value("markerHeight"), drawn("markerHeight"), min = 1, step = 1),
            num_input(ns, "cornerRadius", "Corner radius", value("cornerRadius"), drawn("cornerRadius"), min = 0, step = 1),
            tags$p(class = "gv-element-hint",
                   "The length of a gene comes from its start and end positions.")
          ),
          field_group("Arrowhead",
            num_input(ns, "arrowheadWidth", "Width", value("arrowheadWidth"), drawn("arrowheadWidth"), min = 0, step = 1),
            num_input(ns, "arrowheadHeight", "Height", value("arrowheadHeight"), drawn("arrowheadHeight"), min = 0, step = 1)
          ),
          field_group("Border",
            color_input(ns, "stroke", "Color", value("stroke"), drawn("stroke")),
            num_input(ns, "strokeWidth", "Width", value("strokeWidth"), drawn("strokeWidth"), min = 0, step = 0.5),
            select_input(ns, "strokeDasharray", "Line", DASHES, value("strokeDasharray"),
                         drawn("strokeDasharray"))
          ),
          field_group("Position",
            num_input(ns, "y", "Vertical", value("y"), drawn("y"), min = 0, max = 100, step = 1),
            tags$p(class = "gv-element-hint",
                   "From 0 (bottom) to 100 (top) of the cluster. The gene's label stays in place.")
          )
        ) else if (item$component == "labels") tags$div(
          text, font,
          field_group("Position",
            num_input(ns, "dx", "Move right", value("dx"), step = 1),
            num_input(ns, "dy", "Move down", value("dy"), step = 1),
            num_input(ns, "rotate", "Rotation", value("rotate"), min = -360, max = 360, step = 5)
          )
        ) else if (item$component == "coordinates") tags$div(
          field_group(NULL, hide("Hide this coordinate", isFALSE(style$show))), font
        ) else tags$div(text, font)
      )
    })

    read_style <- function(item) {
      previous <- isolate(edits()[[key(item)]]$style)
      style <- if (item$component == "legend" && identical(item$part, "marker")) list(
        fill = txt(input$fill), opacity = num(input$opacity),
        stroke = txt(input$stroke), strokeWidth = num(input$strokeWidth),
        strokeDasharray = opt(input$strokeDasharray)
      ) else if (item$component == "genes") list(
        fill = txt(input$fill), opacity = num(input$opacity),
        marker = opt(input$marker), markerSize = opt(input$markerSize),
        markerHeight = num(input$markerHeight), cornerRadius = num(input$cornerRadius),
        arrowheadWidth = num(input$arrowheadWidth), arrowheadHeight = num(input$arrowheadHeight),
        stroke = txt(input$stroke), strokeWidth = num(input$strokeWidth),
        strokeDasharray = opt(input$strokeDasharray), y = num(input$y)
      ) else c(
        if (item$component == "coordinates") list(show = if (isTRUE(input$hide)) FALSE)
        else list(text = if (isTRUE(input$hide)) "" else txt(input$text)),
        list(fill = txt(input$fill), fontSize = px(input$fontSize),
             fontWeight = opt(input$fontWeight), fontStyle = opt(input$fontStyle),
             fontFamily = opt(input$fontFamily)),
        if (item$component == "labels")
          list(dx = num(input$dx), dy = num(input$dy), rotate = num(input$rotate))
      )
      style <- compact(style)
      # Populating a field must not turn inherited styling into an override.
      # Keep previous edits, and record only newly changed values.
      for (name in names(style)) {
        if (is.null(previous[[name]]) && !is.null(item$drawn[[name]]) &&
            identical(field_value(name, style[[name]]), field_value(name, item$drawn[[name]]))) {
          style[name] <- NULL
        }
      }
      style
    }
    observeEvent(list(input$token, input$fill, input$opacity, input$marker,
                      input$markerSize, input$markerHeight, input$cornerRadius,
                      input$arrowheadWidth, input$arrowheadHeight, input$stroke,
                      input$strokeWidth, input$strokeDasharray, input$y, input$text,
                      input$hide, input$fontSize, input$fontWeight, input$fontStyle,
                      input$fontFamily,
                      input$dx, input$dy, input$rotate), {
      item <- target()
      if (is.null(item) || !identical(input$token, item$token)) return()
      style <- read_style(item)
      all <- edits()
      # What identifies the element in GC_element_style(): its row (and end),
      # or its cluster and part
      all[[key(item)]] <- if (length(style)) {
        list(component = item$component,
             id = compact(list(row_id = item$row_id, end = item$end,
                               cluster = if (!is.null(item$part)) item$cluster, part = item$part,
                               group = item$group)),
             style = style)
      }
      if (!identical(all, edits())) edits(all)
    }, ignoreInit = TRUE)

    # A swatch fills in its colour field, as if typed
    observeEvent(input$pick, {
      req(target(), input$pick$field %in% c("fill", "stroke"))
      update_block_input(session, input$pick$field, value = input$pick$value)
    })
    observeEvent(input$reset, {
      item <- req(target())
      all <- edits()
      all[[key(item)]] <- NULL
      edits(all)
      show(item)
    })
    observeEvent(input$resetAll, edits(list()))
    observeEvent(input$scope, {
      item <- req(target())
      kind <- kind_of(item)
      if (identical(input$scope, "all")) open_section(kind$section, kind$tab)
      else open_section("element")
    })

    output$scope <- renderUI({
      item <- target()
      if (is.null(item)) return(NULL)
      current <- section()
      kind <- kind_of(item)
      all <- kind$section
      if (!isTRUE(current %in% c("element", all))) return(NULL)
      segment <- function(value, icon, label, active) tags$button(
        type = "button", class = "gv-scope-option", `aria-pressed` = tolower(active),
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          session$ns("scope"), value),
        block_icon(icon, size = "sm"), tags$span(label)
      )
      tags$div(
        class = "gv-scope", role = "group", `aria-label` = "What to edit",
        segment("element", kind$icon, name_of(item), current == "element"),
        segment("all", "list", kind$all, current == all)
      )
    })

    list(open = open, selectable = selectable, clear = function() target(NULL),
         edits = function() unname(edits()), target = target)
  })
}
