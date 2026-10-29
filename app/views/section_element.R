#' Element section
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
  scale = list(name = "Scale axis", icon = "hash", all = "Axis settings", section = "scale", tab = "axis"),
  scaleBar = list(name = "Scale bar", icon = "hash", all = "Scale bar settings", section = "scale", tab = "scalebar"),
  title = list(name = "Chart title", icon = "file-text", all = "Chart title settings", section = "titles", tab = "chart"),
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
WHOLE_SCALE <- c("scale", "scaleBar")
DRAGGABLE <- c("labels", "title", "clusterTitle", "clusterFooter", "clusterLabel", "legend", WHOLE_SCALE)

# The fields are rebuilt for each element, so they are not registered as
# settings of the examples (see register_input())
# Display current values, while keeping a way to restore inherited settings.
text_input <- function(ns, name, label, value) {
  field(ns(name), label, block_input(ns(name), value = value))
}

num_input <- function(ns, name, label, value, ...) {
  field(ns(name), label, block_input(ns(name), value = value, type = "number", ...))
}

select_input <- function(ns, name, label, choices, value) {
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
color_input <- function(ns, name, label, value) {
  pick <- function(js_value) sprintf(
    "Shiny.setInputValue('%s', {field: '%s', value: %s}, {priority: 'event'})",
    ns("pick"), name, js_value)
  tags$div(class = "gv-color-field",
    text_input(ns, name, label, value),
    color_swatches(ns(name), label, pick))
}

# Columns that usually name a gene, in order; matched in any case
NAME_COLUMNS <- c("name", "label", "gene", "locus_tag", "product")

# The name of a gene (a row of the data) for the element card: its value in
# the chart's label column, else in the first name column that has one
gene_name <- function(gene, label_column = NULL) {
  if (!is.list(gene) || is.null(names(gene))) return(NULL)
  columns <- names(gene)
  # The label column exactly as chosen; the name columns in any case, in order
  by_name <- columns[tolower(columns) %in% NAME_COLUMNS]
  by_name <- by_name[order(match(tolower(by_name), NAME_COLUMNS))]
  for (column in c(intersect(label_column, columns), by_name)) {
    value <- gene[[column]]
    if (length(value) == 1 && !is.na(value) && nzchar(as.character(value))) return(as.character(value))
  }
  NULL
}

#' @export
server_element <- function(id, data_key, open_section, section = function() NULL,
                           label_column = function() NULL) {
  moduleServer(id, function(input, output, session) {
    edits <- reactiveVal(list())
    target <- reactiveVal(NULL)
    # Several selected elements (Shift-click or a box in the chart): their
    # items, the token of their fields and the position the fields show
    group <- reactiveVal(NULL)
    group_position <- list()
    generation <- 0L
    key <- function(item) paste(item$component, item$cluster, item$row_id, item$end, item$part, item$group,
                                sep = ":")

    edit_id <- function(item) compact(list(
      row_id = item$row_id, end = item$end,
      cluster = if (!is.null(item$part) || item$component %in% WHOLE_SCALE) item$cluster, part = item$part, group = item$group))

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
      } else if (event$component %in% WHOLE_SCALE) !is.null(event$cluster)
      else if (event$component %in% BY_ROW) !is.null(event$data$rowID) else !is.null(event$part)
    }
    # What the section keeps of a selected element
    as_item <- function(event) {
      by_row <- event$component %in% BY_ROW
      # The gene (of a coordinate), named for the card
      gene <- if (event$component == "legend") NULL else if (event$component == "coordinates") event$data$gene else event$data
      list(component = event$component, cluster = event$cluster,
           row_id = if (by_row) event$data$rowID,
           end = if (event$component == "coordinates") event$data$end,
           part = if (!by_row) event$part,
           group = if (event$component == "legend") event$data,
           gene = gene_name(gene, isolate(label_column())),
           drawn = event$style)
    }
    open <- function(event, navigate = TRUE) {
      if (!selectable(event)) return(FALSE)
      item <- as_item(event)
      current <- isolate(target())
      changed_target <- is.null(current) || key(current) != key(item)
      # Own edits already have their values in the fields. Rebuild only when
      # inherited values change, keeping focus while typing an override.
      style <- isolate(edits()[[key(item)]]$style)
      inherited <- setdiff(union(names(current$drawn), names(item$drawn)), names(style))
      # Text opacity can change during hover; it is not a text editor field.
      if (item$component != "genes" &&
          !(item$component == "legend" && identical(item$part, "marker"))) {
        inherited <- setdiff(inherited, "opacity")
      }
      # Moves refresh the fields themselves (move(), place()): a redraw after
      # clearing one position field must not rebuild the other while typed in
      inherited <- setdiff(inherited, c("moveX", "moveY"))
      changed_style <- any(vapply(inherited, function(name) {
        !identical(field_value(name, current$drawn[[name]]), field_value(name, item$drawn[[name]]))
      }, logical(1)))
      if (changed_target || changed_style) show(item)
      if (navigate || changed_target) open_section("element")
      TRUE
    }

    # A value all of them share, or "" when they differ
    common <- function(items, name) {
      values <- unique(vapply(items, function(item) field_value(name, item$drawn[[name]] %||% 0), ""))
      if (length(values) == 1) values else ""
    }
    position_of <- function(items) {
      movable <- Filter(function(item) item$component %in% DRAGGABLE, items)
      list(moveX = common(movable, "moveX"), moveY = common(movable, "moveY"))
    }
    open_group <- function(events) {
      events <- Filter(selectable, if (is.null(events)) list() else events)
      if (length(events) < 2) {
        group(NULL)
        return(invisible(FALSE))
      }
      items <- lapply(events, as_item)
      current <- isolate(group())
      position <- position_of(items)
      # The same elements where the fields put them: keep the fields, also
      # while one is typed in
      if (!is.null(current) && identical(vapply(current$items, key, ""), vapply(items, key, "")) &&
          identical(position, group_position)) return(invisible(TRUE))
      generation <<- generation + 1L
      group_position <<- position
      group(list(items = items, token = as.character(generation)))
      if (is.null(current)) open_section("element")
      invisible(TRUE)
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
        title = paste(kind, if (identical(item$part, "subtitle")) "subtitle"),
        legend = paste(kind, "\u00b7 group", item$group),
        coordinates = paste(if (identical(item$end, "end")) "End" else "Start", "coordinate",
                            if (!is.null(item$gene)) paste("\u00b7 gene", item$gene)),
        paste(c(kind, if (identical(item$part, "subtitle")) "subtitle", "\u00b7 cluster", item$cluster),
              collapse = " "))
    }

    # The fields of the shown element follow moves made in the chart
    refresh <- function() {
      item <- isolate(target())
      if (is.null(item)) return()
      if (item$component %in% DRAGGABLE) {
        style <- isolate(edits())[[key(item)]]$style
        item$drawn$moveX <- style$moveX %||% 0
        item$drawn$moveY <- style$moveY %||% 0
      }
      show(item)
    }
    # Row IDs belong to one dataset and mapping. Never carry edits to new data.
    observeEvent(data_key(), {
      edits(list())
      target(NULL)
      group(NULL)
    }, ignoreInit = TRUE)

    plural <- function(x) if (grepl("axis$", x)) sub("axis$", "axes", x) else paste0(x, "s")
    group_fields <- function(selection) {
      ns <- session$ns
      items <- selection$items
      kinds <- unique(vapply(items, function(item) kind_of(item)$name, ""))
      names <- vapply(utils::head(items, 3), name_of, "")
      movable <- any(vapply(items, function(item) item$component %in% DRAGGABLE, logical(1)))
      position <- function(id, name, label) field(ns(id), label, block_input(
        ns(id), value = group_position[[name]], placeholder = "Mixed", type = "number", step = 1),
        stacked = TRUE)
      sections(
        tags$div(class = "gv-element-top", tags$div(
          class = "gv-dataset gv-element-card",
          tags$div(class = "gv-dataset-icon", block_icon(if (length(kinds) == 1) kind_of(items[[1]])$icon else "layers", size = "sm")),
          tags$div(
            class = "gv-dataset-text",
            tags$div(class = "gv-dataset-name",
                     paste(length(items), if (length(kinds) == 1) tolower(plural(kinds)) else "elements")),
            tags$div(class = "gv-dataset-meta",
                     paste0(paste(names, collapse = ", "), if (length(items) > 3) ", \u2026"))
          ),
          tags$div(class = "gv-element-reset",
                   block_button("Reset", variant = "ghost", size = "sm", icon = "refresh-cw",
                                id = ns("groupReset")))
        ),
        tags$p(class = "gv-element-hint",
               if (movable) "Changes apply to all of them. Drag one in the chart, or use the arrow keys, to move them together."
               else "Select one of them to change its style.")),
        tags$div(hidden = NA, textInput(ns("groupToken"), NULL, selection$token)),
        if (movable) field_group("Position",
          tags$div(class = "gv-field-pair",
            position("groupMoveX", "moveX", "Right (px)"),
            position("groupMoveY", "moveY", "Down (px)")))
      )
    }

    output$fields <- renderUI({
      selection <- group()
      if (!is.null(selection)) return(group_fields(selection))
      item <- target()
      ns <- session$ns
      if (is.null(item)) {
        n <- length(edits())
        return(tags$div(
          class = "gv-element-empty",
          block_icon("edit", size = "sm"),
          tags$p("Nothing selected."),
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
      fill <- function() field_group("Fill",
        color_input(ns, "fill", "Color", value("fill")),
        num_input(ns, "opacity", "Opacity", value("opacity"), min = 0, max = 1, step = 0.1)
      )
      border <- function() field_group("Border",
        color_input(ns, "stroke", "Color", value("stroke")),
        num_input(ns, "strokeWidth", "Width", value("strokeWidth"), min = 0, step = 0.5),
        select_input(ns, "strokeDasharray", "Line", DASHES, value("strokeDasharray"))
      )
      # Text can be changed and hidden; a coordinate can only be hidden
      font <- field_group("Font",
        color_input(ns, "fill", "Color", value("fill")),
        num_input(ns, "fontSize", "Size (px)", value("fontSize"), min = 4, max = 72, step = 1),
        select_input(ns, "fontWeight", "Weight", FONT_WEIGHTS, value("fontWeight")),
        select_input(ns, "fontStyle", "Style", FONT_STYLES, value("fontStyle")),
        select_input(ns, "fontFamily", "Family", FONT_FAMILIES, value("fontFamily"))
      )
      hide <- function(label, value) tags$div(class = "gv-switch-row",
        block_switch(ns("hide"), label, value = value, size = "sm"))
      text <- field_group("Text",
        text_input(ns, "text", "Text", if (identical(style$text, "")) "" else value("text")),
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
        # Where it is moved to from its place in the chart. Clearing both fields,
        # Reset, or the chart's own undo and reset move it back.
        if (item$component %in% DRAGGABLE) field_group("Position",
          tags$div(class = "gv-field-pair",
            field(ns("moveX"), "Right (px)", block_input(ns("moveX"), value = value("moveX"), type = "number", step = 1),
                  stacked = TRUE),
            field(ns("moveY"), "Down (px)", block_input(ns("moveY"), value = value("moveY"), type = "number", step = 1),
                  stacked = TRUE),
            if (item$component == "labels")
              field(ns("rotate"), "Rotation", block_input(ns("rotate"), value = value("rotate"), type = "number",
                                                          min = -360, max = 360, step = 5), stacked = TRUE)
          ),
          tags$p(class = "gv-element-hint", "Drag it in the chart, or use the arrow keys.")
        ),
        if (item$component %in% WHOLE_SCALE) tags$p(class = "gv-element-hint",
          "Drag anywhere on the scale to move it as a whole. Appearance settings are in Scale.")
        else if (legend_marker) tags$div(
          fill(), border(),
          tags$p(class = "gv-element-hint", "These settings change the legend patch. Gene colours are set under Colors.")
        ) else if (gene) tags$div(
          fill(),
          field_group("Shape",
            select_input(ns, "marker", "Marker", MARKERS, value("marker")),
            select_input(ns, "markerSize", "Size",
                         c("Small" = "small", "Medium" = "medium", "Large" = "large"),
                         value("markerSize")),
            num_input(ns, "markerHeight", "Height", value("markerHeight"), min = 1, step = 1),
            num_input(ns, "cornerRadius", "Corner radius", value("cornerRadius"), min = 0, step = 1),
            tags$p(class = "gv-element-hint",
                   "The length of a gene comes from its start and end positions.")
          ),
          field_group("Arrowhead",
            num_input(ns, "arrowheadWidth", "Width", value("arrowheadWidth"), min = 0, step = 1),
            num_input(ns, "arrowheadHeight", "Height", value("arrowheadHeight"), min = 0, step = 1)
          ),
          border(),
          field_group("Position",
            num_input(ns, "y", "Vertical", value("y"), min = 0, max = 100, step = 1),
            tags$p(class = "gv-element-hint",
                   "From 0 (bottom) to 100 (top) of the cluster. The gene's label stays in place.")
          )
        ) else if (item$component == "labels") tags$div(text, font
        ) else if (item$component == "coordinates") tags$div(
          field_group(NULL, hide("Hide this coordinate", isFALSE(style$show))), font
        ) else tags$div(text, font)
      )
    })

    read_style <- function(item) {
      previous <- isolate(edits()[[key(item)]]$style)
      style <- if (item$component %in% WHOLE_SCALE) list()
      else if (item$component == "legend" && identical(item$part, "marker")) list(
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
        if (item$component == "labels") list(rotate = num(input$rotate))
      )
      if (item$component %in% DRAGGABLE) {
        style <- c(style, list(moveX = num(input$moveX), moveY = num(input$moveY)))
      }
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
                      input$rotate, input$moveX, input$moveY), {
      item <- target()
      if (is.null(item) || !identical(input$token, item$token)) return()
      style <- read_style(item)
      all <- edits()
      # What identifies the element in GC_element_style(): its row (and end),
      # or its cluster and part
      all[[key(item)]] <- if (length(style)) {
        list(component = item$component,
             id = edit_id(item),
             style = style)
      }
      if (!identical(all, edits())) edits(all)
    }, ignoreInit = TRUE)

    move <- function(event) {
      if (!isTRUE(event$component %in% DRAGGABLE) || !selectable(event)) return(FALSE)
      position <- event$position
      if (!is.numeric(position$moveX) || !is.numeric(position$moveY) ||
          length(position$moveX) != 1 || length(position$moveY) != 1 ||
          !is.finite(position$moveX) || !is.finite(position$moveY)) return(FALSE)
      # Whole pixels arrive as integers; fields send doubles
      position <- lapply(position[c("moveX", "moveY")], as.numeric)
      open(event)
      item <- isolate(target())
      all <- isolate(edits())
      previous <- all[[key(item)]]$style
      style <- utils::modifyList(if (is.null(previous)) list() else previous, position)
      # Back where the chart puts it (an undo of its first move): no offset
      if (style$moveX == 0 && style$moveY == 0) style[c("moveX", "moveY")] <- NULL
      all[[key(item)]] <- if (length(style)) list(component = item$component, id = edit_id(item), style = style)
      edits(all)
      item$drawn <- utils::modifyList(if (is.null(item$drawn)) list() else item$drawn, position)
      show(item)
      TRUE
    }
    # A position typed for several elements applies to all that can move;
    # only the field typed in, so mixed values of the other stay
    set_group_position <- function(name, value) {
      selection <- group()
      if (is.null(selection) || !identical(input$groupToken, selection$token)) return()
      # As numbers: an empty number field sends NA, and must not clear the
      # mixed values it shows
      value <- num(value)
      if (identical(value, num(group_position[[name]]))) return()
      group_position[[name]] <<- if (is.null(value)) "" else field_value(name, value)
      all <- edits()
      for (item in selection$items) {
        if (!item$component %in% DRAGGABLE) next
        style <- all[[key(item)]]$style
        if (is.null(style)) style <- list()
        style[[name]] <- value
        all[[key(item)]] <- if (length(style)) list(component = item$component, id = edit_id(item), style = style)
      }
      if (!identical(all, edits())) edits(all)
    }
    observeEvent(input$groupMoveX, set_group_position("moveX", input$groupMoveX), ignoreInit = TRUE)
    observeEvent(input$groupMoveY, set_group_position("moveY", input$groupMoveY), ignoreInit = TRUE)
    observeEvent(input$groupReset, {
      selection <- req(group())
      all <- edits()
      for (item in selection$items) all[[key(item)]] <- NULL
      edits(all)
    })

    # Several elements moved at once: the chart's reset, or undoing or redoing
    # it. Elements back where the chart puts them keep no offset.
    place <- function(events) {
      all <- isolate(edits())
      for (event in events) {
        position <- event$position
        if (!isTRUE(event$component %in% DRAGGABLE) || !selectable(event) ||
            !is.numeric(position$moveX) || !is.numeric(position$moveY) ||
            length(position$moveX) != 1 || length(position$moveY) != 1 ||
            !is.finite(position$moveX) || !is.finite(position$moveY)) next
        item <- as_item(event)
        style <- utils::modifyList(all[[key(item)]]$style %||% list(),
                                   lapply(position[c("moveX", "moveY")], as.numeric))
        if (style$moveX == 0 && style$moveY == 0) style[c("moveX", "moveY")] <- NULL
        all[[key(item)]] <- if (length(style)) list(component = item$component, id = edit_id(item), style = style)
      }
      edits(all)
      refresh()
    }

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
      refresh()
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
      if (is.null(item) || !is.null(group())) return(NULL)
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

    list(open = open, open_group = open_group, move = move, place = place, selectable = selectable,
         clear = function() { target(NULL); group(NULL) },
         edits = function() unname(edits()), target = target,
         # The edits by their keys, for a project file, and back
         saved = function() isolate(edits()),
         restore = function(saved) { target(NULL); group(NULL); edits(saved) })
  })
}
