#' UI helper module
#'
#' Small wrappers around shinyblocks fields, so every setting in the
#' Gene Cluster Dashboard is laid out the same way: a row with the label on
#' the left and a compact control on the right, or a stacked field for wide
#' controls.
'.__module__.'
box::use(
  shiny[tags, tagList, conditionalPanel],
  shinyblocks[block_field, block_field_label, block_field_description, block_input,
              block_select, block_switch, block_textarea]
)

#' Value of the "none" choice of optional selects
#' @export
NONE <- "__none__"

#' The type of every setting, by namespaced input id, so presets (the
#' examples) can update them: "text", "number", "select", "switch",
#' "textarea", "toggle" or "combobox". Filled as the UI is built.
#' @export
INPUT_TYPES <- new.env()

#' Record the type of a setting
#' @export
register_input <- function(id, type) {
  assign(id, type, envir = INPUT_TYPES)
  invisible(id)
}

#' A labelled field
#'
#' @param id Namespaced input id (used for the label)
#' @param label Label text
#' @param input The input
#' @param description Optional help text below the input
#' @param stacked Put the label above the input (for wide inputs)
#' @export
field <- function(id, label, input, description = NULL, stacked = FALSE) {
  block_field(
    class = if (stacked) "gv-field gv-field-stacked" else "gv-field",
    block_field_label(label, `for` = id),
    tags$div(class = "gv-field-control", input),
    if (!is.null(description)) block_field_description(description)
  )
}

#' A labelled number input. A default is geneviewer's value when the field is
#' left empty, shown as placeholder.
#' @export
num_field <- function(id, label, value = "", min = NULL, max = NULL, step = NULL,
                      description = NULL, placeholder = NULL, default = NULL) {
  register_input(id, "number")
  placeholder <- placeholder %||% if (!is.null(default)) as.character(default)
  field(id, label,
        block_input(id, value = value, type = "number", min = min, max = max, step = step,
                    placeholder = placeholder),
        description)
}

#' A labelled text input
#' @export
text_field <- function(id, label, value = "", placeholder = NULL, description = NULL,
                       stacked = FALSE, default = NULL) {
  register_input(id, "text")
  placeholder <- placeholder %||% default
  field(id, label, block_input(id, value = value, placeholder = placeholder),
        description, stacked)
}

#' Registered single-colour settings, for swatch updates
#' @export
COLOR_INPUTS <- new.env()

SWATCHES <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
              "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#000000", "#ffffff")

#' The shared colour swatches and custom picker
#' @export
color_swatches <- function(id, label, pick = NULL) {
  if (is.null(pick)) pick <- function(value) sprintf(
    "Shiny.setInputValue('gvColorPick', {id: '%s', value: %s}, {priority: 'event'})", id, value)
  tags$div(class = "gv-swatches", role = "group", `aria-label` = paste(label, "swatches"),
    lapply(SWATCHES, function(color) tags$button(
      type = "button", class = "gv-swatch", style = paste0("background:", color),
      title = color, `aria-label` = paste(label, color),
      onclick = pick(sprintf("'%s'", color))
    )),
    tags$label(class = "gv-swatch gv-swatch-custom", title = "Other colour",
      tags$input(type = "color", `aria-label` = paste(label, "(other colour)"),
                 onchange = pick("this.value")))
  )
}

#' A colour field with the same swatches as the individual-element editor
#' @export
color_field <- function(id, label, value = "", placeholder = NULL, description = NULL,
                        stacked = FALSE, default = NULL) {
  assign(id, TRUE, envir = COLOR_INPUTS)
  tags$div(class = "gv-color-field",
    text_field(id, label, value, placeholder, description, stacked, default),
    color_swatches(id, label))
}

#' A labelled select. With a default (geneviewer's value), the first choice is
#' "Default (...)", which sends nothing.
#' @export
select_field <- function(id, label, choices, selected = NULL, placeholder = NULL,
                         description = NULL, stacked = FALSE, default = NULL) {
  register_input(id, "select")
  if (!is.null(default)) {
    if (is.null(names(choices))) names(choices) <- choices
    shown <- names(choices)[match(default, choices)]
    if (length(shown) == 0 || is.na(shown) || !nzchar(shown)) shown <- default
    choices <- c(stats::setNames(NONE, sprintf("Default (%s)", shown)), choices)
    selected <- selected %||% NONE
  }
  field(id, label,
        block_select(id, choices = choices, selected = selected, placeholder = placeholder,
                     size = "sm"),
        description, stacked)
}

#' A switch with its label on the left
#' @export
switch_field <- function(id, label, value = FALSE, description = NULL) {
  register_input(id, "switch")
  tags$div(
    class = "gv-switch-row",
    block_switch(id, label = label, value = value, size = "sm"),
    if (!is.null(description)) tags$p(class = "gv-switch-description", description)
  )
}

#' A labelled multi-line text input, e.g. for group style rules
#' @export
textarea_field <- function(id, label, value = "", placeholder = NULL, rows = 3,
                           description = NULL) {
  register_input(id, "textarea")
  field(id, label,
        block_textarea(id, value = value, placeholder = placeholder, rows = rows),
        description, stacked = TRUE)
}

#' The fields of group styles: the column the rules match on, and the rules.
#' The rules field of group styles (genes, labels, legend)
#' @export
group_style_fields <- function(ns, data, example, with_style_by = TRUE) {
  tags$div(
    class = "gv-group-body",
    if (with_style_by) {
      select_field(ns("styleBy"), "Match on", c("Group column" = NONE, names(data)), NONE,
                   description = "The column the groups of the rules are values of.")
    },
    textarea_field(
      ns("groupStyle"), "Rules", placeholder = example, rows = 3,
      description = "One rule per line: groups, a colon, then options. Quote names with commas."
    )
  )
}

#' Several fields, one row each
#' @export
field_row <- function(...) {
  tagList(...)
}

#' A group of fields, with an optional title
#' @export
field_group <- function(title = NULL, ...) {
  tags$section(
    class = "gv-group",
    if (!is.null(title)) tags$h4(class = "gv-group-title", title),
    tags$div(class = "gv-group-body", ...)
  )
}

#' The groups of a settings section
#' @export
sections <- function(...) {
  tags$div(class = "gv-sections", ...)
}

#' Fields or groups shown only when a switch is on
#'
#' @param ns Namespace function of the module
#' @param input_id Id of the controlling input (not namespaced)
#' @export
when_on <- function(ns, input_id, ...) {
  conditionalPanel(sprintf("input['%s']", ns(input_id)), class = "gv-when", ...)
}

#' Font family choices
#' @export
FONT_FAMILIES <- c("sans-serif", "serif", "monospace", "Arial", "Helvetica", "Times New Roman")

#' Font weight choices
#' @export
FONT_WEIGHTS <- c("normal", "bold", "lighter", "bolder")

#' Font style choices
#' @export
FONT_STYLES <- c("normal", "italic", "oblique")

#' Alignment choices
#' @export
ALIGNMENTS <- c("Left" = "left", "Center" = "center", "Right" = "right")

#' Number format choices for axis and coordinate ticks
#' @export
TICK_FORMATS <- c(
  "Comma separated" = ",.0f",
  "Integer"         = "0",
  "SI (0 decimals)" = ".0s",
  "SI (2 decimals)" = ".2s"
)
