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
              block_select, block_switch]
)

#' Value of the "none" choice of optional selects
#' @export
NONE <- "__none__"

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

#' A labelled number input
#' @export
num_field <- function(id, label, value = "", min = NULL, max = NULL, step = NULL,
                      description = NULL) {
  field(id, label,
        block_input(id, value = value, type = "number", min = min, max = max, step = step),
        description)
}

#' A labelled text input
#' @export
text_field <- function(id, label, value = "", placeholder = NULL, description = NULL,
                       stacked = FALSE) {
  field(id, label, block_input(id, value = value, placeholder = placeholder),
        description, stacked)
}

#' A labelled select
#' @export
select_field <- function(id, label, choices, selected = NULL, placeholder = NULL,
                         description = NULL, stacked = FALSE) {
  field(id, label,
        block_select(id, choices = choices, selected = selected, placeholder = placeholder,
                     size = "sm"),
        description, stacked)
}

#' A switch with its label on the left
#' @export
switch_field <- function(id, label, value = FALSE, description = NULL) {
  tags$div(
    class = "gv-switch-row",
    block_switch(id, label = label, value = value, size = "sm"),
    if (!is.null(description)) tags$p(class = "gv-switch-description", description)
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
