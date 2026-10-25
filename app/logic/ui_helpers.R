#' UI helper module
#'
#' Small wrappers around shinyblocks fields, so every setting in the
#' Gene Cluster Dashboard has a label and the same spacing.
'.__module__.'
box::use(
  shiny[tags, conditionalPanel],
  shinyblocks[block_field, block_field_label, block_field_description, block_input,
              block_select, block_switch, block_grid, block_stack, block_separator]
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
#' @export
field <- function(id, label, input, description = NULL) {
  block_field(
    block_field_label(label, `for` = id),
    input,
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
text_field <- function(id, label, value = "", placeholder = NULL, description = NULL) {
  field(id, label, block_input(id, value = value, placeholder = placeholder), description)
}

#' A labelled select
#' @export
select_field <- function(id, label, choices, selected = NULL, placeholder = NULL,
                         description = NULL) {
  field(id, label,
        block_select(id, choices = choices, selected = selected, placeholder = placeholder),
        description)
}

#' A switch with its label
#' @export
switch_field <- function(id, label, value = FALSE) {
  block_switch(id, label = label, value = value)
}

#' Fields side by side, wrapping on narrow screens
#' @export
field_row <- function(...) {
  block_grid(..., min_width = "8.5rem", gap = "sm")
}

#' A titled group of fields
#' @export
field_group <- function(title, ...) {
  tags$div(
    class = "gv-group",
    tags$div(class = "gv-group-title", title),
    block_stack(..., gap = "sm")
  )
}

#' Fields shown only when a switch (or other input) is on
#'
#' @param ns Namespace function of the module
#' @param input_id Id of the controlling input (not namespaced)
#' @export
when_on <- function(ns, input_id, ...) {
  conditionalPanel(sprintf("input['%s']", ns(input_id)), block_stack(..., gap = "sm"))
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
