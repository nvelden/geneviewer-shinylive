
#' Utility functions module
#'
#' Common utility functions for the Gene Cluster Dashboard.
'.__module__.'

#' Parse comma-separated input values
#'
#' Converts a string input into a vector if it contains commas, otherwise returns
#' the string as-is. Returns NULL for empty inputs.
#'
#' @param input_value Character string to parse
#' @return Character vector, single character string, or NULL
#' @export
parse_delimited_input <- function(input_value) {
  if (input_value != "" && grepl(",", input_value)) {
    trimws(strsplit(input_value, ",")[[1]])
  } else if (input_value != "") {
    input_value
  } else {
    NULL
  }
}
