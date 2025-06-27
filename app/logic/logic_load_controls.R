#' Load logic module
#'
#' Contains logic for loading and processing data for the Gene Cluster Dashboard.
'.__module__.'

#' Synonyms for standard input columns
#'
#' A named list where each name is the logical input name,
#' and the value is a character vector of possible synonyms
#' that might appear in uploaded data.
#'
#' @export
synonyms <- list(
  start   = c("start", "start_pos", "start_position", "begin", "from"),
  end     = c("end", "end_pos", "end_position", "stop", "to"),
  cluster = c("cluster", "locus", "region", "domain", "feature"),
  group   = c("group", "class", "category", "type", "family", "subfamily",
              "clade", "taxon", "classification", "annotation", "feature_type"),
  strand  = c("strand", "orientation", "direction", "sense", "polarity", "frame", "reading_frame")
)

#' Set inputs based on column names and synonyms in a data frame
#'
#' Checks if certain columns or any of their synonyms exist
#' (case-insensitively) in a data frame, and updates entries
#' in a reactiveValues object accordingly.
#'
#' For each logical input name, the function searches for any
#' of its synonyms among the data's column names. If found,
#' the column name (with original casing) is stored in
#' \code{r$input[[name]]}. If no synonym matches, the value
#' is set to NULL.
#'
#' @param data A data.frame containing the data
#' @param r A reactiveValues object with an \code{$input} list
#' @param input_names A character vector of desired input names to check
#'                     (e.g. c("start", "end"))
#' @return No return value; updates \code{r$input} by reference
#' @export
set_inputs_from_columns <- function(data, r, input_names) {
  data_cols_lower <- tolower(colnames(data))

  for (input_name in input_names) {
    found <- FALSE

    if (input_name %in% names(synonyms)) {
      for (possible_name in synonyms[[input_name]]) {
        match_idx <- which(data_cols_lower == tolower(possible_name))

        if (length(match_idx) > 0) {
          original_colname <- colnames(data)[match_idx[1]]
          r$input[[input_name]] <- original_colname
          found <- TRUE
          break
        }
      }
    }

    if (!found) {
      r$input[[input_name]] <- NULL
    }
  }
}
