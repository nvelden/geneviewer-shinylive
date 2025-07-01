
#' Utility functions module
#'
#' Common utility functions for the Gene Cluster Dashboard.
'.__module__.'
box::use(
  stats[setNames],
)


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

#' Create a named list of colours for each group
#'
#' @param df        A data.frame
#' @param group_col Name of the column in df to group by (string)
#' @param cols      A vector of colour values (e.g. hex codes)
#' @return Named list of colours (one per unique group), or NULL if cols is NULL or empty
#' @export
make_named_color_list <- function(df, group_col, cols) {
  if (is.null(cols) || length(cols) == 0 || identical(cols, "")) {
    return(NULL)
  }
  groups <- unique(df[[group_col]])
  # repeat or truncate cols so its length == length(groups)
  assigned_cols <- rep(cols, length.out = length(groups))
  # build named list
  setNames(as.list(assigned_cols), groups)
}

#' Reorder and filter a data frame based on user cluster order
#'
#' @param data A data frame (e.g. r$cluster_data)
#' @param cluster_col Name of the cluster column (string)
#' @param order_vec A character vector of desired cluster order
#'
#' @return A reordered and filtered data frame
#' @export
order_cluster_data <- function(data, cluster_col, order_vec) {

  # Return original data if no valid inputs
  if (is.null(cluster_col) ||
      is.null(order_vec) ||
      length(order_vec) == 0 ||
      !cluster_col %in% names(data)) {
    return(data)
  }

  df <- data

  # Filter rows to keep only those clusters listed in order_vec
  df <- df[df[[cluster_col]] %in% order_vec, , drop = FALSE]

  # Apply factor to impose the desired order
  df[[cluster_col]] <- factor(
    df[[cluster_col]],
    levels = order_vec
  )

  # Sort rows
  df <- df[order(df[[cluster_col]]), ]

  return(df)
}
