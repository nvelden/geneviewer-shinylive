
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

#' Drop the settings that still have their default value
#'
#' Used when a theme is set: options the user left at the app default are
#' dropped, so the theme sets them instead.
#'
#' @param x A value or (nested) list of settings
#' @param default The same settings with their default values
#' @return x without the unchanged settings, or NULL when nothing changed
#' @export
drop_unchanged <- function(x, default) {
  if (is.list(x) && is.list(default) && !is.null(names(x))) {
    out <- lapply(names(x), function(n) drop_unchanged(x[[n]], default[[n]]))
    names(out) <- names(x)
    out <- Filter(Negate(is.null), out)
    if (length(out) == 0) NULL else out
  } else if (identical(x, default)) {
    NULL
  } else {
    x
  }
}

#' Call a GC_ function, leaving out NULL arguments
#'
#' Arguments left out take the package default (or the theme), where a NULL
#' could be sent to the chart as an empty value.
#'
#' @param fun A GC_ function
#' @param chart The GC chart object
#' @param ... Arguments of fun
#' @return The result of fun
#' @export
call_gc <- function(fun, chart, ...) {
  args <- Filter(function(a) !is.null(a) && !(is.list(a) && length(a) == 0), list(...))
  do.call(fun, c(list(chart), args))
}
