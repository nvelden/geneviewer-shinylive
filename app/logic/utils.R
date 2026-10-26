
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
  if (is.null(input_value) || length(input_value) == 0) {
    NULL
  } else if (input_value != "" && grepl(",", input_value)) {
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

#' One subtitle per visible cluster, in the displayed order
#' @export
cluster_subtitles <- function(data, cluster_col, value_col) {
  if (is.null(value_col) || !value_col %in% names(data)) return(NULL)
  groups <- if (is.null(cluster_col)) rep("all", nrow(data)) else as.character(data[[cluster_col]])
  vapply(unique(groups), function(group) {
    values <- as.character(data[[value_col]][!is.na(groups) & groups == group])
    values <- values[!is.na(values)]
    if (length(values)) values[1] else ""
  }, character(1), USE.NAMES = FALSE)
}

#' A number from a number input, or NULL when it is empty
#' @export
num <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) return(NULL)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) NULL else x
}

#' Text from a text input, or NULL when it is empty
#' @export
txt <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(trimws(x))) NULL else trimws(x)
}

#' A select value, or NULL for the "none" choice
#' @export
opt <- function(x, none = "__none__") {
  if (is.null(x) || length(x) == 0 || identical(x, none) || !nzchar(x)) NULL else x
}

#' A number as pixels ("12px"), or NULL when it is empty
#' @export
px <- function(x) {
  n <- num(x)
  if (is.null(n)) NULL else paste0(n, "px")
}

#' Leave out NULL elements of a list
#' @export
compact <- function(x) Filter(Negate(is.null), x)

#' Split text on a separator, but not inside double quotes; the parts are
#' trimmed and lose their quotes
#' @export
split_unquoted <- function(text, sep = ",", strip = TRUE) {
  chars <- strsplit(text, "")[[1]]
  parts <- character()
  current <- ""
  quoted <- FALSE
  for (ch in chars) {
    if (ch == "\"") {
      quoted <- !quoted
      current <- paste0(current, ch)
    } else if (ch == sep && !quoted) {
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  parts <- trimws(c(parts, current))
  parts <- parts[nzchar(parts)]
  if (strip) gsub('^"(.*)"$', "\\1", parts) else parts
}

# A value typed in a rule: a number, TRUE/FALSE, or text
parse_value <- function(x) {
  if (x %in% c("TRUE", "FALSE")) return(as.logical(x))
  if (grepl("^-?[0-9]*\\.?[0-9]+$", x)) return(as.numeric(x))
  x
}

# "name = value" pairs, split on the first = outside quotes
parse_pairs <- function(text) {
  pairs <- split_unquoted(text, ",", strip = FALSE)
  out <- list()
  for (pair in pairs) {
    pos <- regexpr("=", pair, fixed = TRUE)
    if (pos < 1) next
    name <- gsub('^"(.*)"$', "\\1", trimws(substr(pair, 1, pos - 1)))
    value <- gsub('^"(.*)"$', "\\1", trimws(substr(pair, pos + 1, nchar(pair))))
    if (nzchar(name)) out[[name]] <- parse_value(value)
  }
  out
}

#' Colours by group, typed as `A = red, "cas, other" = #C9A0DC`; NULL when
#' the text has no `=` (then it is a plain list of colours)
#' @export
parse_named_colors <- function(text) {
  if (is.null(text) || !nzchar(trimws(text)) || !grepl("=", text, fixed = TRUE)) return(NULL)
  colors <- parse_pairs(text)
  if (length(colors) == 0) NULL else lapply(colors, as.character)
}

#' Group styles, one rule per line: `R, L: marker = box, markerHeight = 38`
#' gives list(R = list(marker = "box", markerHeight = 38), L = ...)
#' @export
parse_group_styles <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  styles <- list()
  for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    parts <- split_unquoted(line, ":", strip = FALSE)
    if (length(parts) < 2) next
    groups <- split_unquoted(parts[1], ",")
    options <- parse_pairs(paste(parts[-1], collapse = ":"))
    for (group in groups) styles[[group]] <- c(styles[[group]], options)
  }
  if (length(styles) == 0) NULL else styles
}

#' The value to select when a picker gets new choices: while an example's
#' settings are pending its value (this update may reach the browser after the
#' example's own), else the current value, else the fallback
#' @export
pick_choice <- function(r, id, current, choices, fallback) {
  target <- shiny::isolate(r$pending$targets[[id]])
  if (!is.null(target)) {
    kept <- intersect(target, choices)
    if (length(kept) > 0) return(kept)
  }
  if (length(current) > 0 && all(current %in% choices)) current else fallback
}
