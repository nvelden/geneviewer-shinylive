#' Code generation module
#'
#' Writes the R code that makes the chart shown in the app: the code that
#' loads the data, and the geneviewer calls with the arguments the app used.
'.__module__.'

# Integers from number inputs print as 8L; show them as plain numbers
clean <- function(x) {
  if (is.list(x)) {
    lapply(x, clean)
  } else if (is.integer(x)) {
    as.numeric(x)
  } else {
    x
  }
}

r_value <- function(x) {
  paste(deparse(clean(x), width.cutoff = 500L), collapse = "")
}

# Defaults of options that geneviewer sets in its JavaScript, for arguments
# whose R default is NULL or empty or that are passed through `...`. Values
# equal to these (or to the R defaults) are left out of the code.
JS_DEFAULTS <- list(
  GC_title = list(
    align = "center",
    titleFont = list(fontSize = "16px", fontStyle = "normal", fontWeight = "bold",
                     fontFamily = "sans-serif"),
    subtitleFont = list(fontSize = "14px", fontStyle = "normal", fontWeight = "normal",
                        fontFamily = "sans-serif")
  ),
  GC_genes = list(marker = "arrow", markerSize = "medium", stroke = "black",
                  strokeWidth = 1, y = 50),
  GC_labels = list(fontSize = "12px", fontStyle = "italic", fontFamily = "sans-serif",
                   adjustLabels = TRUE),
  GC_scale = list(
    padding = 2, axis_position = "bottom", ticksCount = 10, ticksFormat = ",.0f",
    tickStyle = list(stroke = "grey", strokeWidth = 1, lineLength = 6),
    textStyle = list(fill = "black", fontSize = "10px", fontFamily = "Arial"),
    lineStyle = list(stroke = "grey", strokeWidth = 1)
  ),
  GC_scaleBar = list(
    title = "1 kb", scaleBarUnit = 1000, x = 0, y = 10, textPadding = 0,
    labelStyle = list(fontSize = "10px", fontFamily = "sans-serif", fill = "black",
                      labelPosition = "left"),
    scaleBarLineStyle = list(stroke = "grey", strokeWidth = 1),
    scaleBarTickStyle = list(stroke = "grey", strokeWidth = 1)
  ),
  GC_clusterTitle = list(
    x = 0, align = "center",
    titleFont = list(fontSize = "16px", fontWeight = "bold", fontFamily = "sans-serif"),
    subtitleFont = list(fontSize = "14px", fontWeight = "normal", fontFamily = "sans-serif")
  ),
  GC_clusterFooter = list(
    x = 0, y = 0, align = "left",
    titleFont = list(fontSize = "12px", fontWeight = "bold", fontFamily = "sans-serif"),
    subtitleFont = list(fontSize = "10px", fontWeight = "normal", fontFamily = "sans-serif")
  ),
  GC_clusterLabel = list(
    x = 0, y = 0, position = "left", align = "center", wrapLabel = TRUE,
    fontSize = "12px", fontStyle = "normal", fontWeight = "bold", fontFamily = "sans-serif",
    subtitleFont = list(fontSize = "11px", fontStyle = "normal")
  ),
  GC_legend = list(
    position = "bottom", align = "left", x = 0, y = 0,
    legendTextOptions = list(fontSize = "12px", fontFamily = "sans-serif")
  ),
  GC_coordinates = list(
    rotate = -45, yPositionTop = 53, yPositionBottom = 48, overlapThreshold = 20,
    ticksFormat = ",.0f",
    tickStyle = list(stroke = "black", strokeWidth = 1, lineLength = 6),
    textStyle = list(fill = "black", fontSize = "10px", fontFamily = "Arial")
  ),
  GC_sequence = list(
    y = 50,
    sequenceStyle = list(stroke = "grey", strokeWidth = 1),
    markerStyle = list(stroke = "grey", strokeWidth = 1)
  ),
  GC_color = list(colorScheme = "schemeCategory10"),
  GC_links = list(
    curve = TRUE, normal_color = "#969696", inverted_color = "#d62728",
    low_color = "white", vmin = 0, vmax = 100, linkWidth = 1
  )
)

# Calls that do nothing without arguments. Not GC_grid(): it also sets the
# height of the clusters from the chart height and the margins.
OPTIONAL_CALLS <- c("GC_color")

same_value <- function(x, default) {
  if (is.numeric(x) && is.numeric(default)) {
    return(length(x) == length(default) && isTRUE(all.equal(as.numeric(x), as.numeric(default))))
  }
  identical(x, default)
}

# The default of an argument: its R default when that is set, else (with
# js = TRUE) the JavaScript default
arg_default <- function(name, arg, js = TRUE) {
  args <- formals(getExportedValue("geneviewer", name))
  # Arguments without a default are an empty symbol, which cannot be assigned
  if (arg %in% names(args) && !identical(args[[arg]], quote(expr = ))) {
    value <- eval(args[[arg]], envir = asNamespace("geneviewer"))
    if (!is.null(value) && !(is.list(value) && length(value) == 0)) return(value)
  }
  if (js) JS_DEFAULTS[[name]][[arg]]
}

#' Leave out the arguments of a call that equal geneviewer's defaults; with
#' js = FALSE only the R defaults (a theme can change the JavaScript ones)
#' @export
drop_defaults <- function(name, args, js = TRUE) {
  kept <- list()
  for (arg in names(args)) {
    value <- args[[arg]]
    default <- arg_default(name, arg, js)
    if (is.list(value) && is.list(default) && !is.null(names(value))) {
      value <- value[!vapply(names(value), function(n) {
        !is.null(default[[n]]) && same_value(value[[n]], default[[n]])
      }, logical(1))]
      if (length(value) > 0) kept[[arg]] <- value
    } else if (is.null(default) || !same_value(value, default)) {
      # [<- with list() keeps a NULL value, e.g. GC_pattern(background = NULL)
      kept[arg] <- list(value)
    }
  }
  kept
}

#' One call, on one line when short, else one argument per line
format_call <- function(name, args, indent = 0, first = NULL) {
  parts <- c(first, if (length(args)) paste0(names(args), " = ", vapply(args, r_value, "")))
  if (length(parts) == 0) {
    return(paste0(name, "()"))
  }
  one_line <- paste0(name, "(", paste(parts, collapse = ", "), ")")
  if (indent + nchar(one_line) <= 76) {
    return(one_line)
  }
  inner <- strrep(" ", indent + 2)
  paste0(name, "(\n", paste0(inner, parts, collapse = ",\n"), "\n", strrep(" ", indent), ")")
}

#' Code that loads the data: the example data or the uploaded files
#'
#' @param files Names of the uploaded files, or NULL for the example data
#' @param cluster_col The cluster column
#' @param order The clusters to show, in order, or NULL for all
#' @param example_code The code that makes the data of an example
#' @export
data_code <- function(files, cluster_col = NULL, order = NULL, example_code = NULL) {
  if (is.null(files)) {
    code <- example_code %||% "# The example data of geneviewer\ndata <- ophA_clusters"
  } else {
    ext <- tolower(tools::file_ext(files[1]))
    paths <- r_value(files)
    code <- if (ext == "csv") {
      if (length(files) == 1) {
        sprintf("data <- read.csv(%s)", paths)
      } else {
        sprintf("data <- dplyr::bind_rows(lapply(%s, read.csv))", paths)
      }
    } else if (ext %in% c("gbk", "gb")) {
      paste0(
        "# One cluster per GenBank file, named after the file\n",
        "files <- ", paths, "\n",
        "data <- dplyr::bind_rows(lapply(files, function(file) {\n",
        "  genes <- gbk_features_to_df(read_gbk(file, origin = FALSE))\n",
        "  genes$cluster <- tools::file_path_sans_ext(basename(file))\n",
        "  genes\n",
        "}))"
      )
    } else {
      sprintf("data <- read_fasta(%s)", paths)
    }
    code <- paste0("# Your uploaded data; set the path to your file(s)\n", code)
  }

  if (!is.null(order) && !is.null(cluster_col)) {
    col <- r_value(cluster_col)
    clusters <- r_value(order)
    code <- paste0(
      code, "\n\n",
      "# Only these clusters, in this order\n",
      "data <- data[data[[", col, "]] %in% ", clusters, ", ]\n",
      "data[[", col, "]] <- factor(data[[", col, "]], levels = ", clusters, ")\n",
      "data <- data[order(data[[", col, "]]), ]"
    )
  }
  code
}

#' The code of the chart
#'
#' @param calls The geneviewer calls, each a list with `name` and `args`;
#'   the first is GC_chart() (its data is written as `data`)
#' @param data The code that loads the data
#' @param trim Leave out arguments equal to geneviewer's JavaScript defaults
#'   too. Not with a theme: there such a value may differ from what the theme
#'   sets. Arguments equal to the R defaults are always left out.
#' @export
chart_code <- function(calls, data, trim = TRUE) {
  if (length(calls) == 0) {
    return("")
  }
  calls <- lapply(calls, function(call) {
    call$args <- drop_defaults(call$name, call$args, js = trim)
    call
  })
  if (trim) {
    calls <- Filter(function(call) {
      length(call$args) > 0 || !call$name %in% OPTIONAL_CALLS
    }, calls)
  }
  chart <- format_call(calls[[1]]$name, calls[[1]]$args, first = "data")
  for (call in calls[-1]) {
    chart <- paste0(chart, " %>%\n  ", format_call(call$name, call$args, indent = 2))
  }
  paste0("library(geneviewer)\n\n", data, "\n\n", chart, "\n")
}
