#' Project files
#'
#' A project file holds what is needed to open a chart again: its data and
#' alignment, every setting, and the edits of single elements. It is JSON, so
#' it can be read without running anything, and its columns keep their types.
'.__module__.'

box::use(
  jsonlite[toJSON, fromJSON]
)

FORMAT <- "geneviewer-project"
VERSION <- 1L

# A data frame as a list of typed columns. Columns of other types (lists,
# dates) are kept as text.
encode_data <- function(data) {
  if (is.null(data)) return(NULL)
  lapply(names(data), function(name) {
    col <- data[[name]]
    type <- if (is.factor(col)) "factor" else if (is.integer(col)) "integer"
      else if (is.double(col)) "double" else if (is.logical(col)) "logical" else "character"
    values <- if (is.list(col)) {
      vapply(col, function(v) paste(as.character(unlist(v)), collapse = ", "), "")
    } else if (type == "character") as.character(col) else col
    c(list(name = name, type = type),
      if (type == "factor") list(levels = I(levels(col))),
      list(values = I(if (type == "factor") as.character(col) else values)))
  })
}

# JSON null as NA, so a column keeps its length
as_vector <- function(values) {
  if (!is.list(values)) return(values)
  unlist(lapply(values, function(v) if (is.null(v)) NA else v))
}

scalar_text <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

decode_data <- function(columns) {
  if (!is.list(columns) || length(columns) == 0) stop("The project has no data.")
  cast <- list(integer = as.integer, double = as.numeric, logical = as.logical,
               character = as.character)
  data <- lapply(columns, function(col) {
    if (!is.list(col) || !scalar_text(col$name) || !scalar_text(col$type)) {
      stop("The data of the project is damaged.")
    }
    values <- as_vector(col$values)
    if (col$type == "factor") {
      factor(as.character(values), levels = as.character(as_vector(col$levels)))
    } else if (col$type %in% names(cast)) {
      cast[[col$type]](values)
    } else {
      stop("The data of the project has a column of unknown type: ", col$type)
    }
  })
  names(data) <- vapply(columns, `[[`, "", "name")
  if (length(unique(lengths(data))) != 1) stop("The data of the project is damaged.")
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

# A setting is a value or a list of values; an empty list is no value
decode_setting <- function(value) {
  if (is.list(value)) {
    if (length(value) == 0) return(NULL)
    if (!all(vapply(value, function(v) is.null(v) || (is.atomic(v) && length(v) == 1), TRUE))) {
      return(NULL)
    }
    value <- as_vector(value)
  }
  if (is.atomic(value)) value
}

scalars <- function(x) {
  is.list(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x))) &&
    all(vapply(x, function(v) is.atomic(v) && length(v) == 1, TRUE))
}

#' The project as JSON
#'
#' @param data The gene data
#' @param settings The value of every setting, by namespaced input id
#' @param edits The edits of the Element section, by their key
#' @param links The alignment: list(data, name, upload), or NULL
#' @param source Where the data came from: list(name, example, files)
#' @param zoom The window of the zoom slider: list(mode, range), or NULL
#' @export
write_project <- function(data, settings, edits = list(), links = NULL, source = list(),
                          zoom = NULL) {
  # A setting without a value is saved as an empty list, so it is not
  # replaced by its default when the project is opened
  settings <- lapply(settings, function(v) if (is.null(v)) I(character(0)) else if (length(v) != 1) I(v) else v)
  project <- list(
    format = FORMAT,
    version = VERSION,
    geneviewer = as.character(utils::packageVersion("geneviewer")),
    saved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    source = list(name = source$name, example = source$example,
                  files = if (length(source$files)) I(source$files)),
    data = encode_data(data),
    links = if (!is.null(links$data)) list(name = links$name, upload = isTRUE(links$upload),
                                          data = encode_data(links$data)),
    settings = settings,
    zoom = if (!is.null(zoom)) list(mode = zoom$mode, range = I(zoom$range)),
    edits = if (length(edits)) edits else structure(list(), names = character(0))
  )
  toJSON(project, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
}

#' Read a project file
#'
#' Stops with a message for the user when the file is not a project, or is
#' damaged. Settings and edits that are not values are left out.
#'
#' @param path The path of the file
#' @return list(data, settings, edits, links, source, zoom)
#' @export
read_project <- function(path) {
  project <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(project) || !identical(project$format, FORMAT)) {
    stop("This is not a geneviewer project file.")
  }
  if (!is.numeric(project$version) || project$version > VERSION) {
    stop("This project was saved by a newer version of the app.")
  }

  settings <- if (is.list(project$settings) && !is.null(names(project$settings))) {
    # Keep settings without a value: they are set to no value
    lapply(project$settings, decode_setting)
  } else list()

  edits <- if (is.list(project$edits) && !is.null(names(project$edits))) {
    Filter(function(edit) {
      is.list(edit) && scalar_text(edit$component) && scalars(edit$id) && scalars(edit$style)
    }, project$edits)
  } else list()

  links <- project$links
  links <- if (is.list(links) && length(links$data)) {
    list(data = decode_data(links$data),
         name = if (scalar_text(links$name)) links$name else "Saved alignment",
         upload = isTRUE(links$upload))
  }

  # The zoom window: shares of the clusters, or base pairs
  zoom <- project$zoom
  range <- if (is.list(zoom)) suppressWarnings(as.numeric(as_vector(zoom$range)))
  zoom <- if (is.list(zoom) && isTRUE(zoom$mode %in% c("relative", "absolute")) &&
              length(range) == 2 && all(is.finite(range)) && range[1] < range[2] &&
              (zoom$mode == "absolute" || (range[1] >= 0 && range[2] <= 1))) {
    list(mode = zoom$mode, range = range)
  }

  source <- project$source %||% list()
  list(
    zoom = zoom,
    data = decode_data(project$data),
    settings = settings,
    edits = edits,
    links = links,
    source = list(
      name = if (scalar_text(source$name)) source$name,
      example = if (scalar_text(source$example)) source$example,
      files = if (length(source$files)) as.character(as_vector(source$files))
    )
  )
}
