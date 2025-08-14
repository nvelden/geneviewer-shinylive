#' Load logic module
#'
#' Contains logic for loading and processing data for the Gene Cluster Dashboard.
'.__module__.'
box::use(
  geneviewer[read_gbk, gbk_features_to_df],
  utils[read.csv],
  dplyr[bind_rows],
  tools[file_path_sans_ext]
)

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

#' Load gene data from CSV file(s)
#'
#' @param filepaths Character vector of CSV file paths
#' @return Combined data frame
#' @export
load_csv <- function(filepaths) {
  dfs <- lapply(filepaths, function(fp) {
    read.csv(fp, stringsAsFactors = FALSE)
  })
  bind_rows(dfs)
}

#' Load gene data from FASTA file(s)
#'
#' @param filepaths Character vector of FASTA file paths
#' @return Combined data frame
#' @export
load_fasta <- function(filepaths) {
  if (length(filepaths) == 1 && dir.exists(filepaths)) {
    # Read all FASTA files from a directory
    df <- geneviewer::read_fasta(
      fasta_path     = filepaths,
      sequence       = TRUE,
      file_extension = "fasta"
    )
  } else {
    # Combine multiple single-file reads
    dfs <- lapply(filepaths, function(fp) {
      geneviewer::read_fasta(
        fasta_path = fp,
        sequence   = TRUE
      )
    })
    df <- dplyr::bind_rows(dfs)
  }

  return(df)
}

#' Load gene data from GBK file(s)
#'
#' @param filepaths Character vector of GBK file paths
#' @param filenames Character vector of original file names (same length as filepaths)
#' @return Combined data frame
#' @export
load_gbk <- function(filepaths, filenames) {
  if (length(filepaths) != length(filenames)) {
    stop("filepaths and filenames must have the same length.")
  }

  gbk_features <- c(
    "start", "end", "strand", "cluster",
    "protein_id", "gene_functions", "product",
    "gene_kind", "score", "gene",
    "GO_function", "GO_process"
  )

  dfs <- Map(function(fp, fn) {
    gbk <- read_gbk(fp, origin = FALSE)
    df <- gbk_features_to_df(gbk)

    # extract file name without extension
    file_name <- tools::file_path_sans_ext(basename(fn))

    # add or replace 'cluster' column
    df$cluster <- file_name

    df
  }, filepaths, filenames)

  dplyr::bind_rows(dfs)
}

#' Load gene data from a list of CSV or GBK files (not mixed)
#'
#' @param fileInput A data frame from Shiny fileInput() or a character vector of file paths
#' @return Combined data frame
#' @export
load_gene_data <- function(fileInput) {

  # If fileInput is a data frame from fileInput()
  if (is.data.frame(fileInput) && all(c("datapath", "name") %in% names(fileInput))) {
    filepaths <- fileInput$datapath
    filenames <- fileInput$name
  } else if (is.character(fileInput)) {
    filepaths <- fileInput
    filenames <- basename(fileInput)
  } else {
    stop("fileInput must be either a character vector of file paths or a data frame from fileInput().")
  }

  if (length(filepaths) == 0) {
    stop("No file paths provided.")
  }

  # Determine file extensions based on original filenames
  extensions <- unique(tolower(tools::file_ext(filenames)))

  if (length(extensions) > 1) {
    stop("Files must all be of the same type. Mixing CSV and GBK files is not allowed.")
  }

  ext <- extensions[1]

  if (ext == "csv") {
    return(load_csv(filepaths))
  } else if (ext %in% c("gbk", "gb")) {
    return(load_gbk(filepaths, filenames))
  } else if (ext %in% c("fasta", "fa", "faa")) {
    return(load_fasta(filepaths))
  } else {
    stop(paste("Unsupported file type:", ext))
  }
}

