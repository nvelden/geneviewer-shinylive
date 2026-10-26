#' Alignment-table validation for uploaded nucleotide/protein links
'.__module__.'

#' @export
validate_alignment <- function(data) {
  required <- c("cluster1", "start1", "end1", "cluster2", "start2", "end2")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Alignment CSV is missing columns: ", paste(missing, collapse = ", "))
  if (!nrow(data)) stop("Alignment CSV has no blocks.")
  if (anyDuplicated(names(data))) stop("Alignment CSV has duplicate column names.")
  for (col in c("cluster1", "cluster2")) {
    data[[col]] <- as.character(data[[col]])
    if (any(is.na(data[[col]]) | !nzchar(trimws(data[[col]])))) {
      stop("Alignment column '", col, "' must contain cluster names.")
    }
  }
  for (col in c("start1", "end1", "start2", "end2", intersect(c("identity", "similarity"), names(data)))) {
    values <- suppressWarnings(as.numeric(as.character(data[[col]])))
    if (any(!is.finite(values))) stop("Alignment column '", col, "' must contain finite numbers.")
    data[[col]] <- values
  }
  # Inverted blocks have start > end. Preserve that orientation.
  data
}
