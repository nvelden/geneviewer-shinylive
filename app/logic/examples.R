#' Examples module
#'
#' The examples of the Data section: their data, the code that makes the data,
#' and the settings that make the chart, by the (namespaced) id of the setting.
#' Settings an example does not set go back to their defaults.
'.__module__.'

# Join group names for a text field; names with commas are quoted
join_names <- function(x) {
  x <- ifelse(grepl(",", x, fixed = TRUE), paste0("\"", x, "\""), x)
  paste(x, collapse = ", ")
}

TN7_COLORS <- c(
  "insertion site" = "#F3C79B", "tnsA" = "#D62728", "tnsB" = "#AEB6F0",
  "tnsC" = "#D7F2F2", "tniQ / tnsD" = "#2CA02C", "tnsE" = "#C7E02C",
  "cas8f / cas5f" = "#8C1515", "cas7f" = "#F2A5C4", "cas6f" = "#7B3FD4",
  "cas5" = "#B5651D", "cas, other" = "#C9A0DC", "CRISPR array" = "#555555",
  "right end" = "#9FD4DE", "left end" = "#F28E2B", "other" = "#E8E8E8"
)

tn7_data <- function() {
  genes <- geneviewer::tn7_crispr_elements
  genes$end[genes$gene == "R"] <- genes$end[genes$gene == "R"] + 100
  genes$start[genes$gene == "L"] <- genes$start[genes$gene == "L"] - 100
  genes
}

#' @export
EXAMPLES <- list(
  ophA = list(
    label = "ophA gene clusters",
    data = function() geneviewer::ophA_clusters,
    code = "# The example data of geneviewer\ndata <- ophA_clusters",
    inputs = function(data) list(
      `data-start` = "start", `data-end` = "end", `data-cluster` = "cluster",
      `data-group` = "class"
    )
  ),

  erythromycin = list(
    label = "Erythromycin A cluster",
    data = function() geneviewer::erythromycin_cluster,
    code = "# The erythromycin A gene cluster, data of geneviewer\ndata <- erythromycin_cluster",
    inputs = function(data) {
      functions <- sort(unique(data$Functions))
      list(
        `data-start` = "Start", `data-end` = "End", `data-group` = "Functions",
        `size-height` = 170,
        `titles-chart_show` = TRUE,
        `titles-chart_title` = "Erythromycin A biosynthetic gene cluster from <i>Saccharopolyspora erythraea</i>",
        `titles-chart_align` = "left",
        `genes-marker` = "boxarrow", `genes-markerSize` = "small",
        `tooltip-formatter` = "<b>{Identifiers}</b><br>{Product}<br>{start} - {end}({Strand})",
        `scale-show` = TRUE, `scale-start` = 778000, `scale-end` = 832000,
        `scale-ticksCount` = 8,
        `legend-order` = join_names(c(setdiff(functions, "Other"), "Other")),
        `colors-customColors` = "Other = grey",
        `theme-theme` = "classic"
      )
    }
  ),

  tn7 = list(
    label = "Tn7-like transposons",
    data = tn7_data,
    code = paste0(
      "# Tn7-like transposons with a CRISPR-Cas system, data of geneviewer\n",
      "data <- tn7_crispr_elements\n",
      "# Lengthen the end markers by 100 bp, as in the tutorial\n",
      "data$end[data$gene == \"R\"] <- data$end[data$gene == \"R\"] + 100\n",
      "data$start[data$gene == \"L\"] <- data$start[data$gene == \"L\"] - 100"
    ),
    inputs = function(data) {
      strains <- unique(data[, c("cluster", "strain_short")])$strain_short
      gene_names <- c("tnsA", "tnsB", "tnsC", "tniQ / tnsD", "tnsE", "cas8f / cas5f",
                      "cas7f", "cas6f", "cas5")
      list(
        `data-start` = "start", `data-end` = "end", `data-cluster` = "cluster",
        `data-group` = "category", `data-strand` = "strand",
        `size-height` = 900,
        `colors-customColors` = paste(
          ifelse(grepl(",", names(TN7_COLORS)), paste0("\"", names(TN7_COLORS), "\""), names(TN7_COLORS)),
          "=", TN7_COLORS, collapse = ", "
        ),
        `scale-axisType` = "range", `scale-breaks` = TRUE,
        `scale-breakPadding` = 500, `scale-breakPaddingUnit` = "bp",
        `alignment-align` = TRUE, `alignment-idColumn` = "gene", `alignment-id` = "R",
        `genes-markerSize` = "small", `genes-markerHeight` = 20, `genes-styleBy` = "gene",
        `genes-groupStyle` = paste(
          "R, L: marker = box, markerHeight = 38, arrowheadHeight = 38",
          "CRISPR: marker = box, markerHeight = 20, arrowheadHeight = 20, stroke = black, strokeWidth = 1",
          "tnsA (fused): marker = box, markerHeight = 20, arrowheadHeight = 20",
          sep = "\n"
        ),
        `colors-showPatterns` = TRUE, `colors-pattern` = "diagonal",
        `colors-patternGroups` = "right end, left end",
        `colors-patternBackground` = "none", `colors-patternSize` = 4,
        `labels-show` = TRUE, `labels-label` = "label", `labels-fontSize` = 9,
        `labels-y` = 50, `labels-dy` = "14.5px", `labels-adjustLabels` = FALSE,
        `labels-styleBy` = "label",
        `labels-groupStyle` = paste(
          "tnsA, cas8f, cas6f, cas5: fill = white",
          "SRP-RNA: y = 55, dy = 0px, textAnchor = end, x = -4",
          sep = "\n"
        ),
        `titles-label_show` = TRUE, `titles-label_subtitle` = paste(strains, collapse = ", "),
        `titles-label_width` = "190px", `titles-label_fontSize` = 12,
        `titles-label_subSize` = 10, `titles-label_subStyle` = "italic",
        `titles-label_align` = "right", `titles-label_x` = -10,
        `titles-chart_show` = TRUE,
        `titles-chart_title` = "Tn7-like transposons that carry a minimal CRISPR-Cas system",
        `titles-chart_subtitle` = "Redrawn from Fig. 3 of Peters et al. (2017), Proc Natl Acad Sci USA 114:E7358-E7366",
        `titles-chart_height` = "70px", `titles-chart_t_fontSize` = 19,
        `titles-chart_s_fontSize` = 14, `titles-chart_s_fontStyle` = "italic",
        `tooltip-formatter` = "<b>{gene}</b> {product}<br>{start} - {end}",
        `legend-align` = "center", `legend-order` = join_names(names(TN7_COLORS)),
        `legend-fontSize` = 12,
        `legend-groupStyle` = paste0(join_names(gene_names), ": fontStyle = italic"),
        `theme-theme` = "publication"
      )
    }
  )
)
