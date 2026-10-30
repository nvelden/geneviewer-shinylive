#' Examples module
#'
#' The examples of the Data section: the code that makes their data, and the
#' settings that make the chart, by the (namespaced) id of the setting.
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

CORONAVIRUS_COLORS <- c(
  "5' UTR" = "#1A1A1A", ORF1a = "#4E79A7", ORF1b = "#A0CBE8", spike = "#E15759",
  "hemagglutinin-esterase" = "#F28E2B", envelope = "#EDC948",
  membrane = "#59A14F", nucleocapsid = "#2F4B7C", accessory = "#B07AA1",
  "3' UTR" = "#BDBDBD"
)

# The data of an example: its code, run on the datasets of geneviewer, so
# the code shown under the chart is the code that made the data
run_code <- function(code) {
  env <- new.env(parent = getNamespaceInfo("geneviewer", "lazydata"))
  eval(parse(text = code), envir = env)
  env$data
}

#' @export
EXAMPLES <- list(
  ophA = list(
    label = "ophA gene clusters",
    code = "# The example data of geneviewer\ndata <- ophA_clusters",
    inputs = function(data) list(
      `data-start` = "start", `data-end` = "end", `data-cluster` = "cluster",
      `data-group` = "class"
    )
  ),

  erythromycin = list(
    label = "Erythromycin A cluster",
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
    code = paste0(
      "# Tn7-like transposons with a CRISPR-Cas system, data of geneviewer\n",
      "data <- tn7_crispr_elements\n",
      "# Lengthen the end markers by 100 bp, as in the tutorial\n",
      "data$end[data$gene == \"R\"] <- data$end[data$gene == \"R\"] + 100\n",
      "data$start[data$gene == \"L\"] <- data$start[data$gene == \"L\"] - 100"
    ),
    inputs = function(data) {
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
        `titles-label_show` = TRUE, `titles-label_subtitleColumn` = "strain_short",
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
  ),

  coronavirus = list(
    label = "Coronaviruses of the four genera",
    code = paste0(
      "# Eleven coronaviruses of the four genera, data of geneviewer\n",
      "data <- coronavirus_genomes\n",
      "# The label below the name of each genome\n",
      "data$label_subtitle <- paste0(\"<i>\", data$subgenus, \"</i>, \", data$host)"
    ),
    inputs = function(data) {
      genus <- unique(data[, c("cluster", "genus")])$genus
      list(
        `data-start` = "start", `data-end` = "end", `data-cluster` = "cluster",
        `data-group` = "category",
        `size-height` = 850,
        `colors-customColors` = paste(names(CORONAVIRUS_COLORS), "=", CORONAVIRUS_COLORS,
                                      collapse = ", "),
        # Every gene a box; ORFs that lie inside another gene, e.g. 9b inside
        # N, go on the tracks below, as smaller boxes with their label inside
        `genes-marker` = "box", `genes-markerHeight` = 18, `genes-arrowheadHeight` = 18,
        # 3c and 3d of SARS-CoV-2 overlap by less than half: they share a
        # track, so no genome needs a third one
        `genes-overlap` = "nested", `genes-overlapMin` = "50%", `genes-overlapSpacing` = -4,
        `genes-styleBy` = "geneTrack",
        `genes-groupStyle` = "2, 3: markerHeight = 13, arrowheadHeight = 13, fill = #E8DCE6, stroke = #B07AA1",
        `labels-show` = TRUE, `labels-label` = "gene", `labels-styleBy` = "geneTrack",
        `labels-groupStyle` = "2, 3: dy = -1.6em, fontSize = 8px",
        # Homologous genes of close relatives, coloured by identity, written
        # on each link where it fits
        `links-show` = TRUE, `links-source` = "matching", `links-group` = "homolog",
        `links-measure` = "identity", `links-label` = TRUE,
        `links-labelPosition` = "link", `links-labelSize` = 9, `links-vmin` = 20,
        `links-colorBar` = "show", `links-colorBarPosition` = "legend",
        `links-colorBarLegendSide` = "right",
        `titles-label_show` = TRUE, `titles-label_subtitleColumn` = "label_subtitle",
        `titles-label_width` = "170px", `titles-label_fontSize` = 13,
        `titles-label_subSize` = 11, `titles-label_align` = "right", `titles-label_x` = -10,
        # The genus above its first genome
        `titles-title_show` = TRUE,
        `titles-title_title` = paste(ifelse(duplicated(genus), "", genus), collapse = ", "),
        `titles-title_align` = "left", `titles-title_height` = "30px",
        `titles-title_x` = -165, `titles-title_y` = 10, `titles-title_t_fontSize` = 13,
        `titles-chart_show` = TRUE,
        `titles-chart_title` = "Conserved core, variable accessory genes: coronaviruses of the four genera",
        `titles-chart_subtitle` = paste(
          "Homologous proteins of close relatives linked by identity;",
          "after Fig. 1 of Chazal (2021)"
        ),
        `titles-chart_height` = "55px", `titles-chart_t_fontSize` = 16,
        `titles-chart_t_fontWeight` = "bold", `titles-chart_s_fontSize` = 12,
        `titles-chart_s_fontStyle` = "italic",
        `tooltip-formatter` = "<b>{gene}</b> {product}<br>{virus} {strain}<br>{start} - {end}",
        `legend-position` = "top", `legend-align` = "center",
        `legend-order` = join_names(names(CORONAVIRUS_COLORS)), `legend-fontSize` = 12,
        # The legend names each category with its gene
        `legend-groupStyle` = paste(
          "5' UTR: text = \"5' UTR (L)\"",
          "spike: text = \"spike (S)\"",
          "hemagglutinin-esterase: text = \"hemagglutinin-esterase (HE)\"",
          "envelope: text = \"envelope (E)\"",
          "membrane: text = \"membrane (M)\"",
          "nucleocapsid: text = \"nucleocapsid (N)\"",
          sep = "\n"
        ),
        # The untranslated ends share a pattern
        `colors-showPatterns` = TRUE, `colors-pattern` = "diagonal",
        `colors-patternGroups` = "5' UTR, 3' UTR", `colors-patternColor` = "#8C8C8C",
        `colors-patternBackground` = "none", `colors-patternSize` = 4,
        `zoom-show` = TRUE, `zoom-height` = 44,
        `theme-theme` = "publication"
      )
    }
  ),

  pqeb1 = list(
    label = "pQEB1 plasmids and rearranged variants",
    links = function() geneviewer::pQEB1_alignment,
    link_code = quote(pQEB1_alignment),
    code = "# Five related IncN plasmids, data of geneviewer\ndata <- pQEB1_plasmids",
    inputs = function(data) {
      colors <- c(
        "Carbapenemase (KPC)" = "#d62728", "Other resistance" = "#ff7f0e",
        "Arsenic resistance" = "#8c564b", "Mobile element" = "#9467bd",
        "Conjugation" = "#1f77b4", "Replication & maintenance" = "#2ca02c",
        "Hypothetical" = "#d9d9d9", "Other" = "#9e9e9e"
      )
      list(
        `data-start` = "start", `data-end` = "end", `data-cluster` = "cluster",
        `data-group` = "category", `data-strand` = "strand",
        `size-height` = 520, `size-bottom` = 20,
        `genes-markerSize` = "small", `scale-axisType` = "range", `scale-show` = TRUE,
        `colors-customColors` = paste(names(colors), "=", colors, collapse = ", "),
        `links-show` = TRUE, `links-source` = "alignment", `links-measure` = "none",
        `links-label` = FALSE, `links-colorBar` = "hide",
        `links-normalColor` = "#7f9cbf", `links-invertedColor` = "#e6894e",
        `titles-label_show` = TRUE, `titles-label_subtitleColumn` = "host",
        `titles-label_width` = "175px", `titles-label_fontSize` = 13,
        `titles-label_subSize` = 12, `titles-label_subStyle` = "italic",
        `titles-label_align` = "right", `titles-label_x` = -10,
        `titles-chart_show` = TRUE,
        `titles-chart_title` = "<i>bla</i>KPC-2 plasmid pQEB1 and its rearranged variants",
        `titles-chart_subtitle` = "IncN plasmids aligned with nucmer (all blocks \u2265 99.2% identity); orange links mark inversions",
        `titles-chart_height` = "50px", `titles-chart_t_fontSize` = 18,
        `titles-chart_t_fontWeight` = "bold", `titles-chart_s_fontSize` = 13,
        `titles-chart_s_fontStyle` = "italic",
        `tooltip-formatter` = "<b>{gene}</b> {product}<br>{start} - {end}",
        `legend-position` = "top", `legend-align` = "center",
        `legend-order` = join_names(names(colors)), `legend-fontSize` = 10,
        `theme-theme` = "publication"
      )
    }
  )
)

EXAMPLES <- lapply(EXAMPLES, function(example) {
  example$data <- function() run_code(example$code)
  example
})
