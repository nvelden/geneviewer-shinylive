library(shiny)
library(shinyblocks)
library(geneviewer)

box::use(
  ./views/section_data[ui_data],
  ./views/section_theme[ui_theme],
  ./views/section_genes[ui_genes, ui_colors],
  ./views/section_labels[ui_labels, ui_legend],
  ./views/section_titles[ui_titles],
  ./views/section_scale[ui_scale, ui_coordinates, ui_sequence],
  ./views/section_compare[ui_links, ui_alignment, ui_tooltip, ui_size]
)

data <- geneviewer::ophA_clusters

# Settings sections: nav label, icon, panel title and description, and content
sections <- list(
  data        = list("Data", "database", "Gene data",
                     "Load CSV or GenBank files and pick the columns to use.", ui_data("data", data)),
  theme       = list("Theme", "sparkles", "Theme",
                     "Start from a built-in look and set the fonts and colours of the whole chart.", ui_theme("theme")),
  genes       = list("Genes", "layers", "Genes",
                     "The shape, size and border of the gene markers.", ui_genes("genes")),
  colors      = list("Colors", "circle-half-stroke", "Colors",
                     "Gene colours per group, and patterns for black and white figures.", ui_colors("colors")),
  labels      = list("Gene labels", "tag", "Gene labels",
                     "Text on or above each gene.", ui_labels("labels", data)),
  legend      = list("Legend", "list", "Legend",
                     "The key to the gene colours.", ui_legend("legend")),
  titles      = list("Titles", "file-text", "Titles",
                     "Titles, footers and labels of the clusters.", ui_titles("titles")),
  scale       = list("Scale", "chart-line", "Scale",
                     "The axis, scale breaks and scale bar.", ui_scale("scale")),
  coordinates = list("Coordinates", "hash", "Coordinates",
                     "Start and end positions of the genes.", ui_coordinates("coordinates")),
  sequence    = list("Sequence", "minus", "Sequence line",
                     "The line the genes sit on.", ui_sequence("sequence")),
  links       = list("Gene links", "link", "Gene links",
                     "Connect matching genes between clusters.", ui_links("links", data)),
  alignment   = list("Alignment", "layout-list", "Alignment",
                     "Line the clusters up on one gene.", ui_alignment("alignment", data)),
  tooltip     = list("Tooltip", "message-circle", "Tooltip",
                     "What shows when you hover a gene.", ui_tooltip("tooltip", data)),
  size        = list("Size", "layout-grid", "Size",
                     "Width, height and margins of the chart.", ui_size("size"))
)

nav_groups <- list(
  "Data"        = c("data"),
  "Appearance"  = c("theme", "genes", "colors", "labels", "legend"),
  "Layout"      = c("titles", "scale", "coordinates", "sequence", "size"),
  "Compare"     = c("links", "alignment"),
  "Interaction" = c("tooltip")
)

nav_items <- unlist(lapply(names(nav_groups), function(group) {
  c(
    list(block_nav_label(group)),
    lapply(nav_groups[[group]], function(key) {
      s <- sections[[key]]
      block_nav_item(s[[1]], value = key, icon = s[[2]], selected = key == "data")
    })
  )
}), recursive = FALSE)

settings_panels <- lapply(names(sections), function(key) {
  s <- sections[[key]]
  conditionalPanel(
    sprintf("input.section === '%s'", key),
    block_card(title = s[[3]], description = s[[4]], class = "gv-settings-card", s[[5]])
  )
})

ui <- block_page(
  title = "Gene Cluster Dashboard",
  theme_mode = "light",
  sidebar = block_sidebar(
    title = tags$span(class = "gv-brand", block_icon("chart-bar", color = "primary"), "geneviewer"),
    collapsible = TRUE,
    do.call(block_nav, c(list(id = "section"), nav_items))
  ),
  header = block_header(
    tags$div(
      class = "gv-header",
      tags$div(
        class = "gv-header-title",
        tags$h1("Gene Cluster Dashboard"),
        tags$p("Design gene cluster figures with the geneviewer R package.")
      ),
      block_cluster(
        gap = "sm",
        tags$a(class = "gv-header-link", href = "https://nvelden.github.io/geneviewer/",
               target = "_blank", rel = "noopener", block_icon("external-link", size = "sm"),
               "Documentation"),
        tags$a(class = "gv-header-link", href = "https://github.com/nvelden/geneviewer-shinylive/issues",
               target = "_blank", rel = "noopener", block_icon("message-circle", size = "sm"),
               "Feedback"),
        block_dark_mode_toggle()
      )
    )
  ),
  tags$head(tags$link(rel = "stylesheet", href = "app.css")),
  block_toaster("toasts"),
  tags$div(
    class = "gv-workspace",
    tags$div(class = "gv-settings", settings_panels),
    tags$div(
      class = "gv-preview",
      block_card(
        title = "Preview",
        description = "Hover the chart and use the download icon to save it as SVG or PNG.",
        class = "gv-preview-card",
        tags$div(class = "gv-canvas", GC_chartOutput("gcChart", width = "100%", height = "auto"))
      )
    )
  )
)
