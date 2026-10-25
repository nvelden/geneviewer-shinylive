#' Genes and colors sections
#'
#' The gene markers and their style, and the gene colours and patterns.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  shinyblocks[block_toggle_group],
  ../logic/ui_helpers[field, select_field, num_field, text_field, switch_field,
                      field_row, field_group, when_on],
  ../logic/utils[num, txt, compact, parse_delimited_input]
)

#' @export
ui_genes <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    field(ns("marker"), "Marker",
          block_toggle_group(ns("marker"),
                             choices = c("Arrow" = "arrow", "Box arrow" = "boxarrow",
                                         "Box" = "box", "Capsule" = "cbox", "Rounded" = "rbox"),
                             selected = "arrow", variant = "outline", size = "sm")),
    field(ns("markerSize"), "Size",
          block_toggle_group(ns("markerSize"),
                             choices = c("Small" = "small", "Medium" = "medium", "Large" = "large"),
                             selected = "medium", variant = "outline", size = "sm")),
    field_group(
      "Border",
      field_row(
        text_field(ns("stroke"), "Color", "black"),
        num_field(ns("strokeWidth"), "Width", 1, min = 0, step = 0.5)
      ),
      num_field(ns("cornerRadius"), "Corner radius", min = 0, step = 1,
                description = "Rounds the corners of the markers. Empty keeps the marker's own shape.")
    ),
    field_group(
      "Arrowhead and position",
      field_row(
        num_field(ns("arrowheadWidth"), "Arrowhead width", min = 1, step = 1),
        num_field(ns("arrowheadHeight"), "Arrowhead height", min = 1, step = 1)
      ),
      num_field(ns("y"), "Vertical position", 50, step = 1,
                description = "Position of the genes in the cluster, from 0 (bottom) to 100 (top).")
    )
  )
}

#' @export
server_genes <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        args = compact(list(marker = input$marker, markerSize = input$markerSize)),
        style = compact(list(
          stroke = txt(input$stroke),
          strokeWidth = num(input$strokeWidth),
          corner_radius = num(input$cornerRadius),
          arrowheadWidth = num(input$arrowheadWidth),
          arrowheadHeight = num(input$arrowheadHeight),
          y = num(input$y)
        ))
      )
    })
  })
}

PALETTES <- c(
  "Category10 (d3)"            = "schemeCategory10",
  "Okabe-Ito, colour-blind safe" = "okabeIto",
  "Classic"                    = "classic",
  "Vivid"                      = "vivid",
  "Dark"                       = "dark",
  "Vintage"                    = "vintage",
  "Infographic"                = "infographic",
  "Greys"                      = "greys",
  "Blues"                      = "blues",
  "Tableau10 (d3)"             = "schemeTableau10",
  "Accent (d3)"                = "schemeAccent",
  "Dark2 (d3)"                 = "schemeDark2",
  "Paired (d3)"                = "schemePaired",
  "Pastel1 (d3)"               = "schemePastel1",
  "Pastel2 (d3)"               = "schemePastel2",
  "Set1 (d3)"                  = "schemeSet1",
  "Set2 (d3)"                  = "schemeSet2",
  "Set3 (d3)"                  = "schemeSet3"
)

#' @export
ui_colors <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "gv-stack",
    select_field(ns("colorScheme"), "Palette", PALETTES, "schemeCategory10"),
    text_field(ns("customColors"), "Custom colors", placeholder = "red, #1f77b4, ...",
               description = "One colour per group, in the order of the groups. Overrides the palette."),
    field_group(
      "Patterns",
      switch_field(ns("showPatterns"), "Fill genes with a pattern per group"),
      when_on(ns, "showPatterns",
        field_row(
          text_field(ns("patternColor"), "Pattern color", "black"),
          num_field(ns("patternSize"), "Pattern size", 6, min = 2, step = 1)
        ),
        select_field(ns("patternBackground"), "Background",
                     c("White" = "white", "Gene colors" = "none"), "white")
      )
    )
  )
}

#' @export
server_colors <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        customColors = parse_delimited_input(txt(input$customColors)),
        style = compact(list(colorScheme = input$colorScheme)),
        patterns = list(
          show = isTRUE(input$showPatterns),
          color = txt(input$patternColor) %||% "black",
          # NULL draws the patterns on top of the gene colours
          background = if (identical(input$patternBackground, "none")) NULL else "white",
          size = num(input$patternSize) %||% 6
        )
      )
    })
  })
}
