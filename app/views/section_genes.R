#' Genes and colors sections
#'
#' The gene markers and their style, and the gene colours and patterns.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  shinyblocks[block_toggle_group],
  ../logic/ui_helpers[field, select_field, num_field, text_field, switch_field,
                      field_group, sections, when_on],
  ../logic/utils[num, txt, compact, parse_delimited_input]
)

#' @export
ui_genes <- function(id) {
  ns <- NS(id)
  sections(
    field_group(
      "Shape",
      select_field(ns("marker"), "Marker",
                   c("Arrow" = "arrow", "Box arrow" = "boxarrow", "Box" = "box",
                     "Capsule" = "cbox", "Rounded box" = "rbox"), "arrow"),
      field(ns("markerSize"), "Size",
            block_toggle_group(ns("markerSize"),
                               choices = c("Small" = "small", "Medium" = "medium", "Large" = "large"),
                               selected = "medium", variant = "outline", size = "sm"))
    ),
    field_group(
      "Border",
      text_field(ns("stroke"), "Color", "black"),
      num_field(ns("strokeWidth"), "Width", 1, min = 0, step = 0.5),
      num_field(ns("cornerRadius"), "Corner radius", min = 0, step = 1,
                description = "Empty keeps the marker's own corners.")
    ),
    field_group(
      "Arrowhead",
      num_field(ns("arrowheadWidth"), "Width", min = 1, step = 1),
      num_field(ns("arrowheadHeight"), "Height", min = 1, step = 1)
    ),
    field_group(
      "Position",
      num_field(ns("y"), "Vertical", 50, step = 1,
                description = "From 0 (bottom) to 100 (top) of the cluster.")
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
  sections(
    field_group(
      "Palette",
      select_field(ns("colorScheme"), "Palette", PALETTES, "schemeCategory10"),
      text_field(ns("customColors"), "Custom colors", placeholder = "red, #1f77b4, ...",
                 description = "One colour per group, in the order of the groups. Overrides the palette.",
                 stacked = TRUE)
    ),
    field_group(
      "Patterns",
      switch_field(ns("showPatterns"), "Fill genes with a pattern",
                   description = "One pattern per group, for black and white figures."),
      when_on(ns, "showPatterns",
        text_field(ns("patternColor"), "Color", "black"),
        num_field(ns("patternSize"), "Size", 6, min = 2, step = 1),
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
