#' Genes and colors sections
#'
#' The gene markers and their style, and the gene colours and patterns.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  ../logic/ui_helpers[color_field, select_field, num_field, text_field, switch_field,
                      field_group, sections, when_on, when_value, register_input,
                      group_style_fields, NONE],
  ../logic/setup[ui_group_picker],
  ../logic/utils[num, txt, opt, compact, parse_delimited_input, parse_named_colors,
                 parse_group_styles, split_unquoted]
)

#' @export
ui_genes <- function(id, data) {
  ns <- NS(id)
  sections(
    field_group(
      "Shape",
      select_field(ns("marker"), "Marker",
                   c("Arrow" = "arrow", "Box arrow" = "boxarrow", "Box" = "box",
                     "Capsule" = "cbox", "Rounded box" = "rbox"), default = "arrow"),
      select_field(ns("markerSize"), "Size",
                   c("Small" = "small", "Medium" = "medium", "Large" = "large"), default = "medium"),
      num_field(ns("markerHeight"), "Height", min = 1, step = 1,
                description = "Empty takes the height from the size.")
    ),
    field_group(
      "Border",
      color_field(ns("stroke"), "Color", default = "black"),
      num_field(ns("strokeWidth"), "Width", min = 0, step = 0.5, default = 1),
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
      num_field(ns("y"), "Vertical", step = 1, default = 50,
                description = "From 0 (bottom) to 100 (top) of the cluster.")
    ),
    field_group(
      "Overlapping genes",
      select_field(ns("overlap"), "Separate",
                   c("Off" = "off", "All overlapping genes" = "all",
                     "Only genes inside another gene" = "nested"), "off",
                   description = "Puts overlapping genes on tracks below each other. Match group styles on Gene track to style each track."),
      when_value(ns, "overlap", not = "off",
        text_field(ns("overlapMin"), "Min overlap", placeholder = "Any overlap",
                   description = "Base pairs, or a share of the shorter gene such as 10%. Genes that overlap by no more stay on one track."),
        num_field(ns("overlapSpacing"), "Track spacing", step = 1, default = 5,
                  description = "Pixels between the tracks; negative values let them overlap.")
      )
    ),
    field_group(
      "Group styles",
      group_style_fields(ns, data, "R, L: marker = box, markerHeight = 38")
    )
  )
}

# The overlap a gene needs to go on its own track: base pairs, or a share
overlap_min <- function(x) {
  x <- txt(x)
  if (is.null(x)) return(NULL)
  if (grepl("%$", x)) x else num(x)
}

#' @export
server_genes <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      overlap <- input$overlap %||% "off"
      list(
        # GC_cluster(): overlapping genes on separate tracks
        cluster = if (overlap != "off") compact(list(
          prevent_gene_overlap = if (overlap == "nested") "nested" else TRUE,
          overlap_min = overlap_min(input$overlapMin),
          overlap_spacing = num(input$overlapSpacing)
        )),
        args = compact(list(
          marker = opt(input$marker), markerSize = opt(input$markerSize),
          style_by = opt(input$styleBy),
          groupStyle = parse_group_styles(input$groupStyle)
        )),
        style = compact(list(
          markerHeight = num(input$markerHeight),
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

PATTERNS <- c("solid", "horizontal", "vertical", "diagonal", "antidiagonal", "grid",
              "crosshatch", "dots", "zigzag", "checkerboard", "rings", "bricks", "plus",
              "wideStripes", "inverseDots")

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
ui_colors <- function(id, data) {
  ns <- NS(id)
  sections(
    field_group("Colour groups", ui_group_picker(id, data)),
    field_group(
      "Palette",
      select_field(ns("colorScheme"), "Palette", PALETTES, default = "schemeCategory10"),
      text_field(ns("customColors"), "Custom colors", placeholder = "red, #1f77b4, ... or Other = grey",
                 description = "One colour per group, in the order of the groups, or colours of named groups: Other = grey. Quote names with commas.",
                 stacked = TRUE)
    ),
    field_group(
      "Patterns",
      switch_field(ns("showPatterns"), "Fill genes with a pattern",
                   description = "For black and white figures, or to mark some groups."),
      when_on(ns, "showPatterns",
        select_field(ns("pattern"), "Pattern", c("One per group" = NONE, PATTERNS), NONE),
        text_field(ns("patternGroups"), "Only groups", placeholder = "All groups",
                   description = "Groups to fill, separated by commas.", stacked = TRUE),
        color_field(ns("patternColor"), "Color", default = "black"),
        num_field(ns("patternSize"), "Size", min = 2, step = 1, default = 6),
        select_field(ns("patternBackground"), "Background",
                     c("White" = "white", "Gene colors" = "none"), default = "white")
      )
    )
  )
}

#' @export
server_colors <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      custom <- txt(input$customColors)
      pattern <- opt(input$pattern)
      groups <- if (!is.null(txt(input$patternGroups))) split_unquoted(input$patternGroups)
      # NULL: a pattern per group in a fixed order; one name: all groups;
      # a named list: only those groups
      patterns <- if (length(groups) > 0) {
        stats::setNames(as.list(rep(pattern %||% "diagonal", length(groups))), groups)
      } else {
        pattern
      }
      list(
        # Colours of named groups, or a list of colours in the order of the groups
        namedColors = parse_named_colors(custom),
        customColors = if (is.null(parse_named_colors(custom))) parse_delimited_input(custom),
        style = compact(list(colorScheme = opt(input$colorScheme))),
        patterns = list(
          show = isTRUE(input$showPatterns),
          args = c(
            compact(list(
              patterns = patterns,
              color = txt(input$patternColor),
              background = if (identical(input$patternBackground, "white")) "white",
              size = num(input$patternSize)
            )),
            # NULL draws the patterns on top of the gene colours
            if (identical(input$patternBackground, "none")) list(background = NULL)
          )
        )
      )
    })
  })
}
