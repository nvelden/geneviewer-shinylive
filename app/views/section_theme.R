#' Theme section
#'
#' A built-in geneviewer theme and the settings that apply to the whole chart.
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive, tags],
  shinyblocks[block_alert],
  ../logic/ui_helpers[color_field, select_field, num_field, text_field, field_group, sections,
                      NONE, FONT_FAMILIES],
  ../logic/utils[num, txt, opt, compact]
)

THEMES <- c(
  "None"          = NONE,
  "Default"       = "default",
  "Classic"       = "classic",
  "Dark"          = "dark",
  "Vintage"       = "vintage",
  "Infographic"   = "infographic",
  "Publication"   = "publication",
  "Black & white" = "bw"
)

#' @export
ui_theme <- function(id) {
  ns <- NS(id)
  sections(
    field_group(
      NULL,
      select_field(ns("theme"), "Theme", THEMES, NONE,
                   description = "Publication follows common journal figure guidelines; black & white fills the genes with patterns.")
    ),
    field_group(
      "Text",
      num_field(ns("textScale"), "Scale", 1, min = 0.5, max = 3, step = 0.1),
      select_field(ns("fontFamily"), "Font", c("Theme font" = NONE, FONT_FAMILIES), NONE),
      color_field(ns("textColor"), "Color", placeholder = "Theme color")
    ),
    field_group(
      "Chart",
      color_field(ns("lineColor"), "Line color", placeholder = "Theme color"),
      color_field(ns("background"), "Background", placeholder = "Theme color")
    ),
    field_group(
      NULL,
      block_alert(
        title = "Your settings come first",
        description = "With a theme, the options you change in the other sections override it; everything you leave alone follows the theme.",
        icon = "info"
      )
    )
  )
}

#' @export
server_theme <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      text_scale <- num(input$textScale)
      if (!is.null(text_scale) && text_scale == 1) text_scale <- NULL

      settings <- compact(list(
        theme      = opt(input$theme),
        textScale  = text_scale,
        fontFamily = opt(input$fontFamily),
        textColor  = txt(input$textColor),
        lineColor  = txt(input$lineColor),
        background = txt(input$background)
      ))
      list(active = length(settings) > 0, settings = settings)
    })
  })
}
