#' Zoom section: the zoom slider below the chart
'.__module__.'

box::use(
  shiny[NS, moduleServer, reactive],
  ../logic/ui_helpers[num_field, select_field, switch_field, field_group, sections, when_on],
  ../logic/utils[num, opt]
)

#' @export
ui_zoom <- function(id) {
  ns <- NS(id)
  sections(
    field_group(
      NULL,
      switch_field(ns("show"), "Show zoom slider")
    ),
    when_on(ns, "show",
      field_group(
        "Window",
        select_field(ns("mode"), "Applies to",
                     c("Same share of each cluster" = "relative",
                       "Same base pairs in all clusters" = "absolute"),
                     default = "relative", stacked = TRUE),
        select_field(ns("update"), "Redraw",
                     c("While dragging" = "live", "When released" = "release"),
                     default = "live")
      ),
      field_group(
        "Slider",
        switch_field(ns("overview"), "Show the genes in the slider", TRUE),
        num_field(ns("height"), "Height (px)", 24, min = 12, max = 80, step = 2)
      )
    )
  )
}

#' The options of GC_zoom(). The window itself is kept by the app (see
#' server.R): the chart reports it, and keeps it when it is drawn again.
#' @export
server_zoom <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        show = isTRUE(input$show),
        args = list(
          mode = opt(input$mode),
          update = opt(input$update),
          overview = isTRUE(input$overview),
          height = num(input$height)
        )
      )
    })
  })
}
