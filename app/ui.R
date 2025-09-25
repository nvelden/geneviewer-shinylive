library(shiny)
library(shinyjqui)
library(shinydashboard)
library(shinydashboardPlus)
library(geneviewer)

box::use(
  ./views/view_load_controls[ui_load],
  ./views/view_gene_controls[ui_genes],
  ./views/view_geneStyling_controls[ui_geneStyling],
  ./views/view_label_controls[ui_labels],
  ./views/view_labelStyling_controls[ui_labelStyling],
  ./views/view_scale_controls[ui_scale],
  ./views/view_scaleStyling_controls[ui_scaleStyling],
  ./views/view_scaleBar_controls[ui_scaleBar],
  ./views/view_scaleBarStyling_controls[ui_scaleBarStyling],
  ./views/view_clusterTitle_controls[ui_clusterTitle],
  ./views/view_clusterTitleStyling_controls[ui_clusterTitleStyling],
  ./views/view_clusterFooter_controls[ui_clusterFooter],
  ./views/view_clusterFooterStyling_controls[ui_clusterFooterStyling],
  ./views/view_clusterLabel_controls[ui_clusterLabel],
  ./views/view_clusterlabelStyling_controls[ui_clusterlabelStyling],
  ./views/view_legend_controls[ui_legend],
  ./views/view_legendStyling_controls[ui_legendStyling],
  ./views/view_color_controls[ui_color],
  ./views/view_sequence_controls[ui_sequence],
  ./views/view_coordinates_controls[ui_coordinates],
  ./views/view_coordinatesStyling_controls[ui_coordinatesStyling],
  ./views/view_gene_alignment_controls[ui_align],
  ./views/view_dimension_controls[ui_dimensions],
  ./views/view_geneLinks_controls[ui_links],
  ./views/view_geneLinksStyling_controls[ui_geneLinksStyling],
  ./views/view_tooltip_controls[ui_tooltip]
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Gene Cluster Dashboard"
  ),
  dashboardSidebar(
    tags$style(HTML("
  .compact-inputs .col-sm-6 {
    padding: 0px !important;
  ")),
    sidebarMenu(
      ui_load("loadControls"),
      ui_genes("geneControls"),
      ui_sequence("sequenceControls"),
      ui_labels("labelControls"),
      ui_links("linksControls"),
      ui_clusterTitle("clusterTitleControls"),
      ui_clusterLabel("clusterLabelControls"),
      ui_clusterFooter("clusterFooterControls"),
      ui_legend("legendControls"),
      ui_scale("scaleControls"),
      ui_scaleBar("scaleBarControls"),
      ui_coordinates("coordinatesControls"),
      ui_align("alignmentControls"),
      ui_tooltip("tooltipControls"),
      ui_color("colorControls"),
      ui_dimensions("dimensionsControls")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    ui_legendStyling("legendStylingControls"),
    ui_labelStyling("labelStylingControls"),
    ui_clusterFooterStyling("clusterFooterStylingControls"),
    ui_clusterTitleStyling("clusterTitleStylingControls"),
    ui_clusterLabelStyling("clusterLabelStylingControls"),
    ui_coordinatesStyling("coordinatesStylingControls"),
    ui_scaleStyling("scaleStylingControls"),
    ui_geneStyling("geneStylingControls"),
    ui_geneLinksStyling("geneLinksStylingControls"),
    ui_scaleBarStyling("scaleBarStylingControls"),
    fluidRow(
      box(
        title       = "Gene Cluster Chart",
        status      = "primary",
        solidHeader = TRUE,
        width       = 12,
        GC_chartOutput("gcChart", width="100%", height="400px")
      )
    )
  )
)

