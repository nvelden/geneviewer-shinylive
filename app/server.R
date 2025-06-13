library(shiny)

box::use(
  ./views/view_gene_controls[server_genes],
  ./views/view_label_controls[server_labels],
  ./views/view_scale_controls[server_scale],
  ./views/view_scaleBar_controls[server_scaleBar],
  ./views/view_clusterTitle_controls[server_clusterTitle],
  ./views/view_legend_controls[server_legend]
)

# Define server logic required to draw a histogram
function(input, output, session) {

  r <- reactiveValues(
    input = list(),
    cluster_data  = ophA_clusters  # <-- store your data here
  )

  # inputs
  gene_inputs <- server_genes("geneControls", r = r)
  label_inputs <- server_labels("labelControls", r = r)
  scale_inputs <- server_scale("scaleControls", r = r)
  scaleBar_inputs <- server_scaleBar("scaleBarControls", r = r)
  clusterTitle_inputs <- server_clusterTitle("clusterTitleControls", r = r)
  legend_inputs <- server_legend("legendControls", r = r)

  output$gcChart <- renderGC_chart({

    req(gene_inputs()$geneGroup)
    req(label_inputs()$labelGroup)

    GC_chart(
      r$cluster_data,
      cluster = "cluster",
      group = "class"
    ) %>%
      GC_genes(
        group = gene_inputs()$geneGroup,
        marker = gene_inputs()$marker,
        markerSize = gene_inputs()$markerSize,
        colorScheme = gene_inputs()$colorScheme,
        customColors = NULL
      ) %>%
      GC_labels(
        label = label_inputs()$labelGroup,
        show = label_inputs()$showLabels,
        fontSize = label_inputs()$fontSize,
        fontStyle = label_inputs()$fontStyle,
        fontFamily = label_inputs()$fontFamily,
        fontWeight = label_inputs()$fontWeight,
        fill = label_inputs()$fill
      ) %>%
       GC_scale(
        hidden = scale_inputs()$scaleHidden,
        reverse = scale_inputs()$scaleReverse,
        scale_breaks = scale_inputs()$scaleBreaks,
        ticksCount = scale_inputs()$ticksCount,
        start = scale_inputs()$start,
        end = scale_inputs()$end,
        ticksFormat = scale_inputs()$ticksFormat,
        axis_type = scale_inputs()$axisType,
        axis_position = scale_inputs()$axisPosition
      ) %>%
      GC_scaleBar(
        show = scaleBar_inputs()$showScaleBar,
        title = scaleBar_inputs()$scaleBarTitle,
        scaleBarUnit = scaleBar_inputs()$scaleBarUnit,
        x = scaleBar_inputs()$scaleBarX,
        y = scaleBar_inputs()$scaleBarY
      ) %>%
      GC_clusterTitle(
        title = clusterTitle_inputs()$title,
        subtitle = clusterTitle_inputs()$subtitle,
        titleFont = clusterTitle_inputs()$titleFont,
        subtitleFont = clusterTitle_inputs()$subtitleFont,
        show = clusterTitle_inputs()$show,
        height = clusterTitle_inputs()$height,
        x = clusterTitle_inputs()$x,
        y = clusterTitle_inputs()$y,
        align = clusterTitle_inputs()$align,
        spacing = clusterTitle_inputs()$spacing
      ) %>%
      GC_legend(
        group = gene_inputs()$geneGroup, # Use gene group
        show = legend_inputs()$showLegend,
        position = legend_inputs()$legendPosition,
        x = legend_inputs()$legendX,
        y = legend_inputs()$legendY,
        legendTextOptions = list(
          fontSize = legend_inputs()$legendFontSize,
          fontFamily = legend_inputs()$legendFontFamily,
          fill = legend_inputs()$legendFill
          )
        ) %>%
      GC_clusterLabel(title = "ophA")

  })

}
