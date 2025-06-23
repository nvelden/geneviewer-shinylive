library(shiny)

box::use(
  ./views/view_gene_controls[server_genes],
  ./views/view_label_controls[server_labels],
  ./views/view_scale_controls[server_scale],
  ./views/view_scaleBar_controls[server_scaleBar],
  ./views/view_clusterTitle_controls[server_clusterTitle],
  ./views/view_clusterLabel_controls[server_clusterLabel],
  ./views/view_legend_controls[server_legend],
  ./views/view_color_controls[server_color],
  ./views/view_sequence_controls[server_sequence],
  ./views/view_coordinates_controls[server_coordinates],
  ./views/view_gene_alignment_controls[server_align],
  ./logic/utils[make_named_color_list]
)

# Define server logic required to draw a histogram
function(input, output, session) {

  r <- reactiveValues(
    input = list(),
    cluster_data  = ophA_clusters
  )

  # inputs
  gene_inputs <- server_genes("geneControls", r = r)
  label_inputs <- server_labels("labelControls", r = r)
  scale_inputs <- server_scale("scaleControls", r = r)
  scaleBar_inputs <- server_scaleBar("scaleBarControls", r = r)
  clusterTitle_inputs <- server_clusterTitle("clusterTitleControls", r = r)
  clusterLabel_inputs <- server_clusterLabel("clusterLabelControls", r = r)
  legend_inputs <- server_legend("legendControls", r = r)
  color_inputs <- server_color("colorControls", r = r)
  sequence_inputs <- server_sequence("sequenceControls", r = r)
  coordinates_inputs <- server_coordinates("coordinatesControls", r = r)
  alignment_inputs <- server_align("alignmentControls", r = r)

  output$gcChart <- renderGC_chart({

    req(gene_inputs()$geneGroup)
    req(label_inputs()$labelGroup)

    GC_chart_object <-
    GC_chart(
      r$cluster_data,
      cluster = "cluster",
      group = "class"
    ) %>%
      GC_genes(
        group = gene_inputs()$geneGroup,
        marker = gene_inputs()$marker,
        markerSize = gene_inputs()$markerSize
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
        show = legend_inputs()$showLegend,
        group = gene_inputs()$geneGroup, # Use gene group
        position = legend_inputs()$legendPosition,
        x = legend_inputs()$legendX,
        y = legend_inputs()$legendY,
        legendTextOptions = list(
          fontSize = legend_inputs()$legendFontSize,
          fontFamily = legend_inputs()$legendFontFamily,
          fill = legend_inputs()$legendFill
          )
        ) %>%
        GC_coordinates(
          show = coordinates_inputs()$showCoordinates,
          yPositionTop = coordinates_inputs()$yPositionTop,
          yPositionBottom = coordinates_inputs()$yPositionBottom,
          ticksFormat = coordinates_inputs()$ticksFormat,
          tickStyle = list(
            stroke = coordinates_inputs()$tickStroke,
            strokeWidth = coordinates_inputs()$tickStrokeWidth,
            lineLength = coordinates_inputs()$tickLineLength
          ),
          textStyle = list(
          fill = coordinates_inputs()$textFill,
          fontSize = coordinates_inputs()$fontSize,
          fontFamily = coordinates_inputs()$fontFamily
          )
        ) %>%
        GC_clusterLabel(
          show = clusterLabel_inputs()$showCluster,
          title = clusterLabel_inputs()$clusterLabel,
          x = clusterLabel_inputs()$clusterLabelX,
          y = clusterLabel_inputs()$clusterLabelY,
          position = clusterLabel_inputs()$clusterLabelPosition,
          fontSize = clusterLabel_inputs()$clusterLabelFontSize,
          fontStyle = clusterLabel_inputs()$clusterLabelFontStyle,
          fontWeight = clusterLabel_inputs()$clusterLabelFontWeight,
          fill = clusterLabel_inputs()$clusterLabelColor
        ) %>%
      GC_sequence(
        show = sequence_inputs()$showSequence,
        y = sequence_inputs()$sequenceY,
        sequenceStyle = list(
          stroke = sequence_inputs()$sequenceStroke,
          strokeWidth = sequence_inputs()$sequenceStrokeWidth
        ),
        markerStyle = list(
          stroke = sequence_inputs()$sequenceMarkerStroke,
          strokeWidth = sequence_inputs()$sequenceMarkerStrokeWidth
        )
      ) %>%
        GC_color(
          colorScheme = if (
            !is.null(color_inputs()$customColors) &&
            any(nzchar(color_inputs()$customColors))
          ) NULL else color_inputs()$colorScheme,
          customColors = make_named_color_list(r$cluster_data, gene_inputs()$geneGroup, color_inputs()$customColors)
        )

    # Display alignment warnings in shiny app
    withCallingHandlers({
    GC_chart_object %>%
    GC_align(
      id_column = alignment_inputs()$idColumn,
      id = alignment_inputs()$id,
      align = alignment_inputs()$align
    )
    },
    warning = function(w) {
      # show the warning in the UI as a yellow notification
      showNotification(
        paste("Warning:", conditionMessage(w)),
        type = "warning",
        duration = 5
      )
      # prevent the warning from also bubbling up to the console
      invokeRestart("muffleWarning")
    })
  })

}
