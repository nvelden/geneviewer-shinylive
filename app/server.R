library(shiny)

box::use(
  ./views/view_load_controls[server_load],
  ./views/view_gene_controls[server_genes],
  ./views/view_label_controls[server_labels],
  ./views/view_labelStyling_controls[server_labelStyling],
  ./views/view_scale_controls[server_scale],
  ./views/view_scaleBar_controls[server_scaleBar],
  ./views/view_clusterTitle_controls[server_clusterTitle],
  ./views/view_clusterTitleStyling_controls[server_clusterTitleStyling],
  ./views/view_clusterFooter_controls[server_clusterFooter],
  ./views/view_clusterFooterStyling_controls[server_clusterFooterStyling],
  ./views/view_clusterLabel_controls[server_clusterLabel],
  ./views/view_clusterLabelStyling_controls[server_clusterLabelStyling],
  ./views/view_legend_controls[server_legend],
  ./views/view_legendStyling_controls[server_legendStyling],
  ./views/view_color_controls[server_color],
  ./views/view_sequence_controls[server_sequence],
  ./views/view_coordinates_controls[server_coordinates],
  ./views/view_gene_alignment_controls[server_align],
  ./views/view_geneLinks_controls[server_links],
  ./views/view_dimension_controls[server_dimensions],
  ./views/view_tooltip_controls[server_tooltip],
  ./logic/utils[make_named_color_list, order_cluster_data],
)

# Define server logic required to draw a histogram
function(input, output, session) {

  r <- reactiveValues(
    input = list(
      start = "start",
      end = "end",
      cluster = "cluster",
      group = "class",
      warning = FALSE # Keep track of warnings
    ),
    cluster_data  = ophA_clusters
  )

  # inputs
  load_inputs <- server_load("loadControls", r = r)
  gene_inputs <- server_genes("geneControls", r = r)
  label_inputs <- server_labels("labelControls", r = r)
  labelStyling_inputs <- server_labelStyling("labelStylingControls", r = r)
  scale_inputs <- server_scale("scaleControls", r = r)
  scaleBar_inputs <- server_scaleBar("scaleBarControls", r = r)
  clusterTitle_inputs <- server_clusterTitle("clusterTitleControls", r = r)
  clusterTitleStyling_inputs <- server_clusterTitleStyling("clusterTitleStylingControls", r = r)
  clusterFooter_inputs <- server_clusterFooter("clusterFooterControls", r = r)
  clusterFooterStyling_inputs <- server_clusterFooterStyling("clusterFooterStylingControls", r = r)
  clusterLabel_inputs <- server_clusterLabel("clusterLabelControls", r = r)
  clusterLabelStyling_inputs <- server_clusterLabelStyling("clusterLabelStylingControls", r = r)
  legend_inputs <- server_legend("legendControls", r = r)
  legendStyling_inputs <- server_legendStyling("legendStylingControls", r = r)
  color_inputs <- server_color("colorControls", r = r)
  sequence_inputs <- server_sequence("sequenceControls", r = r)
  coordinates_inputs <- server_coordinates("coordinatesControls", r = r)
  alignment_inputs <- server_align("alignmentControls", r = r)
  links_inputs <- server_links("linksControls", r = r)
  tooltip_inputs <- server_tooltip("tooltipControls", r = r)
  dimensions_inputs <- server_dimensions("dimensionsControls", r = r)

  output$gcChart <- renderGC_chart({

    # Order cluster data
    cluster_data_ordered <- order_cluster_data(
      data = r$cluster_data,
      cluster_col = r$input[["cluster"]],
      order_vec = r$input[["select_order"]]
    )

    GC_chart_object <- tryCatch({

      withCallingHandlers({
        GC_chart(
          cluster_data_ordered,
          start = r$input[["start"]],
          end = r$input[["end"]],
          cluster = r$input[["cluster"]],
          group = r$input[["group"]],
          strand = r$input[["strand"]],
          width = dimensions_inputs()$width,
          height = dimensions_inputs()$height
        ) %>%
          GC_grid(margin = dimensions_inputs()$margin) %>%
          GC_genes(
            group = r$input[["group"]],
            marker = gene_inputs()$marker,
            markerSize = gene_inputs()$markerSize
          ) %>%
          GC_labels(
            label = label_inputs()$labelGroup,
            show = label_inputs()$showLabels,
            fontSize = labelStyling_inputs()$fontSize,
            fontStyle = labelStyling_inputs()$fontStyle,
            fontFamily = labelStyling_inputs()$fontFamily,
            fontWeight = labelStyling_inputs()$fontWeight,
            fill = labelStyling_inputs()$fill
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
            titleFont = clusterTitleStyling_inputs()$titleFont,
            subtitleFont = clusterTitleStyling_inputs()$subtitleFont,
            show = clusterTitle_inputs()$show,
            height = clusterTitleStyling_inputs()$height,
            x = clusterTitleStyling_inputs()$x,
            y = clusterTitleStyling_inputs()$y,
            align = clusterTitleStyling_inputs()$align,
            spacing = clusterTitleStyling_inputs()$spacing
          ) %>%
          GC_clusterFooter(
            title = clusterFooter_inputs()$title,
            subtitle = clusterFooter_inputs()$subtitle,
            show = clusterFooter_inputs()$show,
            subtitleFont = clusterFooterStyling_inputs()$subtitleFont,
            titleFont = clusterFooterStyling_inputs()$titleFont,
            height = clusterFooterStyling_inputs()$height,
            x = clusterFooterStyling_inputs()$x,
            y = clusterFooterStyling_inputs()$y,
            align = clusterFooterStyling_inputs()$align,
            spacing = clusterFooterStyling_inputs()$spacing
          ) %>%
          GC_legend(
            show = legend_inputs()$showLegend,
            group = r$input[["group"]],
            position = legend_inputs()$legendPosition,
            x = legend_inputs()$legendX,
            y = legend_inputs()$legendY,
            legendTextOptions = list(
              fontSize = legendStyling_inputs()$legendFontSize,
              fontFamily = legendStyling_inputs()$legendFontFamily,
              fill = legendStyling_inputs()$legendFill
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
            show = clusterLabel_inputs()$show,
            title = clusterLabel_inputs()$text,
            width = clusterLabelStyling_inputs()$width,
            position = clusterLabelStyling_inputs()$position,
            wrapLabel = clusterLabelStyling_inputs()$wrapLabel,
            x = clusterLabelStyling_inputs()$x,
            y = clusterLabelStyling_inputs()$y,
            fontSize = clusterLabelStyling_inputs()$fontSize,
            fontStyle = clusterLabelStyling_inputs()$fontStyle,
            fontWeight = clusterLabelStyling_inputs()$fontWeight,
            fontFamily = clusterLabelStyling_inputs()$fontFamily,
            fill = clusterLabelStyling_inputs()$fill
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
          ) %>%
          GC_tooltip(
            show = tooltip_inputs()$show,
            formatter = tooltip_inputs()$formatter
          )
      },
      warning = function(w) {
        if (!r$input[["warning"]]) {
          showNotification(
            paste("Warning:", conditionMessage(w)),
            type = "warning",
            duration = 5
          )
          r$input[["warning"]] <- TRUE # Set flag to prevent showing more warnings
        }
        # suppress warning from propagating
        invokeRestart("muffleWarning")
      })
    },
    error = function(e) {
      showNotification(
        paste("Error:", conditionMessage(e)),
        type = "error",
        duration = 8
      )
      # Return NULL or some placeholder object to avoid crash
      NULL
    }
    )

    ####################### Gene alignment #####################################
    if(!is.null(alignment_inputs()$alignGenes) && alignment_inputs()$alignGenes){
      # Display alignment warnings/errors in shiny app
      tryCatch(
        withCallingHandlers({
          GC_chart_object <-
            GC_chart_object %>%
            GC_align(
              id_column = alignment_inputs()$idColumn,
              id = alignment_inputs()$id,
              align = alignment_inputs()$align
            )
        },
        warning = function(w) {
          # Show the warning in the UI as a yellow notification
          showNotification(
            paste("Warning:", conditionMessage(w)),
            type = "warning",
            duration = 5
          )
          # Prevent the warning from also bubbling up to the console
          invokeRestart("muffleWarning")
        }),
        error = function(e) {
          # Show the error in the UI as a red notification
          showNotification(
            paste("Error:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          # Return the original object unchanged when there's an error
          GC_chart_object
        }
      )
    }
    ####################### Gene alignment #####################################
    if(!is.null(alignment_inputs()$alignGenes) && alignment_inputs()$alignGenes){
      # Display alignment warnings/errors in shiny app
      tryCatch(
        withCallingHandlers({
          GC_chart_object <-
            GC_chart_object %>%
            GC_align(
              id_column = alignment_inputs()$idColumn,
              id = alignment_inputs()$id,
              align = alignment_inputs()$align
            )
        },
        warning = function(w) {
          # Show the warning in the UI as a yellow notification
          showNotification(
            paste("Warning:", conditionMessage(w)),
            type = "warning",
            duration = 5
          )
          # Prevent the warning from also bubbling up to the console
          invokeRestart("muffleWarning")
        }),
        error = function(e) {
          # Show the error in the UI as a red notification
          showNotification(
            paste("Error:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          # Return the original object unchanged when there's an error
          GC_chart_object
        }
      )
    }
    ####################### Gene links #########################################
    if(!is.null(links_inputs()$show) && links_inputs()$show){
      # Display alignment warnings/errors in shiny app
      tryCatch({
        GC_chart_object <-
          GC_chart_object %>%
          GC_links(
            group = links_inputs()$group,
            curve = links_inputs()$curve,
            value1 = links_inputs()$value1,
            value2 = links_inputs()$value2,
            use_group_colors = links_inputs()$colorByGroup
          )
      },
      error = function(e) {
        # Show the error in the UI as a red notification
        showNotification(
          paste("Error:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        # Return the original object unchanged
        GC_chart_object
      },
      warning = function(w) {
        # Show warnings in the UI as yellow notifications
        showNotification(
          paste("Warning:", conditionMessage(w)),
          type = "warning",
          duration = 5
        )
        # Continue execution and suppress the warning
        invokeRestart("muffleWarning")
      })
    }

    GC_chart_object

  })


}
