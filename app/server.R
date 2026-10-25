library(shiny)

box::use(
  ./views/view_load_controls[server_load],
  ./views/view_gene_controls[server_genes],
  ./views/view_geneStyling_controls[server_geneStyling],
  ./views/view_label_controls[server_labels],
  ./views/view_labelStyling_controls[server_labelStyling],
  ./views/view_scale_controls[server_scale],
  ./views/view_scaleStyling_controls[server_scaleStyling],
  ./views/view_scaleBar_controls[server_scaleBar],
  ./views/view_scaleBarStyling_controls[server_scaleBarStyling],
  ./views/view_clusterTitle_controls[server_clusterTitle],
  ./views/view_clusterTitleStyling_controls[server_clusterTitleStyling],
  ./views/view_clusterFooter_controls[server_clusterFooter],
  ./views/view_clusterFooterStyling_controls[server_clusterFooterStyling],
  ./views/view_clusterLabel_controls[server_clusterLabel],
  ./views/view_clusterLabelStyling_controls[server_clusterLabelStyling],
  ./views/view_legend_controls[server_legend],
  ./views/view_legendStyling_controls[server_legendStyling],
  ./views/view_color_controls[server_color, server_pattern],
  ./views/view_theme_controls[server_theme],
  ./views/view_sequence_controls[server_sequence],
  ./views/view_coordinates_controls[server_coordinates],
  ./views/view_coordinatesStyling_controls[server_coordinatesStyling],
  ./views/view_gene_alignment_controls[server_align],
  ./views/view_geneLinks_controls[server_links],
  ./views/view_geneLinksStyling_controls[server_geneLinksStyling],
  ./views/view_dimension_controls[server_dimensions],
  ./views/view_tooltip_controls[server_tooltip],
  ./logic/utils[make_named_color_list, order_cluster_data, drop_unchanged, call_gc],
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
  geneStyling_inputs <- server_geneStyling("geneStylingControls", r = r)
  label_inputs <- server_labels("labelControls", r = r)
  labelStyling_inputs <- server_labelStyling("labelStylingControls", r = r)
  scale_inputs <- server_scale("scaleControls", r = r)
  scaleStyling_inputs <- server_scaleStyling("scaleStylingControls", r = r)
  scaleBar_inputs <- server_scaleBar("scaleBarControls", r = r)
  scaleBarStyling_inputs <- server_scaleBarStyling("scaleBarStylingControls", r = r)
  clusterTitle_inputs <- server_clusterTitle("clusterTitleControls", r = r)
  clusterTitleStyling_inputs <- server_clusterTitleStyling("clusterTitleStylingControls", r = r)
  clusterFooter_inputs <- server_clusterFooter("clusterFooterControls", r = r)
  clusterFooterStyling_inputs <- server_clusterFooterStyling("clusterFooterStylingControls", r = r)
  clusterLabel_inputs <- server_clusterLabel("clusterLabelControls", r = r)
  clusterLabelStyling_inputs <- server_clusterLabelStyling("clusterLabelStylingControls", r = r)
  legend_inputs <- server_legend("legendControls", r = r)
  legendStyling_inputs <- server_legendStyling("legendStylingControls", r = r)
  color_inputs <- server_color("colorControls", r = r)
  pattern_inputs <- server_pattern("colorControls", r = r)
  theme_inputs <- server_theme("themeControls", r = r)
  sequence_inputs <- server_sequence("sequenceControls", r = r)
  coordinates_inputs <- server_coordinates("coordinatesControls", r = r)
  coordinatesStyling_inputs <- server_coordinatesStyling("coordinatesStylingControls", r = r)
  alignment_inputs <- server_align("alignmentControls", r = r)
  links_inputs <- server_links("linksControls", r = r)
  geneLinksStyling_inputs <- server_geneLinksStyling("geneLinksStylingControls", r = r)
  tooltip_inputs <- server_tooltip("tooltipControls", r = r)
  dimensions_inputs <- server_dimensions("dimensionsControls", r = r)

  # The first value of each set of styling options is its app default. With a
  # theme, options left at their default are dropped, so the theme sets them.
  style_defaults <- new.env()
  themed <- function(name, x) {
    if (!exists(name, envir = style_defaults, inherits = FALSE)) {
      assign(name, x, envir = style_defaults)
    }
    if (!isTRUE(theme_inputs()$active)) return(x)
    drop_unchanged(x, get(name, envir = style_defaults)) %||% list()
  }

  output$gcChart <- renderGC_chart({

    # Order cluster data
    cluster_data_ordered <- order_cluster_data(
      data = r$cluster_data,
      cluster_col = r$input[["cluster"]],
      order_vec = r$input[["select_order"]]
    )

    theme <- theme_inputs()
    genes <- themed("genes", gene_inputs())
    geneStyling <- themed("geneStyling", geneStyling_inputs())
    labelStyling <- themed("labelStyling", labelStyling_inputs())
    scaleStyling <- themed("scaleStyling", scaleStyling_inputs())
    scaleBarStyling <- themed("scaleBarStyling", scaleBarStyling_inputs())
    clusterTitleStyling <- themed("clusterTitleStyling", clusterTitleStyling_inputs())
    clusterFooterStyling <- themed("clusterFooterStyling", clusterFooterStyling_inputs())
    clusterLabelStyling <- themed("clusterLabelStyling", clusterLabelStyling_inputs())
    legend <- themed("legend", legend_inputs())
    legendStyling <- themed("legendStyling", legendStyling_inputs())
    sequence <- themed("sequence", sequence_inputs())
    coordinatesStyling <- themed("coordinatesStyling", coordinatesStyling_inputs())
    colors <- themed("colors", color_inputs())
    patterns <- pattern_inputs()

    GC_chart_object <- tryCatch({

      withCallingHandlers({
        chart <- GC_chart(
          cluster_data_ordered,
          start = r$input[["start"]],
          end = r$input[["end"]],
          cluster = r$input[["cluster"]],
          group = r$input[["group"]],
          strand = r$input[["strand"]],
          width = dimensions_inputs()$width,
          height = dimensions_inputs()$height
        ) %>%
          GC_grid(margin = dimensions_inputs()$margin)

        if (theme$active) {
          chart <- do.call(GC_theme, c(list(chart), theme$settings))
        }

        chart <- chart %>%
          call_gc(GC_genes, .,
            group = r$input[["group"]],
            marker = genes$marker,
            markerSize = genes$markerSize,
            stroke = geneStyling$strokeColor,
            strokeWidth = geneStyling$strokeWidth,
            y = geneStyling$y,
            arrowheadWidth = geneStyling$arrowheadWidth,
            arrowheadHeight = geneStyling$arrowheadHeight,
            corner_radius = geneStyling$cornerRadius
          ) %>%
          call_gc(GC_labels, .,
            label = label_inputs()$labelGroup,
            show = label_inputs()$showLabels,
            fontSize = labelStyling$fontSize,
            fontStyle = labelStyling$fontStyle,
            fontFamily = labelStyling$fontFamily,
            fontWeight = labelStyling$fontWeight,
            fill = labelStyling$fill
          ) %>%
          call_gc(GC_scale, .,
            # From scale controls
            hidden = scale_inputs()$scaleHidden,
            reverse = scale_inputs()$scaleReverse,
            scale_breaks = scale_inputs()$scaleBreaks,
            padding = scaleStyling$padding,
            axis_type = scaleStyling$axisType,
            axis_position = scaleStyling$axisPosition,
            ticksCount = scaleStyling$ticksCount,
            ticksFormat = scaleStyling$ticksFormat,
            scale_break_threshold = scaleStyling$scaleBreakThreshold,
            scale_break_padding = scaleStyling$scaleBreakPadding,
            y = scaleStyling$y,
            tickStyle = scaleStyling$tickStyle,
            textStyle = scaleStyling$textStyle,
            lineStyle = scaleStyling$lineStyle
          ) %>%
          call_gc(GC_scaleBar, .,
            show = scaleBar_inputs()$showScaleBar,
            title = scaleBar_inputs()$scaleBarTitle,
            scaleBarUnit = scaleBar_inputs()$scaleBarUnit,
            x = scaleBarStyling$x,
            y = scaleBarStyling$y,
            labelStyle = scaleBarStyling$labelStyle,
            scaleBarLineStyle = scaleBarStyling$scaleBarLineStyle,
            scaleBarTickStyle = scaleBarStyling$scaleBarTickStyle,
            textPadding = scaleBarStyling$textPadding
          ) %>%
          call_gc(GC_clusterTitle, .,
            title = clusterTitle_inputs()$title,
            subtitle = clusterTitle_inputs()$subtitle,
            titleFont = clusterTitleStyling$titleFont,
            subtitleFont = clusterTitleStyling$subtitleFont,
            show = clusterTitle_inputs()$show,
            height = clusterTitleStyling$height,
            x = clusterTitleStyling$x,
            y = clusterTitleStyling$y,
            align = clusterTitleStyling$align,
            spacing = clusterTitleStyling$spacing
          ) %>%
          call_gc(GC_clusterFooter, .,
            title = clusterFooter_inputs()$title,
            subtitle = clusterFooter_inputs()$subtitle,
            show = clusterFooter_inputs()$show,
            subtitleFont = clusterFooterStyling$subtitleFont,
            titleFont = clusterFooterStyling$titleFont,
            height = clusterFooterStyling$height,
            x = clusterFooterStyling$x,
            y = clusterFooterStyling$y,
            align = clusterFooterStyling$align,
            spacing = clusterFooterStyling$spacing
          ) %>%
          call_gc(GC_legend, .,
            show = legend$showLegend,
            group = r$input[["group"]],
            position = legend$legendPosition,
            align = legend$legendAlign,
            x = legend$legendX,
            y = legend$legendY,
            corner_radius = legendStyling$cornerRadius,
            legendTextOptions = Filter(Negate(is.null), list(
              fontSize = legendStyling$legendFontSize,
              fontFamily = legendStyling$legendFontFamily,
              fill = legendStyling$legendFill
            ))
          ) %>%
          call_gc(GC_coordinates, .,
            show = coordinates_inputs()$showCoordinates,
            ticksFormat = coordinates_inputs()$ticksFormat,
            yPositionTop = coordinatesStyling$yPositionTop,
            yPositionBottom = coordinatesStyling$yPositionBottom,
            rotate = coordinatesStyling$rotate,
            overlapThreshold = coordinatesStyling$overlapThreshold,
            tickStyle = coordinatesStyling$tickStyle,
            textStyle = coordinatesStyling$textStyle
          ) %>%
          call_gc(GC_clusterLabel, .,
            show = clusterLabel_inputs()$show,
            title = clusterLabel_inputs()$text,
            subtitle = clusterLabel_inputs()$subtitle,
            width = clusterLabelStyling$width,
            position = clusterLabelStyling$position,
            align = clusterLabelStyling$align,
            spacing = clusterLabelStyling$spacing,
            subtitleFont = clusterLabelStyling$subtitleFont,
            wrapLabel = clusterLabelStyling$wrapLabel,
            x = clusterLabelStyling$x,
            y = clusterLabelStyling$y,
            fontSize = clusterLabelStyling$fontSize,
            fontStyle = clusterLabelStyling$fontStyle,
            fontWeight = clusterLabelStyling$fontWeight,
            fontFamily = clusterLabelStyling$fontFamily,
            fill = clusterLabelStyling$fill
          ) %>%
          call_gc(GC_sequence, .,
            show = sequence$showSequence,
            y = sequence$sequenceY,
            sequenceStyle = Filter(Negate(is.null), list(
              stroke = sequence$sequenceStroke,
              strokeWidth = sequence$sequenceStrokeWidth
            )),
            markerStyle = Filter(Negate(is.null), list(
              stroke = sequence$sequenceMarkerStroke,
              strokeWidth = sequence$sequenceMarkerStrokeWidth
            ))
          ) %>%
          call_gc(GC_color, .,
            colorScheme = if (
              !is.null(color_inputs()$customColors) &&
              any(nzchar(color_inputs()$customColors))
            ) NULL else colors$colorScheme,
            customColors = make_named_color_list(r$cluster_data, gene_inputs()$geneGroup, color_inputs()$customColors)
          ) %>%
          call_gc(GC_tooltip, .,
            show = tooltip_inputs()$show,
            formatter = tooltip_inputs()$formatter
          )

        if (patterns$show) {
          chart <- GC_pattern(
            chart,
            color = patterns$color,
            # NULL draws the patterns on top of the gene colours
            background = patterns$background,
            size = if (is.null(patterns$size) || is.na(patterns$size)) 6 else patterns$size
          )
        }

        if (isTRUE(tooltip_inputs()$trackMouse)) {
          chart <- GC_trackMouse(chart)
        }

        chart
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
    ####################### Gene links #########################################
    linksStyling <- themed("linksStyling", geneLinksStyling_inputs())
    if(!is.null(links_inputs()$show) && links_inputs()$show){
      # Display alignment warnings/errors in shiny app
      tryCatch({
        GC_chart_object <-
          GC_chart_object %>%
          call_gc(GC_links, .,
            group = links_inputs()$group,
            curve = linksStyling$curve,
            value1 = links_inputs()$value1,
            value2 = links_inputs()$value2,
            use_group_colors = linksStyling$use_group_colors,
            normal_color = linksStyling$normal_color,
            inverted_color = linksStyling$inverted_color,
            low_color = linksStyling$low_color,
            vmin = linksStyling$vmin,
            vmax = linksStyling$vmax,
            linkWidth = linksStyling$linkWidth,
            linkStyle = linksStyling$linkStyle
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
