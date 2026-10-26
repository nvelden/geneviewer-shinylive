# Gene Cluster Dashboard

An interactive R Shiny application for visualizing and customizing gene cluster data with extensive styling and layout options, build with the [geneviewer](https://github.com/nvelden/geneviewer) R package.


🌐 **[Access the Application](https://nvelden.github.io/geneviewer-shinylive/)** *(Please allow 1-2 minutes for initial loading)*

💬 **Issues or suggestions?** [Open a GitHub Issue](https://github.com/nvelden/geneviewer-shinylive/issues)

## Overview

The Gene Cluster Dashboard is a comprehensive web application built with R Shiny that allows researchers to create publication-ready visualizations of gene clusters. The application provides an intuitive interface for loading gene data and customizing every aspect of the visualization, from gene styling to coordinate systems and legends.

## Features

### Data Loading
- **Multiple file formats**: Support for CSV and GBK (GenBank) files
- **Batch processing**: Upload multiple GBK files simultaneously
- **Intelligent column mapping**: Automatic detection of standard gene annotation columns
- **Example data**: ophA, erythromycin A, Tn7-like transposons, and pQEB1 plasmids with their nucleotide alignment

### Visualization Components
- **Gene representations**: Multiple marker styles (arrow, box, rounded box) with customizable sizes
- **Interactive tooltips**: Configurable data display on hover
- **Scale bars**: Customizable scale indicators with styling options
- **Coordinate systems**: Flexible axis positioning and formatting
- **Legends**: Automatic generation with full styling control
- **Sequence lines**: Visual representation of genomic sequences
- **Gene labels**: Customizable text annotations
- **Cluster titles and footers**: Publication-ready headers and footers

### Advanced Features
- **Gene alignment**: Align genes across clusters based on specific criteria
- **Gene links**: Visual connections between related genes across clusters
- **Scale breaks**: Handle large genomic regions with automatic scale breaks

### Extensive Customization
- **Color schemes**: Built-in color palettes or custom color definitions
- **Typography**: Full control over fonts, sizes, and styling
- **Layout**: Precise positioning and spacing controls
- **Dimensions**: Customizable plot dimensions and margins
- **Styling panels**: Dedicated interfaces for fine-tuning appearance


## Alignment blocks and the pQEB1 example

Choose **Data → Load an example → pQEB1 plasmids and rearranged variants** to reproduce the plasmid comparison, with 401 genes, 14 alignment blocks and orange inversion ribbons. The data are bundled with the development version of geneviewer; no MUMmer installation is needed to load this example.

Under **Links**, choose matching gene values or alignment blocks. Alignment CSV uploads require `cluster1`, `start1`, `end1`, `cluster2`, `start2`, `end2`, and optionally numeric `identity` or `similarity` percentages. Cluster names must match the gene data. Preserve `start > end` for inverted blocks. Choose **Strand only** for solid normal/inverted colors, or a measure for intensity shading. Color bars can be shown or hidden.

Under **Titles → Cluster label**, choose a subtitle column (such as `host` or `strain_short`) to keep subtitles attached to clusters when filtering or reordering them. Select **Manual text** to enter subtitles yourself. The R code panel includes the alignment source and can reproduce the chart outside the app.

## App regression tests

With the current development versions of geneviewer and shinyblocks installed, run `Rscript tests/testthat.R` from the repository root. The tests use Shiny's mock session and do not require a browser.
