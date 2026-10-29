library(shiny)
library(shinyblocks)
library(geneviewer)

box::use(
  ./views/section_data[ui_data],
  ./views/section_theme[ui_theme],
  ./views/section_genes[ui_genes, ui_colors],
  ./views/section_labels[ui_labels, ui_legend],
  ./views/section_titles[ui_titles],
  ./views/section_element[ui_element, ui_element_scope],
  ./views/section_scale[ui_scale, ui_coordinates, ui_sequence],
  ./views/section_compare[ui_links, ui_alignment, ui_tooltip, ui_size]
)

data <- geneviewer::ophA_clusters

# Settings sections: nav label, icon, panel title and description, and content
sections <- list(
  data        = list("Data", "database", "Gene data",
                     "Load CSV or GenBank files and pick the columns to use.", ui_data("data", data)),
  theme       = list("Theme", "sparkles", "Theme",
                     "Start from a built-in look and set the fonts and colours of the whole chart.", ui_theme("theme")),
  genes       = list("Genes", "layers", "Genes",
                     "The shape, size and border of the gene markers.", ui_genes("genes", data)),
  colors      = list("Colors", "circle-half-stroke", "Colors",
                     "Gene colours per group, and patterns for black and white figures.", ui_colors("colors", data)),
  labels      = list("Gene labels", "tag", "Gene labels",
                     "Text on or above each gene.", ui_labels("labels", data)),
  legend      = list("Legend", "list", "Legend",
                     "The key to the gene colours.", ui_legend("legend", data)),
  element     = list("Element", "edit", "Element",
                     "Click a gene, label, title or legend part in the chart to style it on its own.", ui_element("element")),
  titles      = list("Titles", "file-text", "Titles",
                     "Titles, footers and labels of the clusters.", ui_titles("titles", data)),
  scale       = list("Scale", "chart-line", "Scale",
                     "The axis, scale breaks and scale bar.", ui_scale("scale")),
  coordinates = list("Coordinates", "hash", "Coordinates",
                     "Start and end positions of the genes.", ui_coordinates("coordinates")),
  sequence    = list("Sequence", "minus", "Sequence line",
                     "The line the genes sit on.", ui_sequence("sequence")),
  links       = list("Links", "link", "Links",
                     "Connect matching genes or aligned regions between clusters.", ui_links("links", data)),
  alignment   = list("Alignment", "layout-list", "Alignment",
                     "Line the clusters up on one gene.", ui_alignment("alignment", data)),
  tooltip     = list("Tooltip", "message-circle", "Tooltip",
                     "What shows when you hover a gene.", ui_tooltip("tooltip", data)),
  size        = list("Size", "layout-grid", "Size",
                     "Width, height and margins of the chart.", ui_size("size"))
)

nav_groups <- list(
  "Data"        = c("data"),
  "Appearance"  = c("theme", "genes", "colors", "labels", "legend", "element"),
  "Layout"      = c("titles", "scale", "coordinates", "sequence", "size"),
  "Compare"     = c("links", "alignment"),
  "Interaction" = c("tooltip")
)

nav_items <- unlist(lapply(names(nav_groups), function(group) {
  c(
    list(block_nav_label(group)),
    lapply(nav_groups[[group]], function(key) {
      s <- sections[[key]]
      block_nav_item(s[[1]], value = key, icon = s[[2]], selected = key == "data")
    })
  )
}), recursive = FALSE)

inspector_panels <- lapply(names(sections), function(key) {
  s <- sections[[key]]
  conditionalPanel(
    sprintf("input.section === '%s'", key),
    class = "gv-inspector-panel",
    tags$header(
      class = "gv-inspector-header",
      tags$div(class = "gv-inspector-icon", block_icon(s[[2]], size = "sm")),
      tags$div(
        tags$h2(class = "gv-inspector-title", s[[3]]),
        tags$p(class = "gv-inspector-description", s[[4]])
      )
    ),
    tags$div(class = "gv-inspector-body", s[[5]])
  )
})

ui <- block_page(
  title = "Gene Cluster Dashboard",
  favicon = "favicon-32x32.png",
  theme_mode = "light",
  sidebar = block_sidebar(
    title = tags$span(class = "gv-brand", block_icon("chart-bar", color = "primary"), "geneviewer"),
    collapsible = TRUE,
    do.call(block_nav, c(list(id = "section"), nav_items))
  ),
  # The settings of the selected section; collapse it to give the preview
  # the full width
  sidebar_right = block_sidebar(
    title = "Settings",
    collapsible = TRUE,
    side = "right",
    id = "settings",
    class = "gv-inspector",
    ui_element_scope("element"),
    inspector_panels
  ),
  header = block_header(
    tags$div(
      class = "gv-header",
      tags$div(
        class = "gv-header-title",
        tags$h1("Gene Cluster Dashboard"),
        tags$p("Design gene cluster figures with the geneviewer R package.")
      ),
      block_cluster(
        gap = "sm",
        tags$a(class = "gv-header-link", href = "https://nvelden.github.io/geneviewer/",
               target = "_blank", rel = "noopener", `aria-label` = "Documentation", block_icon("external-link", size = "sm"),
               tags$span(class = "gv-header-link-text", "Documentation")),
        tags$a(class = "gv-header-link", href = "https://github.com/nvelden/geneviewer-shinylive/issues",
               target = "_blank", rel = "noopener", `aria-label` = "Feedback", block_icon("message-circle", size = "sm"),
               tags$span(class = "gv-header-link-text", "Feedback")),
        block_dark_mode_toggle()
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "app.css?v=20261027-scope"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    # Picking a section, or clicking a part of the chart, opens the settings
    # when they are collapsed
    tags$script(HTML("
      document.addEventListener('click', function (event) {
        if (!event.target.closest('#sb-sidebar .sb-nav-item')) return;
        var page = document.querySelector('.sb-page');
        if (page && page.getAttribute('data-sidebar-right-collapsed') === 'true') {
          document.querySelector('#settings .sb-sidebar-toggle').click();
        }
      });
      // Colour pickers edit the same data mapping. Silent server updates do
      // not dispatch change, so only user choices reach this event input.
      document.addEventListener('change', function (event) {
        var target = event.target;
        if (target.tagName === 'SELECT' && target.closest('.gv-group-picker')) {
          Shiny.setInputValue('groupSetup-choice', {column: target.value}, {priority: 'event'});
        }
      });
      // While an example loads, cover the app until the chart of that load
      // finishes drawing, or the chart fails
      var gvLoading = null;
      var gvLoaded = function (token) {
        if (gvLoading !== null && token === gvLoading) {
          gvLoading = null;
          document.getElementById('gv-loading').hidden = true;
        }
      };
      Shiny.addCustomMessageHandler('gv-loading', function (message) {
        gvLoading = message.token;
        document.getElementById('gv-loading').hidden = false;
      });
      Shiny.addCustomMessageHandler('gv-loaded', function (message) {
        gvLoaded(message.token);
      });
      document.addEventListener('geneviewer:dragmove', function (event) {
        var current = Shiny.shinyapp.$inputValues.gcChart_select;
        // A gene label by its row, a legend part by its group
        var key = function (d) {
          return JSON.stringify([d.component, d.cluster, d.part,
                                 d.data && typeof d.data === 'object' ? d.data.rowID : d.data]);
        };
        if (!current || key(current) !== key(event.detail)) return;
        ['moveX', 'moveY'].forEach(function (name) {
          var field = document.querySelector('[data-sb-input-id=\"element-' + name + '\"] input:not([data-shiny-no-bind-input])');
          if (field) field.value = event.detail.position[name];
        });
      });
      document.addEventListener('geneviewer:rendered', function (event) {
        if (event.target.id === 'gcChart') gvLoaded(event.detail.id);
      });
      $(document).on('shiny:error', function (event) {
        if (event.name === 'gcChart') gvLoaded(gvLoading);
      });
      Shiny.addCustomMessageHandler('gv-legend-availability', function (message) {
        var formatting = document.getElementById(message.formatting);
        if (formatting) {
          formatting.disabled = !message.available;
          formatting.inert = !message.available;
          formatting.setAttribute('aria-disabled', String(!message.available));
        }
      });
      Shiny.addCustomMessageHandler('gv-open-settings', function (message) {
        var page = document.querySelector('.sb-page');
        if (page && page.getAttribute('data-sidebar-right-collapsed') === 'true') {
          document.querySelector('#settings .sb-sidebar-toggle').click();
        }
        if (message && message.focus) {
          requestAnimationFrame(function () {
            var mount = document.querySelector('[data-sb-input-id=\"' + message.focus + '\"]');
            var control = mount && mount.querySelector('button, [role=\"combobox\"]');
            if (control) {
              control.scrollIntoView({block: 'center'});
              control.focus();
            }
          });
        }
      });
    "))
  ),
  block_toaster("toasts"),
  # Comes back after the settings of a dataset have arrived (see server.R)
  tags$div(hidden = NA, block_input("gvMarker")),
  tags$div(
    id = "gv-loading", class = "gv-loading", hidden = NA, role = "status",
    tags$div(class = "gv-loading-card", block_spinner(label = "Loading chart"),
             tags$span("Loading chart..."))
  ),
  tags$div(
    class = "gv-workspace",
    tags$div(
      class = "gv-canvas-area",
      tags$div(
        class = "gv-canvas-toolbar",
        tags$div(
          tags$h2(class = "gv-canvas-title", "Preview"),
          tags$p(class = "gv-canvas-hint",
                 "Click any part of the chart to edit it, or drag labels, titles and scales to move them. Hover the chart and use the download icon to save it as SVG or PNG.")
        )
      ),
      tags$div(class = "gv-canvas", GC_chartOutput("gcChart", width = "100%", height = "auto")),
      block_accordion(
        type = "single",
        collapsible = TRUE,
        class = "gv-code",
        block_accordion_item(
          "code", "R code", icon = "code",
          tags$p(class = "gv-code-hint",
                 "The geneviewer code that makes this chart, with the settings you chose."),
          uiOutput("chartCode")
        )
      )
    )
  )
)
