# Theme for plotting ----
ggplot_theme <- 
  ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(size = 14, angle = 0),
    
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )

# No data plot ----
no_data_plot <- function(title = NULL) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Data not available",
             size = 5, fontface = "italic", colour = "black") +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

# Legend for leaflet ----
add_legend <- function(map, colors, labels, sizes, opacity = 1, group, title, layerId) { #map, 
  colorAdditions <- glue::glue(
    "{colors}; border-radius: 50%; width:{sizes}px; height:{sizes}px"
  )
  labelAdditions <- glue::glue(
    "<div style='display: inline-block; height: {sizes}px; ",
    "margin-top: 4px;line-height: {sizes}px;'>{labels}</div>"
  )
  
  return(
    leaflet::addLegend(map,
                       colors = colorAdditions,
                       labels = labelAdditions,
                       opacity = opacity,
                       title = title,
                       position = "topright",
                       group = group,
                       layerId = layerId
    )
  )
}

ensure_sf_ll <- function(x, lon = "longitude_dd", lat = "latitude_dd") {
  if (inherits(x, "sf")) return(x)
  stopifnot(lon %in% names(x), lat %in% names(x))
  sf::st_as_sf(x, coords = c(lon, lat), crs = 4326)
}

has_leafgl <- function() requireNamespace("leafgl", quietly = TRUE)

# Loaders for plots ----
spinnerPlotOutput <- function(outputId, ...) {
  withSpinner(
    plotOutput(outputId, ...),
    color = getOption("spinner.color", default = "#0D576E"),
    type = 6
  )
}

spinnerLeafletOutput <- function(outputId, ...) {
  withSpinner(
    leafletOutput(outputId, ...),
    color = getOption("spinner.color", default = "#0D576E"),
    type = 6
  )
}

spinnerTableOutput <- function(outputId, ...) {
  withSpinner(
    tableOutput(outputId, ...),
    color = getOption("spinner.color", default = "#0D576E"),
    type = 6
  )
}

spinnerUiOutput <- function(outputId, ...) {
  withSpinner(
    uiOutput(outputId, ...),
    color = getOption("spinner.color", default = "#0D576E"),
    type = 6
  )
}