# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 20, current_zoom = 9) {
  leaflet() |>
    addTiles(
      group = "OpenStreetMap",
      options = tileOptions(minZoom = 4, maxZoom = max_zoom)
    ) |>
    addProviderTiles(
      providers$Esri.WorldImagery,
      group = "Satellite",
      options = providerTileOptions(minZoom = 4, maxZoom = max_zoom)
    ) %>%
    setView(lng = mean(nsw_bruv_data$bruv_metadata$longitude_dd),
            lat = mean_lat, current_zoom) |>
    # addMapPane("polys",  zIndex = 410) |>
    # addMapPane("points", zIndex = 420) |>
    
    # TODO add NSW Marine Parks
    # Use regular polygons for static layers:
    addPolygons(
      data = state_mp,
      color = "black", weight = 1,
      fillColor = ~state_pal(zone_type), fillOpacity = 0.8,
      group = "NSW Marine Parks",
      popup = ~comments,
      label = ~comments#,
      # options = pathOptions(pane = "polys")
    ) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      # options = pathOptions(pane = "polys"), 
      group = "Commonwealth Marine Parks"
    ) %>%
    
    # Legends

    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Commonwealth Marine Parks",
      position = "bottomright",
      group = "Commonwealth Marine Parks"
    ) %>%
    addLegend(
      pal = state_pal,
      values = state_mp$zone_type,
      opacity = 1,
      title = "NSW Marine Parks",
      position = "bottomright",
      group = "NSW Marine Parks"
    )
}

# ---- output id helpers -------------------------------------------------------
# TODO move this into the helpers code
metric_output_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_", which, "_", metric_id)
}

metric_plot_id <- function(prefix, metric_id, which) {
  metric_output_id(prefix, metric_id, which)
}

metric_map_id <- function(prefix, metric_id, which = "map") {
  metric_output_id(prefix, metric_id, which)
}

metric_plotOutput <- function(prefix, metric_id, which, height = 600, spinner_type = 6) {
  withSpinner(
    plotOutput(metric_plot_id(prefix, metric_id, which), height = height),
    color = getOption("spinner.color", default = "#0D576E"),
    type = spinner_type
  )
}

metric_plotlyOutput <- function(
    prefix,
    metric_id,
    which,
    height = 600,
    spinner_type = 6
) {
  
  withSpinner(
    plotly::plotlyOutput(
      metric_plot_id(prefix, metric_id, which),
      height = height
    ),
    color = getOption(
      "spinner.color",
      default = "#0D576E"
    ),
    type = spinner_type
  )
}

metric_leafletOutput <- function(prefix, metric_id, which = "map", height = 500, spinner_type = 6) {
  div(
    class = "map-full-wrapper",
    withSpinner(
      leafletOutput(metric_map_id(prefix, metric_id, which), height = height),
      color = getOption("spinner.color", default = "#0D576E"),
      type = spinner_type
    )
  )
}

metric_plot_type_input_id <- function(prefix, metric_id) {
  paste0(prefix, "_", metric_id, "_plot_type")
}

metric_year_input_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_", which, "_year_", metric_id)
}

metric_species_input_id <- function(prefix, metric_id) {
  paste0(prefix, "_", metric_id)
}

# ID for switch controlling year facets
metric_species_length_facet_id <- function(prefix, metric_id) {
  paste0(
    prefix,
    "_",
    metric_id,
    "_length_facet_year"
  )
}

# ID for dynamically sized length plot UI
metric_species_length_ui_id <- function(prefix, metric_id) {
  paste0(
    prefix,
    "_",
    metric_id,
    "_length_ui"
  )
}

metric_tab_body_ui <- function(metric_id, prefix = "bioregion", year_choices = NULL, species_choices = NULL) {
  
  data_id <- metric_id
  
  n_years <- length(
    unique(
      stats::na.omit(
        year_choices
      )
    )
  )
  
  facet_rows <- max(
    1,
    ceiling(n_years / 3)
  )
  
  diagnostic_height <- 450 * facet_rows
  
  # # Special layout for species
  # if (metric_id == "species") {
  #   return(
  #     tagList(
  #       
  #       selectInput(
  #         inputId = metric_species_input_id(prefix, data_id),
  #         label   = "Choose a species",
  #         choices = species_choices,
  #         width = "100%",
  #         selected = species_choices[1]
  #       ),
  #       
  #       layout_columns(
  #         col_widths = c(6, 6),
  #         
  #         card(
  #           full_screen = TRUE,
  #           card_header("Temporal"),
  #           metric_plotOutput(
  #             prefix = prefix,
  #             metric_id = data_id,
  #             which = "year",
  #             height = 600
  #           )
  #         ),
  #         
  #         card(
  #           full_screen = TRUE,
  #           card_header("Spatial"),
  #           metric_leafletOutput(
  #             prefix = prefix,
  #             metric_id = data_id,
  #             which = "map",
  #             height = 500
  #           )
  #         )
  #       ),
  #       
  #       card(
  #         full_screen = TRUE,
  #         card_header("Length distribution"),
  #         
  #         metric_plotOutput(
  #           prefix = prefix,
  #           metric_id = data_id,
  #           which = "length",
  #           height = 500
  #         )
  #       )
  #     )
  #   )
  # }
  
  # Special layout for species
  if (metric_id == "species") {
    
    return(
      tagList(
        
        # -------------------------------------------------------
        # Species selector
        # -------------------------------------------------------
        
        selectInput(
          inputId = metric_species_input_id(
            prefix,
            data_id
          ),
          label = "Choose a species",
          choices = species_choices,
          width = "100%",
          selected = species_choices[1]
        ),
        
        
        # -------------------------------------------------------
        # Existing temporal plot + spatial map
        # -------------------------------------------------------
        
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            full_screen = TRUE,
            card_header("Temporal"),
            
            metric_plotOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "year",
              height = 600
            )
          ),
          
          card(
            full_screen = TRUE,
            card_header("Spatial"),
            
            metric_leafletOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "map",
              height = 500
            )
          )
        ),
        
        
        # -------------------------------------------------------
        # Length distribution
        # -------------------------------------------------------
        
        card(
          full_screen = TRUE,
          
          card_header(
            "Length distribution"
          ),
          
          card_body(
            
            # Switch:
            # FALSE = all years combined
            # TRUE  = facet by year
            bslib::input_switch(
              id = metric_species_length_facet_id(
                prefix,
                data_id
              ),
              label = "Facet by year",
              value = FALSE
            ),
            
            br(),
            
            # Plot height is generated reactively
            # depending on whether facets are turned on
            uiOutput(
              metric_species_length_ui_id(
                prefix,
                data_id
              )
            )
          )
        )
      )
    )
  }
  
  # Default layout for all other metrics
  
  tagList(
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        full_screen = TRUE,
        card_header("Temporal"),
        metric_plotOutput(
          prefix = prefix,
          metric_id = data_id,
          which = "year",
          height = 600
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Spatial"),
        metric_leafletOutput(
          prefix = prefix,
          metric_id = data_id,
          which = "map",
          height = 500
        )
      )
    ),
    
    if (metric_id == "total_abundance") {
      card(
        full_screen = TRUE,
        card_header("Compare most abundant species by year"),
        
        layout_columns(
          col_widths = c(6, 6),
          
          div(
            selectInput(
              inputId = metric_year_input_id(prefix, data_id, "left"),
              label   = "Choose a year",
              choices = year_choices,
              width = "100%",
              selected = if (!is.null(year_choices) && length(year_choices) > 0) min(year_choices, na.rm = TRUE) else NULL
            ),
            metric_plotOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "left_year_status",
              height = 500
            )
          ),
          
          div(
            selectInput(
              inputId = metric_year_input_id(prefix, data_id, "right"),
              label   = "Choose a year",
              choices = year_choices,
              width = "100%",
              selected = if (!is.null(year_choices) && length(year_choices) > 0) max(year_choices, na.rm = TRUE) else NULL
            ),
            metric_plotOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "right_year_status",
              height = 500
            )
          )
        )
      )
    },
    
    if (metric_id == "cti") {
      
      card(
        full_screen = TRUE,
        card_header("Diagnostic plots"),
        
        layout_columns(
          col_widths = c(12),
          
          div(
            metric_plotOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "diagnostic",
              height = diagnostic_height
            )
          )
        )
      )
      
    },
    
    if (metric_id == "species_richness") {
      
      card(
        full_screen = TRUE,
        card_header("Species accumulation"),
        
        layout_columns(
          col_widths = c(12),
          
          div(
            metric_plotlyOutput(
              prefix = prefix,
              metric_id = data_id,
              which = "diagnostic",
              height = 700
            )
          )
        )
      )
      
    },
    
    if (metric_id %in% c("a20", "b20", "a30", "b30", "alt", "blt")) {
      
      card(
        full_screen = TRUE,
        card_header("Top 10 species by year"),
        
        metric_plotOutput(
          prefix = prefix,
          metric_id = data_id,
          which = "diagnostic",
          height = diagnostic_height
        )
      )
    }
  )
}

# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {
  
  # Overview Value Boxes ----
  stats <- nsw_bruv_data$overview_stats[1, ]
  
  output$num_bruvs <- renderText(comma(stats$num_bruvs))
  output$num_fish <- renderText(comma(stats$num_fish))
  output$num_lengths <- renderText(comma(stats$num_lengths))
  
  output$years_included <- renderText({
    paste0(stats$min_year, " - ", stats$max_year)
  })
  
  output$depths_surveyed <- renderText({
    paste0(stats$min_depth, " - ", stats$max_depth, " m")
  })
  
  output$average_depth <- renderText({
    paste0(round(stats$average_depth), " m")
  })
  
  # Overview Map ----
  output$map <- renderLeaflet({
    
    method_cols <- c("BRUVs" = "#004DA7", "UVC" = "#C600FF")
    
    pts <- (nsw_bruv_data$bruv_metadata) %>%
      dplyr::mutate(method = "BRUVS")
    
    m <- base_map(current_zoom = 6) |>
      # define panes with explicit stacking
      # addMapPane("points",    zIndex = 411) |>
      addMapPane("points", zIndex = 430) %>%
      # addMapPane("highlight", zIndex = 415) %>%
      
      # leafgl::addGlPoints(
      #   data = pts,
      #   # fillColor = method_cols[pts$method],
      #   weight = 1,
      #   # popup = pts$popup,
      #   group = "Sampling locations",
      #   pane  = "points"
      # ) %>%
      
      addCircleMarkers(
        data = pts,
        radius = 3,
        fillOpacity = 1,
        color = "#063F5C",
        weight = 1,
        opacity = 1,
        group = "Sampling locations",
        options = pathOptions(pane = "points"),
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 40,      # Smaller cluster groups
          showCoverageOnHover = TRUE,
          disableClusteringAtZoom = 8
        )
      ) %>%
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Satellite"
        ),
        overlayGroups = c("Sampling locations",
                          "NSW Marine Parks",
                          "Commonwealth Marine Parks"
                          ),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) #%>%
    
    # hideGroup("Commonwealth Marine Parks") 
    
    
    m
  })
  
  
  # ===== EXPLORE BIOREGION =====
  
  # Populate bioregion dropdown ----
  observe({
    updateSelectizeInput(
      session, "bioregion",
      choices = sort(unique(nsw_bruv_data$bioregion_stats$bioregion)),
      selected = unique(nsw_bruv_data$bioregion_stats$bioregion)[1],
      server = TRUE
    )
  })
  
  # Bioregion map ----
  bioregion_deployments <- reactive({
    req(input$bioregion)
    
    deployments <- nsw_bruv_data$bruv_metadata %>%
      dplyr::filter(bioregion %in% input$bioregion) %>%
      dplyr::mutate(method = "BRUVs")
    
  })
  
  bio_min_lat <- reactive({ min(bioregion_deployments()$latitude_dd,  na.rm = TRUE) })
  bio_min_lon <- reactive({ min(bioregion_deployments()$longitude_dd, na.rm = TRUE) })
  bio_max_lat <- reactive({ max(bioregion_deployments()$latitude_dd,  na.rm = TRUE) })
  bio_max_lon <- reactive({ max(bioregion_deployments()$longitude_dd, na.rm = TRUE) })
  
  output$bioregion_survey_effort <- renderLeaflet({
    
    req(input$bioregion)
    
    method_cols <- c("BRUVs" = "#063F5C", "UVC" = "#C600FF")
    
    pts <- bioregion_deployments()
    
    m <- base_map(current_zoom = 6) |>
      
      fitBounds(bio_min_lon(), bio_min_lat(), bio_max_lon(), bio_max_lat()) %>%
      
      # define panes with explicit stacking
      # addMapPane("points",    zIndex = 411) |>
      # addMapPane("highlight", zIndex = 415) %>%
      addMapPane("points", zIndex = 430) %>%
      
      # leafgl::addGlPoints(
      #   data = pts,
      #   # fillColor = method_cols[pts$method],
      #   weight = 1,
      #   # popup = pts$popup,
      #   group = "Sampling locations"#,
      #   # pane  = "points"
      # ) %>%
      
      addCircleMarkers(
        data = pts,
        radius = 3,
        fillOpacity = 1,
        color = "#063F5C",
        weight = 1,
        opacity = 1,
        group = "Sampling locations",
        options = pathOptions(pane = "points"),
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 40,      # Smaller cluster groups
          showCoverageOnHover = TRUE,
          disableClusteringAtZoom = 11
        )
      ) %>%
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Satellite"
        ),
        overlayGroups = c("Sampling locations",
                          "NSW Marine Parks", 
                          "Commonwealth Marine Parks"
                          ),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
    
    hideGroup("Commonwealth Marine Parks")
    
    
    m
  })
  
  # Bioregion Value Boxes ----
  
  bioregion_stats <- reactive({
    req(input$bioregion)
    
    nsw_bruv_data$bioregion_stats %>%
      dplyr::filter(bioregion %in% input$bioregion)
    
  })
  
  
  output$bioregion_num_bruvs <- renderText(comma(bioregion_stats()$num_bruvs))
  output$bioregion_num_fish <- renderText(comma(bioregion_stats()$num_fish))
  output$bioregion_num_lengths <- renderText(comma(bioregion_stats()$num_lengths))
  
  output$bioregion_years_included <- renderText({
    paste0(bioregion_stats()$min_year, " - ", bioregion_stats()$max_year)
  })
  
  output$bioregion_depths_surveyed <- renderText({
    paste0(bioregion_stats()$min_depth, " - ", bioregion_stats()$max_depth, " m")
  })
  
  output$bioregion_average_depth <- renderText({
    paste0(round(bioregion_stats()$average_depth), " m")
  })
  
  # TODO make this function be able to work on bioregion or marine_park
  make_top10_plot <- function(bioregion_name,
                              title_lab = "Common species",
                              number_species,
                              include_status = FALSE
  ) {
    
    # ---- Data prep ----
    df_raw <- nsw_bruv_data$top_species |>
      dplyr::filter(group == "bioregion") %>%
      dplyr::filter(bioregion == bioregion_name)
    
    # Top N species within the focal period
    top_species <- df_raw %>% 
      dplyr::filter(by_status == FALSE) |>
      dplyr::slice_max(order_by = average_abundance,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    if(include_status %in% TRUE){
      plot_df <- df_raw %>% dplyr::filter(by_status == TRUE) 
    } else {
      plot_df <- df_raw %>% dplyr::filter(by_status == FALSE)
    }
    
    # Extract sci/common and build markdown label
    plot_df <- plot_df %>%
      dplyr::filter(display_name %in% top_species)
    
    # Species order: smallest at bottom, biggest at top
    # I want to order by overall abundance
    overall_order_species <- df_raw %>%
      dplyr::filter(by_status == FALSE) |>
      dplyr::filter(display_name %in% top_species)
    
    species_order <- overall_order_species |>
      dplyr::arrange(average_abundance) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    base_plot <- ggplot(plot_df, aes(x = average_abundance, y = label, fill = status)) +
      labs(
        x     = "Average abundance per BRUV",
        y     = NULL,
        title = title_lab,
        fill  = NULL
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )
    
    if(include_status %in% FALSE){
      
      final_plot <- base_plot +
        geom_col(fill = "#063F5C") + 
        geom_errorbarh(
          aes(
            xmin = average_abundance - se,
            xmax = average_abundance + se
          ),
          height   = 0.3
        )  +
        guides(fill = "none")
    } else {
      
      dodge <- position_dodge(width = 0.75)
      
      final_plot <- base_plot +
        geom_col(position = dodge) + 
        geom_errorbarh(
          aes(
            xmin = average_abundance - se,
            xmax = average_abundance + se
          ),
          position = dodge,
          height   = 0.3
        )  +
        scale_fill_manual(
          values = c(
            "Fished"  = "#A9173A",
            "No-Take" = "#67C7BB"
          ))
    }
    
    final_plot
  }
  
  # Bioregion top species plot ----
  output$bioregion_top <- renderPlot({
    
    req(input$bioregion)
    
    make_top10_plot(
      title_lab      = "", #"Most common species",
      bioregion_name = input$bioregion,
      number_species = input$bioregion_number_species
    )
  })
  
  output$bioregion_top_status <- renderPlot({
    
    req(input$bioregion)
    
    make_top10_plot(
      title_lab      = "", #"Most common species",
      bioregion_name = input$bioregion,
      number_species = input$bioregion_number_species,
      include_status = TRUE
      
    )
  })
  
  # Build a tabbed card with one tab per metric
  # output$bioregion_tabset <- renderUI({
  #   req(input$bioregion)
  #   
  #   bslib::navset_card_tab(
  #     !!!lapply(names(metric_defs), function(id) {
  #       bslib::nav(
  #         title = metric_defs[[id]],
  #         metric_tab_body_ui(id, prefix = "bioregion")
  #       )
  #     })
  #   )
  # })
  output$bioregion_tabset <- renderUI({
    req(input$bioregion)
    
    year_choices <- nsw_bruv_data$top_50_most_abundant_species_bioregion_status_year %>%
      dplyr::filter(bioregion == input$bioregion) %>%
      dplyr::pull(year) %>%
      unique() %>%
      sort()
    
    species_choices <- nsw_bruv_data$top_species %>%
      dplyr::filter(group == "bioregion") %>%
      dplyr::filter(by_status == FALSE) %>%
      dplyr::filter(bioregion == input$bioregion) %>%
      arrange(dplyr::desc(average_abundance)) %>%
      dplyr::pull(display_name) %>%
      unique()
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(
            metric_id = id,
            prefix = "bioregion",
            year_choices = year_choices,
            species_choices = species_choices
          )
        )
      })
    )
  })

  # Generic metric plots ----
  
  get_metric_label <- function(metric_id) {
    if (exists("metric_y_lab", inherits = TRUE) &&
        metric_id %in% names(metric_y_lab)) {
      metric_y_lab[[metric_id]]
    } else if (metric_id %in% names(metric_defs)) {
      metric_defs[[metric_id]]
    } else {
      metric_id
    }
  }
  
  bioregion_metric_data <- function(metric_id) {
    req(input$bioregion)
    
    nsw_bruv_data$metrics %>%
      dplyr::filter(bioregion %in% input$bioregion) %>%
      dplyr::filter(metric == metric_id)
  }
  # 
  # make_metric_boxplot <- function(metric_id,
  #                                 x_col = "bioregion",
  #                                 plot_title = NULL,
  #                                 plot_subtitle = NULL) {
  #   
  #   df <- bioregion_metric_data(metric_id)
  #   
  #   validate(
  #     need(nrow(df) > 0, paste("No data available for", metric_id)),
  #     need(x_col %in% names(df), paste("Column", x_col, "not found in metrics data"))
  #   )
  #   
  #   mean_se <- df %>%
  #     dplyr::group_by(.data[[x_col]]) %>%
  #     dplyr::summarise(
  #       n    = sum(!is.na(value)),
  #       mean = mean(value, na.rm = TRUE),
  #       se   = dplyr::if_else(
  #         n > 1,
  #         stats::sd(value, na.rm = TRUE) / sqrt(n),
  #         0
  #       ),
  #       .groups = "drop"
  #     )
  #   
  #   ggplot(df, aes(x = .data[[x_col]], y = value, fill = status)) +
  #     
  #     # geom_boxplot(
  #     #   width = 0.6,
  #     #   outlier.shape = NA,
  #     #   alpha = 0.85,
  #     #   colour = "black"
  #     # ) +
  #     
  #     # geom_jitter(
  #     #   width = 0.15,
  #     #   height = 0,
  #     #   alpha = 0.35,
  #     #   size = 1.2
  #     # ) +
  #     
  #     geom_pointrange(
  #       data = mean_se,
  #       aes(
  #         x    = .data[[x_col]],
  #         y    = mean,
  #         ymin = mean - se,
  #         ymax = mean + se
  #       ),
  #       inherit.aes = FALSE,
  #       colour = "black",
  #       linewidth = 0.6
  #     ) +
  #     
  #     labs(
  #       x        = NULL,
  #       y        = get_metric_label(metric_id),
  #       title    = plot_title,
  #       subtitle = plot_subtitle
  #     ) +
  #     
  #     theme_minimal(base_size = 16) +
  #     theme(
  #       legend.position  = "none",
  #       panel.grid.minor = element_blank()
  #     )
  # }
  # 
  # make_metric_main_plot <- function(metric_id) {
  #   make_metric_boxplot(
  #     metric_id     = metric_id,
  #     x_col         = "bioregion",
  #     plot_title    = metric_defs[[metric_id]],
  #     plot_subtitle = paste(input$bioregion, collapse = ", ")
  #   )
  # }
  # 
  make_metric_boxplot_year <- function(metric_id,
                                       x_col = "year",
                                       plot_title = NULL,
                                       plot_subtitle = NULL) {
    
    df <- bioregion_metric_data(metric_id)
    
    validate(
      need(nrow(df) > 0, paste("No data available for", metric_id)),
      need(x_col %in% names(df), paste("Column", x_col, "not found in metrics data"))
    )
    
    # Make year a real date
    df <- df %>%
      dplyr::mutate(
        year = as.Date(paste0(year, "-01", "-01"))
      )
    
    mean_se <- df %>%
      dplyr::group_by(.data[[x_col]], status) %>%
      dplyr::summarise(
        n    = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        se   = dplyr::if_else(
          n > 1,
          stats::sd(value, na.rm = TRUE) / sqrt(n),
          0
        ),
        .groups = "drop"
      ) %>%
      glimpse
    
    ggplot(df, aes(x = .data[[x_col]], y = value, fill = status, colour = status)) +
      geom_pointrange(
        data = mean_se,
        aes(
          x    = .data[[x_col]],
          y    = mean,
          ymin = mean - se,
          ymax = mean + se,
          fill = status,
          colour = status
        ),
        position = position_dodge(width = 3),
        inherit.aes = FALSE,
        # colour = "black",
        linewidth = 0.6
      ) +
      
      labs(
        x        = NULL,
        y        = get_metric_label(metric_id)
      ) +
      scale_x_date(
        date_labels = "%Y",
        date_breaks = "1 year",
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      
      theme_minimal(base_size = 16) +
      theme(
        # legend.position  = "none",
        panel.grid.minor = element_blank()
      ) +
      scale_colour_manual(
        values = c(
          "Fished"  = "#A9173A",
          "No-Take" = "#67C7BB"
        ))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  make_metric_year_plot <- function(metric_id) {
    make_metric_boxplot_year(
      metric_id     = metric_id,
      x_col         = "year",
      plot_title    = metric_defs[[metric_id]],
      plot_subtitle = paste(input$bioregion, collapse = ", ")
    )
  }
  
  make_metric_status_plot <- function(metric_id) {
    df <- bioregion_metric_data(metric_id)
    
    # Change or extend this list if your status/grouping column has another name.
    status_col <- intersect(
      c("status", "period", "protection_status", "zone", "mp_zone"),
      names(df)
    )[1]
    
    if (is.na(status_col)) {
      # Fallback so the UI's "_status" plot output is still created.
      make_metric_boxplot(
        metric_id     = metric_id,
        x_col         = "bioregion",
        plot_title    = paste(metric_defs[[metric_id]], "- status"),
        plot_subtitle = paste(input$bioregion, collapse = ", ")
      )
    } else {
      make_metric_boxplot(
        metric_id     = metric_id,
        x_col         = status_col,
        plot_title    = paste(metric_defs[[metric_id]], "by", status_col),
        plot_subtitle = paste(input$bioregion, collapse = ", ")
      )
    }
  }
  
  # Register one pair of renderPlot outputs for every metric in metric_defs.
  # This matches the IDs created by metric_tab_body_ui().
  for (id in names(metric_defs)) {
    local({
      metric_id <- id
      
      output[[metric_plot_id("bioregion", metric_id, "main")]] <- renderPlot({
        req(input$bioregion)
        make_metric_main_plot(metric_id)
      })
      
      output[[metric_plot_id("bioregion", metric_id, "year")]] <- renderPlot({
        req(input$bioregion)
        make_metric_year_plot(metric_id)
      })
      
      output[[metric_plot_id("bioregion", metric_id, "status")]] <- renderPlot({
        req(input$bioregion)
        make_metric_status_plot(metric_id)
      })
    })
  }
  
  
  bioregion_metric_map_data <- function(metric_id) {
    req(input$bioregion)
    
    df <- nsw_bruv_data$metrics %>%
      dplyr::filter(bioregion %in% input$bioregion) %>%
      dplyr::filter(metric == metric_id)
      
    
    # TODO include latitude and longitude in create metrics script
      df <- df %>%
        dplyr::left_join(
          nsw_bruv_data$bruv_metadata %>%
            dplyr::select(
              campaignid, sample, sample_url,
              latitude_dd,
              longitude_dd
            )
          )
  }
  
  # TODO update labels for the pop-ups
  make_metric_leaflet_map <- function(metric_id) {
    
    pts <- bioregion_metric_map_data(metric_id)
    
    validate(
      need(nrow(pts) > 0, paste("No mappable data available for", metric_id))
    )
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = pts$value,
      na.color = "#BDBDBD"
    )
    
    # Rescale point radius safely
    if (length(unique(stats::na.omit(pts$value))) <= 1) {
      pts$radius <- 8
    } else {
      pts$radius <- scales::rescale(
        pts$value,
        to = c(4, 14),
        from = range(pts$value, na.rm = TRUE)
      )
    }
    
    pts <- pts %>%
      dplyr::mutate(
        popup_text = paste0(
          "<strong>", get_metric_label(metric_id), "</strong><br>",
          "Value: ", round(value, 2), "<br>",
          "Status: ", status, "<br>"#,
          # "Latitude: ", round(latitude_dd, 5), "<br>",
          # "Longitude: ", round(longitude_dd, 5)
        )
      )
    
    legend_title <- stringr::str_wrap(get_metric_label(metric_id), width = 15)
    legend_title <- gsub("\n", "<br>", legend_title)
    
    base_map(current_zoom = 6) %>%
      fitBounds(
        lng1 = min(pts$longitude_dd, na.rm = TRUE),
        lat1 = min(pts$latitude_dd,  na.rm = TRUE),
        lng2 = max(pts$longitude_dd, na.rm = TRUE),
        lat2 = max(pts$latitude_dd,  na.rm = TRUE)
      ) %>%
      addMapPane("metric_points", zIndex = 430) %>%
      addCircleMarkers(
        data = pts,
        lng = ~longitude_dd,
        lat = ~latitude_dd,
        radius = ~radius,
        fillColor = ~pal(value),
        fillOpacity = 0.8,
        color = "#FFFFFF",
        weight = 1,
        opacity = 1,
        popup = ~popup_text,
        group = "Metric values",
        options = pathOptions(pane = "metric_points")
      ) %>%
      addLegend(
        pal = pal,
        values = pts$value,
        title = htmltools::HTML(legend_title),
        position = "topleft",
        opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Satellite"
        ),
        overlayGroups = c(
          "Metric values",
          "NSW Marine Parks",
          "Commonwealth Marine Parks"
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  }
  
  make_metric_blank_plot <- function(metric_id) {
    graphics::plot.new()
    graphics::text(
      x = 0.5,
      y = 0.5,
      labels = "Plot coming soon",
      cex = 1.5,
      col = "#063F5C"
    )
  }
  
  # Register outputs for every metric in metric_defs.
  # This matches the IDs created by metric_tab_body_ui().
  for (id in names(metric_defs)) {
    local({
      metric_id <- id
      
      # output[[metric_plot_id("bioregion", metric_id, "year")]] <- renderPlot({
      #   req(input$bioregion)
      #   make_metric_year_plot(metric_id)
      # })
      # 
      output[[metric_map_id("bioregion", metric_id, "map")]] <- renderLeaflet({
        req(input$bioregion)
        make_metric_leaflet_map(metric_id)
      })
      # 
      # output[[metric_plot_id("bioregion", metric_id, "blank")]] <- renderPlot({
      #   req(input$bioregion)
      #   make_metric_blank_plot(metric_id)
      # })
    })
  }
  
  
  # TOTAL ABUNDANCE DIAGNOSTIC PLOTS ----
  make_top_abundance_bioregion_status_year_plot <- function(
    bioregion_name,
    selected_year,
    comparison_year,
    number_species = 10,
    title_lab = NULL
  ) {
    
    req(bioregion_name, selected_year)
    
    # df_raw <- nsw_bruv_data$top_50_most_abundant_species_bioregion_status_year %>%
    #   dplyr::filter(bioregion == bioregion_name) %>%
    #   dplyr::filter(as.character(year) == as.character(selected_year))
    # 
    # validate(
    #   need(nrow(df_raw) > 0, paste("No species data available for", bioregion_name, "in", selected_year))
    # )
    # 
    # top_species <- df_raw %>%
    #   dplyr::group_by(display_name) %>%
    #   dplyr::summarise(
    #     overall_average_abundance = sum(average_abundance, na.rm = TRUE),
    #     .groups = "drop"
    #   ) %>%
    #   dplyr::slice_max(
    #     order_by = overall_average_abundance,
    #     n = number_species,
    #     with_ties = FALSE
    #   ) %>%
    #   dplyr::pull(display_name)
    # 
    # plot_df <- df_raw %>%
    #   dplyr::filter(display_name %in% top_species)
    
    # Get data for the TWO selected years
    df_compare <- nsw_bruv_data$top_50_most_abundant_species_bioregion_status_year %>%
      dplyr::filter(
        bioregion == bioregion_name,
        as.character(year) %in% as.character(c(selected_year, comparison_year))
      )
    
    # Top N species separately for each of the two selected years
    top_species_by_year <- df_compare %>%
      dplyr::group_by(year, display_name) %>%
      dplyr::summarise(
        overall_average_abundance = sum(average_abundance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::group_by(year) %>%
      dplyr::slice_max(
        order_by = overall_average_abundance,
        n = number_species,
        with_ties = FALSE
      ) %>%
      dplyr::ungroup()
    
    # Species in the selected year's top N
    selected_top_species <- top_species_by_year %>%
      dplyr::filter(as.character(year) == as.character(selected_year)) %>%
      dplyr::pull(display_name)
    
    # Species in the OTHER selected year's top N
    comparison_top_species <- top_species_by_year %>%
      dplyr::filter(as.character(year) == as.character(comparison_year)) %>%
      dplyr::pull(display_name)
    
    # Species unique to THIS selected year
    unique_species <- setdiff(
      selected_top_species,
      comparison_top_species
    )
    
    # Data to actually plot
    df_raw <- df_compare %>%
      dplyr::filter(
        as.character(year) == as.character(selected_year)
      )
    
    validate(
      need(
        nrow(df_raw) > 0,
        paste(
          "No species data available for",
          bioregion_name,
          "in",
          selected_year
        )
      )
    )
    
    plot_df <- df_raw %>%
      dplyr::filter(display_name %in% selected_top_species)
    
    # species_order <- plot_df %>%
    #   dplyr::group_by(label) %>%
    #   dplyr::summarise(
    #     overall_average_abundance = sum(average_abundance, na.rm = TRUE),
    #     .groups = "drop"
    #   ) %>%
    #   dplyr::arrange(overall_average_abundance) %>%
    #   dplyr::pull(label)
    # 
    # plot_df <- plot_df %>%
    #   dplyr::mutate(
    #     label = factor(label, levels = species_order)
    #   )
    
    species_order <- plot_df %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(
        overall_average_abundance = sum(average_abundance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(overall_average_abundance) %>%
      dplyr::pull(label)
    
    # Bold species that are unique between the two chosen years
    plot_df <- plot_df %>%
      dplyr::mutate(
        plot_label = dplyr::if_else(
          display_name %in% unique_species,
          paste0("<b>", label, "</b>"),
          label
        )
      )
    
    # Preserve the abundance ordering after changing the labels
    species_order_plot <- plot_df %>%
      dplyr::distinct(label, plot_label) %>%
      dplyr::mutate(
        order = match(label, species_order)
      ) %>%
      dplyr::arrange(order) %>%
      dplyr::pull(plot_label)
    
    plot_df <- plot_df %>%
      dplyr::mutate(
        plot_label = factor(
          plot_label,
          levels = species_order_plot
        )
      )
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(plot_df, aes(x = average_abundance, 
                        y = plot_label, 
                        fill = status)) +
      geom_col(position = dodge) +
      geom_errorbarh(
        aes(
          xmin = average_abundance - se,
          xmax = average_abundance + se
        ),
        position = dodge,
        height = 0.3
      ) +
      scale_fill_manual(
        values = c(
          "Fished"  = "#A9173A",
          "No-Take" = "#67C7BB"
        )
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Average abundance per BRUV",
        y = NULL,
        title = title_lab,
        fill = NULL
      ) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y = ggtext::element_markdown(size = 12)
      )
  }
  
  # observe({
  #   req(input$bioregion)
  #   
  #   df <- nsw_bruv_data$top_50_most_abundant_species_bioregion_status_year %>%
  #     dplyr::filter(bioregion == input$bioregion)
  #   
  #   years <- sort(unique(df$year))
  #   
  #   req(length(years) > 0)
  #   
  #   updateSelectInput(
  #     session,
  #     inputId = metric_year_input_id("bioregion", "total_abundance", "left"),
  #     choices = years,
  #     selected = min(years, na.rm = TRUE)
  #   )
  #   
  #   updateSelectInput(
  #     session,
  #     inputId = metric_year_input_id("bioregion", "total_abundance", "right"),
  #     choices = years,
  #     selected = max(years, na.rm = TRUE)
  #   )
  # })
  
  output[[metric_plot_id("bioregion", "total_abundance", "left_year_status")]] <- renderPlot({
    req(input$bioregion)
    req(input[[metric_year_input_id("bioregion", "total_abundance", "left")]])
    
    # make_top_abundance_bioregion_status_year_plot(
    #   bioregion_name = input$bioregion,
    #   selected_year  = input[[metric_year_input_id("bioregion", "total_abundance", "left")]],
    #   number_species = input$bioregion_number_species,
    #   title_lab      = input[[metric_year_input_id("bioregion", "total_abundance", "left")]]
    # )
    
    make_top_abundance_bioregion_status_year_plot(
      bioregion_name = input$bioregion,
      
      selected_year =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "left"
          )
        ]],
      
      comparison_year =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "right"
          )
        ]],
      
      number_species = input$bioregion_number_species,
      
      title_lab =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "left"
          )
        ]]
    )
    
  })
  
  output[[metric_plot_id("bioregion", "total_abundance", "right_year_status")]] <- renderPlot({
    req(input$bioregion)
    req(input[[metric_year_input_id("bioregion", "total_abundance", "right")]])
    
    # make_top_abundance_bioregion_status_year_plot(
    #   bioregion_name = input$bioregion,
    #   selected_year  = input[[metric_year_input_id("bioregion", "total_abundance", "right")]],
    #   number_species = input$bioregion_number_species,
    #   title_lab      = input[[metric_year_input_id("bioregion", "total_abundance", "right")]]
    # )
    
    make_top_abundance_bioregion_status_year_plot(
      bioregion_name = input$bioregion,
      
      selected_year =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "right"
          )
        ]],
      
      comparison_year =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "left"
          )
        ]],
      
      number_species = input$bioregion_number_species,
      
      title_lab =
        input[[
          metric_year_input_id(
            "bioregion",
            "total_abundance",
            "right"
          )
        ]]
    )
    
  })
  
  #   # CTI DIAGNOSTIC PLOTS ----
  make_top_cti_year_plot <- function(
    bioregion_name,
    title_lab = NULL
  ) {
    
    req(bioregion_name)
    message(bioregion_name)
    message("CTI plots")
    
    df_raw <- nsw_bruv_data$cti_top_10 %>%
      dplyr::filter(bioregion == bioregion_name) %>%
      glimpse
    
    # Species that only occur in the top 10 for one year
    message("view unique species CTI")
    
    unique_species <- df_raw %>%
      dplyr::distinct(year, scientific) %>%
      dplyr::count(scientific, name = "n_years") %>%
      dplyr::filter(n_years == 1) %>%
      dplyr::pull(scientific) %>%
      glimpse()
    
    # Make bold if it is unique
    df_raw <- df_raw %>%
      dplyr::mutate(
        
        label = dplyr::case_when(
          
          scientific %in% unique_species &
            !is.na(common) ~
            paste0(
              "***", sci, "***",
              "<br>(",
              common,
              ")"
            ),
          
          scientific %in% unique_species ~
            paste0("***", sci, "***"),
          
          !is.na(common) ~
            paste0(
              "*", sci, "*",
              "<br>(",
              common,
              ")"
            ),
          
          TRUE ~
            paste0("*", sci, "*")
        )
      )
    
    max_maxn <- max(df_raw$maxn) + max(df_raw$se)
    
    # choose the centering statistic
    mid_niche <- median(df_raw$rls_thermal_niche, na.rm = TRUE)
    
    # global limits across both facets/years
    niche_limits <- range(df_raw$rls_thermal_niche, na.rm = TRUE)
    
    ggplot(
      df_raw,
      aes(
        x = reorder_within(label, rls_thermal_niche, year),
        y = maxn,
        fill = rls_thermal_niche
      )
    ) +
      geom_col(colour = "black", linewidth = 0.25) +
      geom_errorbar(
        aes(
          ymin = pmax(maxn - se, 0),
          ymax = maxn + se
        ),
        width = 0.2
      ) +
      # geom_text(aes(y = 23, label = niche_lab), hjust = 0, size = 3) +
          geom_text(
            aes(
              y = max_maxn + 1,
              label = paste0(niche_lab, "\u00B0C")
            ),
            hjust = 0,
            size = 3.5
          ) +
      coord_flip(clip = "off") +
      facet_wrap(~year, scales = "free_y") +
      scale_x_reordered() +
      scale_y_continuous(
        trans = log1p10_trans,
        expand = expansion(mult = c(0, 0.15)),
        breaks = c(0, 5, 10, 20, 30),
        labels = scales::label_number()
      ) +
      # centre GREY at the mean thermal niche
      scale_fill_gradientn(
        colours = c(
          "#2166ac",
          "#67a9cf",
          "#d1e5f0",
          "#fddbc7",
          "#ef8a62",
          "#b2182b"
          
        ),
        values  = scales::rescale(c(niche_limits[1],
                                    mid_niche,
                                    niche_limits[2])),
        limits = niche_limits,
        na.value = "grey80"
      ) +
      guides(fill = "none") +
      labs(
        x = "Species",
        y = expression(Log[10]~(Average~abundance~+~1))
      ) +
      # theme_bw() +
      theme_classic() +
      theme(
        
        # Species labels
        axis.text.y = ggtext::element_markdown(
          size = 12
        ),
        
        # Abundance-axis numbers
        axis.text.x = element_text(
          size = 13
        ),
        
        # Year facet headings
        strip.text = element_text(
          size = 14,
          face = "bold"
        )
      )
  }
  
  output[[metric_plot_id("bioregion", "cti", "diagnostic")]] <- renderPlot({
    req(input$bioregion)
    
    make_top_cti_year_plot(
      bioregion_name = input$bioregion
    )
  })
  
  # SPECIES RICHNESS DIAGNOSTIC PLOTS ----
  # 
  # make_species_accumulation_plot <- function(
  #   bioregion_name,
  #   title_lab = NULL
  # ) {
  #   
  #   req(bioregion_name)
  #   
  #   df <- nsw_bruv_data$species_accumulation %>%
  #     dplyr::filter(
  #       bioregion == bioregion_name
  #     )
  #   
  #   validate(
  #     need(
  #       nrow(df) > 0,
  #       paste(
  #         "No species accumulation data available for",
  #         bioregion_name
  #       )
  #     )
  #   )
  #   
  #   # Put years in chronological order
  #   year_levels <- sort(
  #     unique(as.character(df$year))
  #   )
  #   
  #   df <- df %>%
  #     dplyr::mutate(
  #       year = factor(
  #         as.character(year),
  #         levels = year_levels
  #       )
  #     )
  #   
  #   # Create enough line types for however many years occur
  #   line_types <- rep(
  #     c(
  #       "solid",
  #       "22",
  #       "42",
  #       "13",
  #       "44",
  #       "F1"
  #     ),
  #     length.out = length(year_levels)
  #   )
  #   
  #   names(line_types) <- year_levels
  #   
  #   
  #   ggplot(
  #     df,
  #     aes(
  #       x = deployments,
  #       y = richness,
  #       colour = status,
  #       fill = status,
  #       linetype = year,
  #       group = interaction(status, year)
  #     )
  #   ) +
  #     
  #     geom_ribbon(
  #       aes(
  #         ymin = lower,
  #         ymax = upper
  #       ),
  #       alpha = 0.15,
  #       colour = NA
  #     ) +
  #     
  #     geom_line(
  #       linewidth = 1.2
  #     ) +
  #     
  #     scale_colour_manual(
  #       name = "Status",
  #       values = c(
  #         "Fished"  = "#A9173A",
  #         "No-Take" = "#67C7BB"
  #       )
  #     ) +
  #     
  #     scale_fill_manual(
  #       name = "Status",
  #       values = c(
  #         "Fished"  = "#A9173A",
  #         "No-Take" = "#67C7BB"
  #       )
  #     ) +
  #     
  #     scale_linetype_manual(
  #       name = "Year",
  #       values = line_types
  #     ) +
  #     
  #     labs(
  #       x = "Number of BRUV deployments",
  #       y = "Species richness",
  #       title = title_lab
  #     ) +
  #     
  #     theme_minimal(
  #       base_size = 16
  #     ) +
  #     
  #     theme(
  #       panel.grid.minor = element_blank(),
  #       legend.position = "right"
  #     )
  # }
  
  
  
  # # Output -----
  # output[[
  #   metric_plot_id(
  #     "bioregion",
  #     "species_richness",
  #     "diagnostic"
  #   )
  # ]] <- renderPlot({
  #   
  #   req(input$bioregion)
  #   
  #   make_species_accumulation_plot(
  #     bioregion_name = input$bioregion
  #   )
  #   
  # })
  
  # SPECIES RICHNESS DIAGNOSTIC PLOTS ----
  
  make_species_accumulation_plot <- function(
    bioregion_name,
    title_lab = NULL
  ) {
    
    req(bioregion_name)
    
    
    # -----------------------------
    # Filter data
    # -----------------------------
    
    df <- nsw_bruv_data$species_accumulation %>%
      dplyr::filter(
        bioregion == bioregion_name
      ) %>%
      dplyr::mutate(
        year = as.character(year),
        
        # Protect against NA SD values
        sd = tidyr::replace_na(sd, 0),
        
        lower = pmax(
          richness - sd,
          0
        ),
        
        upper = richness + sd
      )
    
    
    validate(
      need(
        nrow(df) > 0,
        paste(
          "No species accumulation data available for",
          bioregion_name
        )
      )
    )
    
    
    # -----------------------------
    # Status colours
    # -----------------------------
    
    status_colours <- c(
      "Fished"  = "#A9173A",
      "No-Take" = "#67C7BB"
    )
    
    
    # Give any unexpected status a neutral colour
    unknown_status <- setdiff(
      unique(df$status),
      names(status_colours)
    )
    
    if (length(unknown_status) > 0) {
      
      status_colours <- c(
        status_colours,
        stats::setNames(
          rep(
            "#6C757D",
            length(unknown_status)
          ),
          unknown_status
        )
      )
    }
    
    
    # -----------------------------
    # Year line types
    # -----------------------------
    
    year_levels <- sort(
      unique(df$year)
    )
    
    
    plotly_dash_types <- c(
      "solid",
      "dash",
      "dot",
      "dashdot",
      "longdash",
      "longdashdot"
    )
    
    
    year_dashes <- stats::setNames(
      rep(
        plotly_dash_types,
        length.out = length(year_levels)
      ),
      year_levels
    )
    
    
    # -----------------------------
    # Helper: hex -> rgba
    #
    # Used to make transparent
    # uncertainty ribbons
    # -----------------------------
    
    hex_to_rgba <- function(
    hex_colour,
    alpha = 0.15
    ) {
      
      rgb <- grDevices::col2rgb(
        hex_colour
      )
      
      sprintf(
        "rgba(%s,%s,%s,%.2f)",
        rgb[1],
        rgb[2],
        rgb[3],
        alpha
      )
    }
    
    
    # -----------------------------
    # Order groups
    # -----------------------------
    
    status_order <- c(
      intersect(
        c("Fished", "No-Take"),
        unique(df$status)
      ),
      setdiff(
        sort(unique(df$status)),
        c("Fished", "No-Take")
      )
    )
    
    
    curve_keys <- df %>%
      dplyr::distinct(
        status,
        year
      ) %>%
      dplyr::mutate(
        status = factor(
          status,
          levels = status_order
        ),
        year = factor(
          year,
          levels = year_levels
        )
      ) %>%
      dplyr::arrange(
        status,
        year
      )
    
    
    # -----------------------------
    # Start Plotly figure
    # -----------------------------
    
    fig <- plotly::plot_ly()
    
    
    # -----------------------------
    # Add one ribbon + one line
    # for every status/year
    # -----------------------------
    
    for (i in seq_len(nrow(curve_keys))) {
      
      status_i <- as.character(
        curve_keys$status[i]
      )
      
      year_i <- as.character(
        curve_keys$year[i]
      )
      
      
      curve_df <- df %>%
        dplyr::filter(
          status == status_i,
          year == year_i
        ) %>%
        dplyr::arrange(
          deployments
        )
      
      
      # Unique ID used to link the line
      # and its uncertainty ribbon
      curve_id <- paste(
        status_i,
        year_i,
        sep = "__"
      )
      
      
      colour_i <- status_colours[[status_i]]
      dash_i <- year_dashes[[year_i]]
      
      
      legend_name <- paste0(
        year_i,
        " \u2014 ",
        status_i
      )
      
      
      # -------------------------
      # Hover text
      # -------------------------
      
      curve_df <- curve_df %>%
        dplyr::mutate(
          
          hover_text = paste0(
            "<b>", year_i, " \u2014 ", status_i, "</b>",
            "<br>",
            "BRUV deployments: ",
            deployments,
            "<br>",
            "Species richness: ",
            round(richness, 1),
            "<br>",
            "SD: ",
            round(sd, 1)
          )
        )
      
      
      # -------------------------
      # Uncertainty ribbon
      # -------------------------
      
      ribbon_x <- c(
        curve_df$deployments,
        rev(curve_df$deployments)
      )
      
      
      ribbon_y <- c(
        curve_df$upper,
        rev(curve_df$lower)
      )
      
      
      fig <- fig %>%
        plotly::add_trace(
          
          x = ribbon_x,
          y = ribbon_y,
          
          type = "scatter",
          mode = "lines",
          
          fill = "toself",
          
          fillcolor = hex_to_rgba(
            colour_i,
            alpha = 0.15
          ),
          
          line = list(
            color = "rgba(0,0,0,0)",
            width = 0
          ),
          
          hoverinfo = "skip",
          
          showlegend = FALSE,
          
          legendgroup = curve_id,
          
          meta = "ribbon"
        )
      
      
      # -------------------------
      # Accumulation line
      # -------------------------
      
      fig <- fig %>%
        plotly::add_trace(
          
          x = curve_df$deployments,
          y = curve_df$richness,
          
          type = "scatter",
          mode = "lines",
          
          name = legend_name,
          
          legendgroup = curve_id,
          
          line = list(
            color = colour_i,
            dash = dash_i,
            width = 3
          ),
          
          text = curve_df$hover_text,
          
          hovertemplate = paste0(
            "%{text}",
            "<extra></extra>"
          ),
          
          showlegend = TRUE,
          
          meta = "line"
        )
    }
    
    
    # -----------------------------
    # Plot layout
    # -----------------------------
    
    fig <- fig %>%
      plotly::layout(
        
        title = list(
          text = if (
            is.null(title_lab)
          ) {
            ""
          } else {
            title_lab
          },
          
          x = 0.02,
          xanchor = "left"
        ),
        
        
        xaxis = list(
          title = list(
            text = "Number of BRUV deployments"
          ),
          
          rangemode = "tozero",
          
          showgrid = TRUE,
          
          zeroline = FALSE
        ),
        
        
        yaxis = list(
          title = list(
            text = "Species richness"
          ),
          
          rangemode = "tozero",
          
          showgrid = TRUE,
          
          zeroline = FALSE
        ),
        
        
        legend = list(
          
          title = list(
            text = "<b>Year \u2014 Status</b>"
          ),
          
          orientation = "v",
          
          # Single click:
          # hide/show this curve
          itemclick = "toggle",
          
          # Double click:
          # isolate this curve
          itemdoubleclick = "toggleothers",
          
          # Makes ribbon + line respond together
          groupclick = "togglegroup"
        ),
        
        
        hovermode = "closest",
        
        
        hoverlabel = list(
          align = "left"
        ),
        
        
        font = list(
          family = "Barlow"
        ),
        
        
        margin = list(
          l = 80,
          r = 160,
          b = 70,
          t = 40
        )
      )
    
    
    # -----------------------------
    # Clean Plotly toolbar
    # -----------------------------
    
    fig <- fig %>%
      plotly::config(
        
        displaylogo = FALSE,
        
        responsive = TRUE,
        
        scrollZoom = FALSE,
        
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "toggleSpikelines"
        )
      )
    
    
    # -----------------------------
    # Hover highlighting
    #
    # Hover a line:
    #   - selected curve gets thicker
    #   - selected ribbon remains visible
    #   - all other curves dim
    #
    # Move away:
    #   - reset opacity and line width
    # -----------------------------
    
    fig <- htmlwidgets::onRender(
      fig,
      
      "
    function(el, x) {

      function focusCurve(group) {

        el.data.forEach(function(trace, i) {

          var sameGroup =
            trace.legendgroup === group;

          var newOpacity =
            sameGroup ? 1 : 0.12;


          Plotly.restyle(
            el,
            {
              opacity: newOpacity
            },
            [i]
          );


          if (trace.meta === 'line') {

            Plotly.restyle(
              el,
              {
                'line.width':
                  sameGroup ? 5 : 2
              },
              [i]
            );

          }

        });

      }


      function resetCurves() {

        el.data.forEach(function(trace, i) {

          Plotly.restyle(
            el,
            {
              opacity: 1
            },
            [i]
          );


          if (trace.meta === 'line') {

            Plotly.restyle(
              el,
              {
                'line.width': 3
              },
              [i]
            );

          }

        });

      }


      el.on(
        'plotly_hover',
        function(eventData) {

          if (
            !eventData.points ||
            !eventData.points.length
          ) {
            return;
          }


          var curveNumber =
            eventData.points[0].curveNumber;


          var trace =
            el.data[curveNumber];


          // Only trigger when hovering
          // an accumulation line, not
          // the uncertainty ribbon
          if (trace.meta !== 'line') {
            return;
          }


          focusCurve(
            trace.legendgroup
          );

        }
      );


      el.on(
        'plotly_unhover',
        function() {

          resetCurves();

        }
      );

    }
    "
    )
    
    
    fig
  }
  
  output[[
    metric_plot_id(
      "bioregion",
      "species_richness",
      "diagnostic"
    )
  ]] <- plotly::renderPlotly({
    
    req(input$bioregion)
    
    make_species_accumulation_plot(
      bioregion_name = input$bioregion
    )
    
  })
  
  # SPECIES SPEFICIC DIAGNOSTIC PLOTS ----
  make_species_year_plot <- function(
    bioregion_name,
    title_lab = NULL,
    selected_species
  ) {
    
    req(bioregion_name)
    req(selected_species)
    
    message(bioregion_name)
    message(paste("Species plots:", selected_species))
    
    df <- nsw_bruv_data$top_50_abundance %>%
      dplyr::filter(bioregion == bioregion_name) %>%
      dplyr::filter(display_name %in% selected_species) %>%
      dplyr::mutate(year = as.Date(paste0(year, "-01", "-01"))) #%>% # Make year a real date
      #glimpse
    
    mean_se <- df %>%
      dplyr::group_by(year, status) %>%
      dplyr::summarise(
        n    = sum(!is.na(count)),
        mean = mean(count, na.rm = TRUE),
        se   = dplyr::if_else(
          n > 1,
          stats::sd(count, na.rm = TRUE) / sqrt(n),
          0
        ),
        .groups = "drop"
      ) %>%
      glimpse
    
    ggplot(df, aes(x = year, y = value, fill = status, colour = status)) +
      geom_pointrange(
        data = mean_se,
        aes(
          x    = year,
          y    = mean,
          ymin = mean - se,
          ymax = mean + se,
          fill = status,
          colour = status
        ),
        position = position_dodge(width = 3),
        inherit.aes = FALSE,
        # colour = "black",
        linewidth = 0.6
      ) +
      
      labs(
        x        = NULL,
        y        = "Average MaxN"
      ) +
      scale_x_date(
        date_labels = "%Y",
        date_breaks = "1 year",
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      
      theme_minimal(base_size = 16) +
      theme(
        # legend.position  = "none",
        panel.grid.minor = element_blank()
      ) +
      scale_colour_manual(
        values = c(
          "Fished"  = "#A9173A",
          "No-Take" = "#67C7BB"
        ))+ 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  output[[metric_plot_id("bioregion", "species", "year")]] <- renderPlot({
    req(input$bioregion)
    
    make_species_year_plot(
      bioregion_name = input$bioregion,
      selected_species = input[[metric_species_input_id("bioregion", "species")]]
    )
  })
  
  
  
  bioregion_species_map_data <- function(selected_species, bioregion_name) {
    req(input$bioregion)
    
    df <- nsw_bruv_data$top_50_abundance %>%
      dplyr::filter(bioregion == bioregion_name) %>%
      dplyr::filter(display_name %in% selected_species) %>%
      dplyr::mutate(year = as.Date(paste0(year, "-01", "-01"))) #%>% # Make year a real date
    #glimpse
    
    # TODO include latitude and longitude in create metrics script
    df <- df %>%
      dplyr::left_join(
        nsw_bruv_data$bruv_metadata %>%
          dplyr::select(
            campaignid, sample, sample_url,
            latitude_dd,
            longitude_dd
          )
      )
  }
  
  make_species_leaflet_map <- function(selected_species, bioregion_name) {
    
    pts <- bioregion_species_map_data(selected_species, bioregion_name)
    
    validate(
      need(nrow(pts) > 0, paste("No mappable data available for", selected_species))
    )
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = pts$count,
      na.color = "#BDBDBD"
    )
    
    # Rescale point radius safely
    if (length(unique(stats::na.omit(pts$count))) <= 1) {
      pts$radius <- 8
    } else {
      pts$radius <- scales::rescale(
        pts$count,
        to = c(4, 14),
        from = range(pts$count, na.rm = TRUE)
      )
    }
    
    pts <- pts %>%
      dplyr::mutate(
        popup_text = paste0(
          "<strong> MaxN</strong><br>",
          "count: ", round(count, 2), "<br>",
          "Status: ", status, "<br>"
        )
      )
    
    legend_title <- stringr::str_wrap("MaxN", width = 15)
    legend_title <- gsub("\n", "<br>", legend_title)
    
    base_map(current_zoom = 6) %>%
      fitBounds(
        lng1 = min(pts$longitude_dd, na.rm = TRUE),
        lat1 = min(pts$latitude_dd,  na.rm = TRUE),
        lng2 = max(pts$longitude_dd, na.rm = TRUE),
        lat2 = max(pts$latitude_dd,  na.rm = TRUE)
      ) %>%
      addMapPane("metric_points", zIndex = 430) %>%
      addCircleMarkers(
        data = pts,
        lng = ~longitude_dd,
        lat = ~latitude_dd,
        radius = ~radius,
        fillColor = ~pal(count),
        fillOpacity = 0.8,
        color = "#FFFFFF",
        weight = 1,
        opacity = 1,
        popup = ~popup_text,
        group = "Sampling points",
        options = pathOptions(pane = "metric_points")
      ) %>%
      addLegend(
        pal = pal,
        values = pts$count,
        title = htmltools::HTML(legend_title),
        position = "topleft",
        opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Satellite"
        ),
        overlayGroups = c(
          "Sampling points",
          "NSW Marine Parks",
          "Commonwealth Marine Parks"
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  }
  
  output[[metric_plot_id("bioregion", "species", "map")]] <- renderLeaflet({
    # req(input$bioregion)

    make_species_leaflet_map(
      bioregion_name = input$bioregion,
      selected_species = input[[metric_species_input_id("bioregion", "species")]]
    )
  })
  
  # make_species_length_plot <- function(
  #   bioregion_name,
  #   selected_species
  # ) {
  #   
  #   req(bioregion_name, selected_species)
  #   
  #   df <- nsw_bruv_data$species_length_data %>%
  #     dplyr::filter(
  #       bioregion %in% bioregion_name,
  #       display_name %in% selected_species
  #     )
  #   
  #   validate(
  #     need(
  #       nrow(df) > 0,
  #       paste("No length measurements available for", selected_species)
  #     )
  #   )
  #   
  #   ggplot(
  #     df,
  #     aes(
  #       x = length_mm,
  #       fill = status,
  #       weight = count
  #     )
  #   ) +
  #     
  #     geom_histogram(
  #       bins = 30,
  #       position = "identity",
  #       alpha = 0.65,
  #       colour = "white",
  #       linewidth = 0.2
  #     ) +
  #     
  #     scale_fill_manual(
  #       values = c(
  #         "Fished"  = "#A9173A",
  #         "No-Take" = "#67C7BB"
  #       )
  #     ) +
  #     
  #     scale_x_continuous(
  #       expand = expansion(mult = c(0.01, 0.02))
  #     ) +
  #     
  #     labs(
  #       x = "Fish length (mm)",
  #       y = "Number of fish measured",
  #       fill = NULL
  #     ) +
  #     
  #     theme_minimal(base_size = 16) +
  #     
  #     theme(
  #       legend.position = "bottom",
  #       panel.grid.minor = element_blank()
  #     )
  # }
  # 
  # output[[
  #   metric_plot_id(
  #     "bioregion",
  #     "species",
  #     "length"
  #   )
  # ]] <- renderPlot({
  #   
  #   req(
  #     input$bioregion,
  #     input[[metric_species_input_id("bioregion", "species")]]
  #   )
  #   
  #   make_species_length_plot(
  #     bioregion_name = input$bioregion,
  #     selected_species =
  #       input[[metric_species_input_id("bioregion", "species")]]
  #   )
  # })
  
  make_species_length_plot <- function(
    bioregion_name,
    selected_species,
    facet_by_year = FALSE
  ) {
    
    req(
      bioregion_name,
      selected_species
    )
    
    
    # ---------------------------------------------------------
    # Filter data
    # ---------------------------------------------------------
    
    df <- nsw_bruv_data$species_length_data %>%
      dplyr::filter(
        bioregion == bioregion_name,
        display_name == selected_species
      ) %>%
      dplyr::filter(
        !is.na(length_mm),
        !is.na(status)
      ) %>%
      dplyr::mutate(
        year = factor(
          year,
          levels = sort(unique(year))
        )
      )
    
    
    validate(
      need(
        nrow(df) > 0,
        paste(
          "No length measurements available for",
          selected_species
        )
      )
    )
    
    
    # ---------------------------------------------------------
    # Base plot
    # ---------------------------------------------------------
    
    p <- ggplot(
      df,
      aes(
        x = length_mm,
        fill = status,
        weight = count
      )
    ) +
      
      geom_histogram(
        binwidth = 25,
        boundary = 0,
        position = "identity",
        alpha = 0.65,
        colour = "white",
        linewidth = 0.2
      ) +
      
      scale_fill_manual(
        values = c(
          "Fished"  = "#A9173A",
          "No-Take" = "#67C7BB"
        ),
        drop = FALSE
      ) +
      
      scale_x_continuous(
        expand = expansion(
          mult = c(0.01, 0.02)
        )
      ) +
      
      labs(
        x = "Fish length (mm)",
        y = "Number of fish measured",
        fill = NULL
      ) +
      
      theme_minimal(
        base_size = 16
      ) +
      
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    
    # ---------------------------------------------------------
    # Optional facets
    # ---------------------------------------------------------
    
    if (isTRUE(facet_by_year)) {
      
      p <- p +
        facet_wrap(
          ~ year,
          ncol = 3
        ) +
        theme(
          strip.text = element_text(
            size = 14,
            face = "bold"
          )
        )
    }
    
    
    p
  }
  
  output[[
    metric_species_length_ui_id(
      "bioregion",
      "species"
    )
  ]] <- renderUI({
    
    req(
      input$bioregion,
      input[[
        metric_species_input_id(
          "bioregion",
          "species"
        )
      ]]
    )
    
    
    selected_species <- input[[
      metric_species_input_id(
        "bioregion",
        "species"
      )
    ]]
    
    
    facet_by_year <- isTRUE(
      input[[
        metric_species_length_facet_id(
          "bioregion",
          "species"
        )
      ]]
    )
    
    
    # ---------------------------------------------------------
    # Calculate dynamic height
    # ---------------------------------------------------------
    
    if (facet_by_year) {
      
      n_years <- nsw_bruv_data$species_length_data %>%
        dplyr::filter(
          bioregion == input$bioregion,
          display_name == selected_species
        ) %>%
        dplyr::distinct(year) %>%
        nrow()
      
      
      n_rows <- max(
        1,
        ceiling(n_years / 3)
      )
      
      
      # 375 px per row of facets
      plot_height <- 375 * n_rows
      
    } else {
      
      # Normal non-faceted histogram
      plot_height <- 500
    }
    
    
    metric_plotOutput(
      prefix = "bioregion",
      metric_id = "species",
      which = "length",
      height = plot_height
    )
  })
  
  output[[
    metric_plot_id(
      "bioregion",
      "species",
      "length"
    )
  ]] <- renderPlot({
    
    req(
      input$bioregion,
      input[[
        metric_species_input_id(
          "bioregion",
          "species"
        )
      ]]
    )
    
    
    selected_species <- input[[
      metric_species_input_id(
        "bioregion",
        "species"
      )
    ]]
    
    
    facet_by_year <- isTRUE(
      input[[
        metric_species_length_facet_id(
          "bioregion",
          "species"
        )
      ]]
    )
    
    
    make_species_length_plot(
      bioregion_name = input$bioregion,
      selected_species = selected_species,
      facet_by_year = facet_by_year
    )
  })
  
  # ABUNDANCE AND BIOMASS DIAGNOSTIC PLOTS ----
  # make_ab_diagnostic_plot <- function(
  #   metric_id,
  #   bioregion_name
  # ) {
  #   
  #   req(metric_id, bioregion_name)
  #   
  #   df <- nsw_bruv_data$top_10_diagnostic_a_and_b %>%
  #     dplyr::filter(
  #       bioregion == bioregion_name,
  #       metric == metric_id
  #     )
  #   
  #   validate(
  #     need(
  #       nrow(df) > 0,
  #       paste("No diagnostic data available for", bioregion_name)
  #     )
  #   )
  #   
  #   
  #   # ---------------------------------------------------------
  #   # Species occurring in the top 10 in ONLY ONE year
  #   # ---------------------------------------------------------
  #   
  #   unique_species <- df %>%
  #     dplyr::distinct(year, scientific) %>%
  #     dplyr::count(scientific, name = "n_years") %>%
  #     dplyr::filter(n_years == 1) %>%
  #     dplyr::pull(scientific)
  #   
  #   
  #   # ---------------------------------------------------------
  #   # Prepare labels and within-facet ordering
  #   # ---------------------------------------------------------
  #   
  #   plot_df <- df %>%
  #     dplyr::group_by(year, scientific) %>%
  #     dplyr::mutate(
  #       order_value = mean(value, na.rm = TRUE)
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(
  #       
  #       # Scientific name only
  #       species_label = paste(genus, species),
  #       
  #       # Bold species that are unique to one year's top 10.
  #       # All scientific names are italicised.
  #       species_label = dplyr::if_else(
  #         scientific %in% unique_species,
  #         paste0("***", species_label, "***"),
  #         paste0("*", species_label, "*")
  #       ),
  #       
  #       species_label = tidytext::reorder_within(
  #         species_label,
  #         order_value,
  #         year
  #       )
  #     )
  #   
  #   
  #   # Axis label
  #   x_lab <- dplyr::case_when(
  #     metric_id == "a20" ~ "Average abundance >20 cm per BRUV",
  #     metric_id == "a30" ~ "Average abundance >30 cm per BRUV",
  #     metric_id == "b20" ~ "Average biomass >20 cm per BRUV (kg)",
  #     metric_id == "b30" ~ "Average biomass >30 cm per BRUV (kg)"
  #   )
  #   
  #   
  #   dodge <- position_dodge(width = 0.75)
  #   
  #   
  #   ggplot(
  #     plot_df,
  #     aes(
  #       x = value,
  #       y = species_label,
  #       fill = status
  #     )
  #   ) +
  #     
  #     geom_col(
  #       position = dodge,
  #       width = 0.7,
  #       colour = "black",
  #       linewidth = 0.3
  #     ) +
  #     
  #     geom_errorbarh(
  #       aes(
  #         xmin = pmax(value - se, 0),
  #         xmax = value + se
  #       ),
  #       position = dodge,
  #       height = 0.2
  #     ) +
  #     
  #     facet_wrap(
  #       ~year,
  #       scales = "free_y"
  #     ) +
  #     
  #     tidytext::scale_y_reordered() +
  #     
  #     # Similar appearance to your example
  #     scale_fill_manual(
  #       values = c(
  #         "Fished" = "white",
  #         "No-Take" = "grey50"
  #       ),
  #       drop = FALSE
  #     ) +
  #     
  #     # Similar log-like axis to the example, but can still display zero
  #     scale_x_continuous(
  #       trans = scales::pseudo_log_trans(base = 10),
  #       expand = expansion(mult = c(0, 0.08))
  #     ) +
  #     
  #     labs(
  #       x = x_lab,
  #       y = NULL,
  #       fill = "Status"
  #     ) +
  #     
  #     theme_classic(base_size = 14) +
  #     
  #     theme(
  #       strip.background = element_rect(
  #         fill = "grey85",
  #         colour = "black"
  #       ),
  #       strip.text = element_text(
  #         size = 13
  #       ),
  #       axis.text.y = ggtext::element_markdown(
  #         size = 11
  #       ),
  #       legend.position = "right"
  #     )
  # }
  # 
  # NEW FUNCTION ----
  make_ab_diagnostic_plot <- function(
    metric_id,
    bioregion_name
  ) {
    
    req(metric_id, bioregion_name)
    
    df <- nsw_bruv_data$top_10_diagnostic_a_and_b %>%
      dplyr::filter(
        bioregion == bioregion_name,
        metric == metric_id
      )
    
    validate(
      need(
        nrow(df) > 0,
        paste("No diagnostic data available for", bioregion_name)
      )
    )
    
    
    # ---------------------------------------------------------
    # Identify species that only occur in the top 10 in one year
    # ---------------------------------------------------------
    
    unique_species <- df %>%
      dplyr::distinct(year, scientific) %>%
      dplyr::count(scientific, name = "n_years") %>%
      dplyr::filter(n_years == 1) %>%
      dplyr::pull(scientific)
    
    
    # ---------------------------------------------------------
    # Prepare plot data
    # ---------------------------------------------------------
    
    plot_df <- df %>%
      
      # Order species using mean across Fished / No-Take
      dplyr::group_by(year, scientific) %>%
      dplyr::mutate(
        order_value = mean(value, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      
      dplyr::mutate(
        
        # species_label = paste(genus, species),
        # 
        # # Bold + italic if unique to that year's top 10
        # species_label = dplyr::if_else(
        #   scientific %in% unique_species,
        #   paste0("***", species_label, "***"),
        #   paste0("*", species_label, "*")
        # ),
        
        sci_label = paste(
          genus,
          species
        ),
        
        species_label = dplyr::case_when(
          
          scientific %in% unique_species &
            !is.na(australian_common_name) ~
            paste0(
              "***", sci_label, "***",
              "<br>(",
              australian_common_name,
              ")"
            ),
          
          scientific %in% unique_species ~
            paste0(
              "***",
              sci_label,
              "***"
            ),
          
          !is.na(australian_common_name) ~
            paste0(
              "*", sci_label, "*",
              "<br>(",
              australian_common_name,
              ")"
            ),
          
          TRUE ~
            paste0(
              "*",
              sci_label,
              "*"
            )
        ),
        
        species_label = tidytext::reorder_within(
          species_label,
          order_value,
          year
        ),
        
        # Order separately within each year
        species_label = tidytext::reorder_within(
          species_label,
          order_value,
          year
        )
      )
    
    
    # ---------------------------------------------------------
    # Axis label
    # ---------------------------------------------------------
    
    x_lab <- dplyr::case_when(
      metric_id == "a20" ~ "Average abundance >20 cm per BRUV",
      metric_id == "a30" ~ "Average abundance >30 cm per BRUV",
      metric_id == "b20" ~ "Average biomass >20 cm per BRUV (kg)",
      metric_id == "b30" ~ "Average biomass >30 cm per BRUV (kg)",
      metric_id == "alt" ~ "Average abundance LT per BRUV",
      metric_id == "blt" ~ "Average biomass LT per BRUV (kg)"
    )
    
    
    # ---------------------------------------------------------
    # Plot
    # ---------------------------------------------------------
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(
      plot_df,
      aes(
        x = value,
        y = species_label,
        fill = status
      )
    ) +
      
      geom_col(
        position = dodge
      ) +
      
      geom_errorbarh(
        aes(
          xmin = pmax(value - se, 0),
          xmax = value + se
        ),
        position = dodge,
        height = 0.3
      ) +
      
      facet_wrap(
        ~year,
        scales = "free_y"
      ) +
      
      tidytext::scale_y_reordered() +
      
      # Same colours as other dashboard species plots
      scale_fill_manual(
        values = c(
          "Fished"  = "#A9173A",
          "No-Take" = "#67C7BB"
        )
      ) +
      
      # Same x-axis style as existing bar plots
      # scale_x_continuous(
      #   expand = expansion(mult = c(0, 0.05))
      # ) +
      
          scale_x_continuous(
            trans = scales::pseudo_log_trans(base = 10),
            expand = expansion(mult = c(0, 0.08))
          ) +
      
      labs(
        x = x_lab,
        y = NULL,
        fill = NULL
      ) +
      
      # Same theme as existing bar plots
      theme_classic() +
      
      theme(
        legend.position = "bottom",
        axis.text.y = ggtext::element_markdown(size = 12),
        
        # Keep facet headers simple
        strip.background = element_blank(),
        strip.text = element_text(
          size = 12,
          face = "bold"
        )
      )
  }
  
  # MAKE PLOTS -----
  for (id in c("a20", "b20", "a30", "b30", "alt", "blt")) {
    
    local({
      
      metric_id <- id
      
      output[[
        metric_plot_id(
          "bioregion",
          metric_id,
          "diagnostic"
        )
      ]] <- renderPlot({
        
        req(input$bioregion)
        
        make_ab_diagnostic_plot(
          metric_id = metric_id,
          bioregion_name = input$bioregion
        )
        
      })
      
    })
  }
}