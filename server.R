# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 20, current_zoom = 9) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, max_zoom)) |>
    setView(lng = mean(nsw_bruv_data$bruv_metadata$longitude_dd),
            lat = mean_lat, current_zoom) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    
    # TODO add state marine parks
    # Use regular polygons for static layers:
    # addPolygons(
    #   data = state_mp, 
    #   color = "black", weight = 1,
    #   fillColor = ~state.pal(zone), fillOpacity = 0.8,
    #   group = "State Marine Parks",
    #   popup = ~name,
    #   options = pathOptions(pane = "polys")
    # ) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      options = pathOptions(pane = "polys"), group = "Australian Marine Parks"
    ) %>%
    
    # Legends
    # addLegend(
    #   pal = state.pal,
    #   values = state_mp$zone,
    #   opacity = 1,
    #   title = "State Zones",
    #   position = "bottomright",
    #   group = "State Marine Parks"
    # ) |>
    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Australian Marine Park Zones",
      position = "bottomright",
      group = "Australian Marine Parks"
    )
}

# ---- output id helpers -------------------------------------------------------
# TODO move this into the helpers code
metric_plot_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_plot_", metric_id, "_", which)
}

metric_plotOutput <- function(prefix, metric_id, which, height = 600, spinner_type = 6) {
  withSpinner(
    plotOutput(metric_plot_id(prefix, metric_id, which), height = height),
    color = getOption("spinner.color", default = "#0D576E"),
    type = spinner_type
  )
}

metric_plot_type_input_id <- function(prefix, metric_id) {
  paste0(prefix, "_", metric_id, "_plot_type")
}

metric_tab_body_ui <- function(metric_id, prefix = "bioregion") {
  
  data_id <- metric_id
  plot_type_id <- metric_plot_type_input_id(prefix, data_id)
  
  tagList(
    # Your existing layout(s)
    switch(
      metric_id,
      
      # total_abundance = {
      #   tagList(
      #     layout_columns(
      #       col_widths = c(6, 6),
      #       metric_plotOutput(prefix, data_id, "main"),
      #       metric_plotOutput(prefix, data_id, "status")
      #     )
      #   )
      # },
      # default: 2 plots
      {
        tagList(
        metric_plotOutput(prefix, data_id, "year"),
        layout_columns(
          col_widths = c(6, 6),
          metric_plotOutput(prefix, data_id, "main"),
          metric_plotOutput(prefix, data_id, "status")
        ))
      }
    )
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
      addMapPane("points",    zIndex = 411) |>
      addMapPane("highlight", zIndex = 415) %>%
      
      leafgl::addGlPoints(
        data = pts,
        # fillColor = method_cols[pts$method],
        weight = 1,
        # popup = pts$popup,
        group = "Sampling locations",
        pane  = "points"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) #%>%
    
    # hideGroup("Australian Marine Parks") 
    
    
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
    
    method_cols <- c("BRUVs" = "#004DA7", "UVC" = "#C600FF")
    
    pts <- bioregion_deployments()
    
    m <- base_map(current_zoom = 6) |>
      
      fitBounds(bio_min_lon(), bio_min_lat(), bio_max_lon(), bio_max_lat()) %>%
      
      # define panes with explicit stacking
      addMapPane("points",    zIndex = 411) |>
      addMapPane("highlight", zIndex = 415) %>%
      
      leafgl::addGlPoints(
        data = pts,
        # fillColor = method_cols[pts$method],
        weight = 1,
        # popup = pts$popup,
        group = "Sampling locations",
        pane  = "points"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) #%>%
    
    # hideGroup("Australian Marine Parks") 
    
    
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
                              number_species
  ) {
    
    # ---- Data prep ----
    df_raw <- nsw_bruv_data$top_species |>
      dplyr::filter(group == "bioregion") %>%
      dplyr::filter(bioregion == bioregion_name)
    
    # Top N species within the focal period
    top_species <- df_raw |>
      dplyr::slice_max(order_by = average_abundance,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # TODO do this before the server code
    
    # Extract sci/common and build markdown label
    plot_df <- df_raw %>%
      dplyr::filter(display_name %in% top_species) %>%
      tidyr::extract(
        display_name,
        into   = c("sci", "common"),
        regex  = "^(.*?)\\s*\\((.*?)\\)$",
        remove = FALSE
      ) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # Species order: smallest at bottom, biggest at top for focal period
    species_order <- plot_df |>
      dplyr::arrange(average_abundance) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    p <- ggplot(
      plot_df,
      aes(
        x    = average_abundance,
        y    = label
      )
    ) +
      geom_col() + # position = dodge
      # geom_errorbarh( 
      #   aes(
      #     xmin = average_abundance - se,
      #     xmax = average_abundance + se
      #   ),
      #   position = dodge,
      #   height   = 0.3
      # ) +
      labs(
        x     = "Average abundance per BRUV",
        y     = NULL,
        title = title_lab,
        fill  = NULL
      ) 
    
    # Shared scales / theme
    p +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )
  }
  
  # Bioregion top species plot ----
  output$bioregion_top <- renderPlot({
    
    req(input$bioregion)
    
    make_top10_plot(
      title_lab      = "Most common species",
      bioregion_name = input$bioregion,
      number_species = input$bioregion_number_species
    )
  })
  
  # Build a tabbed card with one tab per metric
  output$bioregion_tabset <- renderUI({
    req(input$bioregion)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(id, prefix = "bioregion")
        )
      })
    )
  })
  
  # # SPECIES RICHNESS: main plot --------------------
  # output$bioregion_plot_species_richness_main <- renderPlot({
  #   req(input$bioregion)
  #   
  #   df <- nsw_bruv_data$metrics %>%
  #     dplyr::filter(bioregion == input$bioregion) %>%
  #     dplyr::filter(metric %in% "species_richness")
  #   
  #   mean_se <- df %>%
  #     dplyr::group_by(bioregion) %>%
  #     dplyr::summarise(mean = mean(value), 
  #                      se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value)))) %>%
  #     dplyr::ungroup() %>%
  #     glimpse
  #   
  #   ggplot(df, aes(x = bioregion, y = value)) +
  #     
  #     geom_boxplot(
  #       width = 0.6,
  #       outlier.shape = NA,
  #       alpha = 0.85,
  #       colour = "black"
  #     ) +
  #     
  #     # raw points
  #     geom_jitter(
  #       # aes(colour = period),
  #       width = 0.15,
  #       height = 0,      # <— prevents any vertical jitter
  #       alpha = 0.35,
  #       size = 1.2
  #     ) +
  #     
  #     # # mean ± SE
  #     geom_pointrange(
  #       data = mean_se,
  #       aes(
  #         x    = bioregion,
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
  #       x = NULL,
  #       y = metric_y_lab[["species_richness"]],
  #       subtitle = input$bioregion
  #     ) +
  #     theme_minimal(base_size = 16) +
  #     theme(
  #       legend.position  = "none",
  #       panel.grid.minor = element_blank()
  #     )
  # }) 
  
  
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
  
  make_metric_boxplot <- function(metric_id,
                                  x_col = "bioregion",
                                  plot_title = NULL,
                                  plot_subtitle = NULL) {
    
    df <- bioregion_metric_data(metric_id)
    
    validate(
      need(nrow(df) > 0, paste("No data available for", metric_id)),
      need(x_col %in% names(df), paste("Column", x_col, "not found in metrics data"))
    )
    
    mean_se <- df %>%
      dplyr::group_by(.data[[x_col]]) %>%
      dplyr::summarise(
        n    = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        se   = dplyr::if_else(
          n > 1,
          stats::sd(value, na.rm = TRUE) / sqrt(n),
          0
        ),
        .groups = "drop"
      )
    
    ggplot(df, aes(x = .data[[x_col]], y = value)) +
      
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      geom_jitter(
        width = 0.15,
        height = 0,
        alpha = 0.35,
        size = 1.2
      ) +
      
      geom_pointrange(
        data = mean_se,
        aes(
          x    = .data[[x_col]],
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      
      labs(
        x        = NULL,
        y        = get_metric_label(metric_id),
        title    = plot_title,
        subtitle = plot_subtitle
      ) +
      
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }
  
  make_metric_main_plot <- function(metric_id) {
    make_metric_boxplot(
      metric_id     = metric_id,
      x_col         = "bioregion",
      plot_title    = metric_defs[[metric_id]],
      plot_subtitle = paste(input$bioregion, collapse = ", ")
    )
  }
  
  make_metric_boxplot_year <- function(metric_id,
                                  x_col = "start_month",
                                  plot_title = NULL,
                                  plot_subtitle = NULL) {
    
    df <- bioregion_metric_data(metric_id)
    
    validate(
      need(nrow(df) > 0, paste("No data available for", metric_id)),
      need(x_col %in% names(df), paste("Column", x_col, "not found in metrics data"))
    )
    
    mean_se <- df %>%
      dplyr::group_by(.data[[x_col]]) %>%
      dplyr::summarise(
        n    = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        se   = dplyr::if_else(
          n > 1,
          stats::sd(value, na.rm = TRUE) / sqrt(n),
          0
        ),
        .groups = "drop"
      )
    
    ggplot(df, aes(x = .data[[x_col]], y = value)) +
      
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.85,
        colour = "black"
      ) +
      
      geom_jitter(
        width = 0.15,
        height = 0,
        alpha = 0.35,
        size = 1.2
      ) +
      
      geom_pointrange(
        data = mean_se,
        aes(
          x    = .data[[x_col]],
          y    = mean,
          ymin = mean - se,
          ymax = mean + se
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.6
      ) +
      
      labs(
        x        = NULL,
        y        = get_metric_label(metric_id),
        title    = plot_title,
        subtitle = plot_subtitle
      ) +
      
      theme_minimal(base_size = 16) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank()
      )
  }
  
  make_metric_year_plot <- function(metric_id) {
    make_metric_boxplot_year(
      metric_id     = metric_id,
      x_col         = "start_month",
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
  
  
}