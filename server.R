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
# metric_plot_id <- function(prefix, metric_id, which) {
#   paste0(prefix, "_plot_", metric_id, "_", which)
# }
# 
# metric_plotOutput <- function(prefix, metric_id, which, height = 600, spinner_type = 6) {
#   withSpinner(
#     plotOutput(metric_plot_id(prefix, metric_id, which), height = height),
#     color = getOption("spinner.color", default = "#0D576E"),
#     type = spinner_type
#   )
# }

metric_plot_type_input_id <- function(prefix, metric_id) {
  paste0(prefix, "_", metric_id, "_plot_type")
}

metric_year_input_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_", which, "_year_", metric_id)
}

metric_tab_body_ui <- function(metric_id, prefix = "bioregion", year_choices = NULL) {
  
  data_id <- metric_id
  
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
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(
            metric_id = id,
            prefix = "bioregion",
            year_choices = year_choices
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
                                       x_col = "start_month",
                                       plot_title = NULL,
                                       plot_subtitle = NULL) {
    
    df <- bioregion_metric_data(metric_id)
    
    validate(
      need(nrow(df) > 0, paste("No data available for", metric_id)),
      need(x_col %in% names(df), paste("Column", x_col, "not found in metrics data"))
    )
    
    # Make start_month a real date
    df <- df %>%
      dplyr::mutate(
        start_month = as.Date(paste0(start_month, "-01"))
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
      
      # geom_boxplot(
      #   width = 0.6,
      #   outlier.shape = NA,
      #   alpha = 0.85,
      #   colour = "black"
      # ) +
      # 
      # geom_jitter(
      #   width = 0.15,
      #   height = 0,
      #   alpha = 0.35,
      #   size = 1.2
      # ) +
      
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
        position = position_dodge(width = 0.5),
        inherit.aes = FALSE,
        # colour = "black",
        linewidth = 0.6
      ) +
      
      labs(
        x        = NULL,
        y        = get_metric_label(metric_id)#,
        # title    = plot_title,
        # subtitle = plot_subtitle
      ) +
      
      # scale_y_continuous(
      #   limits = c(0, NA),
      #   expand = expansion(mult = c(0, 0.05))
      # ) +
      
      scale_x_date(
        date_labels = "%Y-%m",
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
  
  
  bioregion_metric_map_data <- function(metric_id) {
    req(input$bioregion)
    
    df <- nsw_bruv_data$metrics %>%
      dplyr::filter(bioregion %in% input$bioregion) %>%
      dplyr::filter(metric == metric_id)
    
    # If latitude/longitude are already in nsw_bruv_data$metrics, this will do nothing.
    # If they are not, join them from bruv_metadata.
    if (!all(c("latitude_dd", "longitude_dd") %in% names(df))) {
      
      join_cols <- intersect(
        c("deployment_id", "sample_id", "sample", "sample_name", "deployment"),
        names(df)
      )
      
      metadata_join_cols <- intersect(join_cols, names(nsw_bruv_data$bruv_metadata))
      
      validate(
        need(
          length(metadata_join_cols) > 0,
          "No matching sample/deployment ID found to join metric data to BRUV metadata."
        )
      )
      
      join_col <- metadata_join_cols[1]
      
      df <- df %>%
        dplyr::left_join(
          nsw_bruv_data$bruv_metadata %>%
            dplyr::select(
              dplyr::all_of(join_col),
              latitude_dd,
              longitude_dd
            ),
          by = join_col
        )
    }
    
    df %>%
      dplyr::filter(
        !is.na(latitude_dd),
        !is.na(longitude_dd),
        !is.na(value)
      )
  }
  
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
        overlayGroups = c(
          "Australian Marine Parks",
          "State Marine Parks",
          "Metric values"
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
    number_species = 10,
    title_lab = NULL
  ) {
    
    req(bioregion_name, selected_year)
    
    df_raw <- nsw_bruv_data$top_50_most_abundant_species_bioregion_status_year %>%
      dplyr::filter(bioregion == bioregion_name) %>%
      dplyr::filter(as.character(year) == as.character(selected_year))
    
    validate(
      need(nrow(df_raw) > 0, paste("No species data available for", bioregion_name, "in", selected_year))
    )
    
    top_species <- df_raw %>%
      dplyr::group_by(display_name) %>%
      dplyr::summarise(
        overall_average_abundance = sum(average_abundance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::slice_max(
        order_by = overall_average_abundance,
        n = number_species,
        with_ties = FALSE
      ) %>%
      dplyr::pull(display_name)
    
    plot_df <- df_raw %>%
      dplyr::filter(display_name %in% top_species)
    
    species_order <- plot_df %>%
      dplyr::group_by(label) %>%
      dplyr::summarise(
        overall_average_abundance = sum(average_abundance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(overall_average_abundance) %>%
      dplyr::pull(label)
    
    plot_df <- plot_df %>%
      dplyr::mutate(
        label = factor(label, levels = species_order)
      )
    
    dodge <- position_dodge(width = 0.75)
    
    ggplot(plot_df, aes(x = average_abundance, y = label, fill = status)) +
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
    
    make_top_abundance_bioregion_status_year_plot(
      bioregion_name = input$bioregion,
      selected_year  = input[[metric_year_input_id("bioregion", "total_abundance", "left")]],
      number_species = input$bioregion_number_species,
      title_lab      = input[[metric_year_input_id("bioregion", "total_abundance", "left")]]
    )
  })
  
  output[[metric_plot_id("bioregion", "total_abundance", "right_year_status")]] <- renderPlot({
    req(input$bioregion)
    req(input[[metric_year_input_id("bioregion", "total_abundance", "right")]])
    
    make_top_abundance_bioregion_status_year_plot(
      bioregion_name = input$bioregion,
      selected_year  = input[[metric_year_input_id("bioregion", "total_abundance", "right")]],
      number_species = input$bioregion_number_species,
      title_lab      = input[[metric_year_input_id("bioregion", "total_abundance", "right")]]
    )
  })
  
  
}