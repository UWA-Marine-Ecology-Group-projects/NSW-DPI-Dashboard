ui <- page_navbar(
  title = div(
    "NSW DPI Dashboard",
    favicon = "www/favicon.ico",
    style = "display:flex; gap:10px; align-items:center; padding-right:15px; font-weight:bold; color:#063F5C;"
  ),
  
  tags$head(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Barlow:wght@300;400;500;600;700&display=swap"
  ),
  
  theme = client_theme,
  
  # --- CSS: sticky navbar; sidebar map fills; right panel scrolls ---
  tags$head(
    tags$style(HTML("
                    
                    .navbar {
  border-bottom: 1px solid rgba(13, 87, 110, 0.15);
                    }

  .navbar .nav-link.active {
  font-weight: 600;
  border-bottom: 2px solid #063F5C;
  }

  
.bslib-layout-sidebar-sidebar {
  padding-top: 0.75rem;
  box-shadow: 6px 0 16px rgba(0,0,0,0.03);
}
                    
  /* Force Barlow globally */
  html, body, .bslib-page {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* Common Bootstrap/UI elements */
  .navbar, .nav-link, .btn, .form-control, .form-select, .dropdown-menu,
  .card, .bslib-card, .value-box, .sidebar, .bslib-sidebar {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* selectizeInput */
  .selectize-control, .selectize-input, .selectize-dropdown, .selectize-dropdown-content {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* DT tables */
  table.dataTable, table.dataTable * {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }

  /* Leaflet controls (zoom buttons, layer control text, etc.) */
  .leaflet-container, .leaflet-control, .leaflet-control * {
    font-family: 'Barlow', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }
  

.modern-kpi {
  background-color: #FFFFFF !important;
  border: 1px solid #063F5C !important;   /* teal border */
  border-radius: 1rem !important;
  box-shadow: none !important;
text-align:center; width:100%; 
}

.value-box {
  background-color: #FFFFFF !important;
  border: 2px solid #063F5C !important;
  border-radius: 1rem !important;
  box-shadow: none !important;
}

/* KPI title */
.modern-kpi .value-box-title {
  color: #4F6F7C;
  font-weight: 500;
}

/* KPI numbers */
.modern-kpi .pp-val,
.modern-kpi .value-box-value {
  color: #063F5C !important;
  font-weight: 700;
  font-size: 1.4rem;
}

/* KPI labels */
.modern-kpi .pp-lab {
  color: #6D8C98;
  font-weight: 500;
}

/* KPI icons */
.modern-kpi .fa,
.modern-kpi svg,
.modern-kpi img {
  color: #063F5C !important;
  filter: none;
}

.kpi-row {
  background-color: #F1F7F9;   /* very light marine tint */
  padding: 1rem;
  border-radius: 1rem;
}

.kpi-section {
  background-color: #F1F6F8;   /* very light tint */
  padding: 1.25rem;
  border-radius: 0.75rem;
}

.bslib-sidebar-layout>.sidebar {
    grid-column: 1 / 2;
    width: 100%;
    border-right: var(--_vert-border);
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    color: var(--_sidebar-fg);
    background-color: rgb(227 235 240 / 100%);
}
")),

tags$style(HTML("
      /* Navbar fixed */
      .bslib-page .navbar { position: sticky; top: 0; z-index: 1050; }
      .bslib-page { scroll-padding-top: var(--bslib-navbar-height, 56px); }

      @media (min-width: 992px) {
        /* Sidebar: sticky, full viewport under the navbar, and FLEX COLUMN */
        .bslib-layout-sidebar-sidebar {
          position: sticky;
          top: var(--bslib-navbar-height, 56px);
          height: calc(100dvh - var(--bslib-navbar-height, 56px));
          display: flex;
          flex-direction: column;
          overflow: hidden; /* the map card will control overflow */
        }

        /* Sidebar map card flexes to fill; min-height:0 allows flex child to shrink */
        .sidebar-map-card {
          flex: 1 1 auto;
          min-height: 0;
          display: flex;
          flex-direction: column;
          margin-bottom: 0;
        }

        /* Card body must flex too, with no padding so the map can fill it */
        .sidebar-map-card .card-body {
          flex: 1 1 auto;
          min-height: 0;
          padding: 0;
          display: flex;
          flex-direction: column;
        }

        /* Map wrapper fills the body; Leaflet fills the wrapper */
        .sidebar-map-fill { flex: 1 1 auto; min-height: 0; }
        .sidebar-map-card .leaflet-container { height: 100% !important; width: 100% !important; }

        /* Right/main column gets the only scrollbar */
        .bslib-layout-sidebar-main {
          height: calc(100dvh - var(--bslib-navbar-height, 56px));
          overflow: auto;
        }
      }
      
/* ===== Sidebar background (layout_sidebar) ===== */
.bslib-layout-sidebar-sidebar {
  background: #E9F1F5 !important;
  border-right: 1px solid rgba(13,87,110,0.12) !important;
  box-shadow: 6px 0 16px rgba(0,0,0,0.03);
}

/* The inner sidebar container should also be tinted (this is usually the visible layer) */
.bslib-layout-sidebar-sidebar .bslib-sidebar,
.bslib-layout-sidebar-sidebar .sidebar-content,
.bslib-layout-sidebar-sidebar .offcanvas-body {
  background: #E9F1F5 !important;
}

/* Header row */
table.hab-table thead th {
  background-color: #4A8C8C !important;
  color: #ffffff !important;
  border-color: rgba(0,0,0,0.12) !important;
}

/* Stripe colours applied to CELLS (td/th), not tr */
table.hab-table tbody tr > * {
  background-color: #d7e1e1 !important;   /* odd rows */
  color: #0b1f24 !important;
}

table.hab-table tbody tr:nth-child(even) > * {
  background-color: #80A9A9 !important;   /* even rows */
}

/* Optional: tidy borders */
table.hab-table td, table.hab-table th {
  border-color: rgba(0,0,0,0.12) !important;
}
    "))
  ),

tags$head(
  tags$style(HTML("
    .pp-wrap { display:flex; justify-content:space-between; gap:2rem; margin-top:.25rem; }
    .pp-col  { text-align:center; flex:1; }
    .pp-lab  { font-size:1.00rem; opacity:0.85; display:block; }
    .pp-val  { font-size:1.25rem; font-weight:700; margin-top:.25rem; display:block; }
    .pp-title-center .value-box-title { text-align:center; width:100%; }
  ")
  )
),

tags$head(
  tags$style(HTML("
    .vb-icon-wrap {
      padding-top: 2rem;      /* move icon down inside box */
      /* or use margin-top instead if you prefer */
      /* margin-top: 3rem; */
    }
  ")
  )
),

tags$head(
  tags$style(HTML("
    /* existing CSS ... */

    /* Make spinner wrappers fill the map card */
    .map-full-wrapper {
      height: 100%;
    }

    .map-full-wrapper .shiny-spinner-output-container,
    .map-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }

    .map-full-wrapper .leaflet-container {
      height: 100% !important;
      width: 100% !important;
    }
  "))
),

tags$head(
  tags$style(HTML("
    /* Map spinners (leaflet) */
    .map-full-wrapper {
      height: 100%;
    }
    .map-full-wrapper .shiny-spinner-output-container,
    .map-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }
    .map-full-wrapper .leaflet-container {
      height: 100% !important;
      width: 100% !important;
    }

    /* Plot spinners: parent controls height */
    .plot-full-wrapper {
      height: 100%;
    }
    .plot-full-wrapper .shiny-spinner-output-container,
    .plot-full-wrapper .shiny-spinner-placeholder {
      height: 100%;
    }
    
    .kpi-title {
  font-weight: 600;
  color: #063F5C;
  margin-bottom: 1rem;
}

.page-header h3 {
  font-weight: 600;
  margin-bottom: 0.25rem;
}
  ")),
  
  # tags$head(
  #   tags$style(HTML("
  #   .overview-value-box .value-box-showcase,
  #   .overview-value-box .value-box-showcase *,
  #   .overview-value-box .value-box-showcase svg,
  #   .overview-value-box .value-box-showcase svg *,
  #   .overview-value-box .value-box-showcase .fa,
  #   .overview-value-box .value-box-showcase i,
  #   .bioregion-value-box .value-box-showcase,
  #   .bioregion-value-box .value-box-showcase *,
  #   .bioregion-value-box .value-box-showcase svg,
  #   .bioregion-value-box .value-box-showcase svg *,
  #   .bioregion-value-box .value-box-showcase .fa,
  #   .bioregion-value-box .value-box-showcase i {
  #     color: #FFFFFF !important;
  #     fill: #FFFFFF !important;
  #     stroke: #FFFFFF !important;
  #   }
  # "))
  # )
),


nav_panel(
  "Overview",
  
  layout_columns(
    col_widths = c(7, 5),
    
    div(
      
      div(class = "page-header",
          h3("Overview"),
          # h5("NSW DPI Dashboard", class = "text-muted")
      ),
      
      ui <- page_fillable(
        tags$head(
          tags$style(HTML("
      .overview-value-box {
        background-color: #063F5C !important;
        color: white !important;
        border-radius: 10px;
        min-height: 170px;
      }

      .overview-value-box .value-box-title {
        font-size: 1.5rem;
        font-weight: 700;
      }

.overview-value-box .value-box-showcase {
  color: #F6FBFE !important;
}

      .overview-value-box .value-box-value {
        font-size: 2.8rem;
        font-weight: 400;
      }
      
           .bioregion-value-box .value-box-title {
        font-size: 1.25rem;
        font-weight: 700;
      }

.bioregion-value-box .value-box-showcase {
  color: #F6FBFE !important;
}

      .bioregion-value-box .value-box-value {
        font-size: 2rem;
        font-weight: 400;
      }
      
            .bioregion-value-box {
        background-color: #063F5C !important;
        color: white !important;
        border-radius: 10px;
        min-height: 170px;
      }
    "))
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          value_box(
            title = "Deployments",
            value = textOutput("num_bruvs"),
            showcase = bs_icon("camera-video-fill", size = "1.5em"),
            class = "overview-value-box"
          ),
          
          value_box(
            title = "Fish counted",
            value = textOutput("num_fish"),
            showcase = icon("fish"),
            class = "overview-value-box"
          ),
          
          value_box(
            title = "Fish measured",
            value = textOutput("num_lengths"),
            showcase = bs_icon("rulers", size = "1.5em"),
            class = "overview-value-box"
          ),
          
          value_box(
            title = "Years sampled",
            value = textOutput("years_included"),
            showcase = bs_icon("calendar", size = "1.5em"),
            class = "overview-value-box"
          ),
          
          # value_box(
          #   title = "Depths Surveyed",
          #   value = textOutput("depths_surveyed"),
          #   showcase = bs_icon("arrows-expand", size = "4em"),
          #   class = "overview-value-box"
          # ),
          # 
          # value_box(
          #   title = "Average Depth",
          #   value = textOutput("average_depth"),
          #   showcase = bs_icon("activity", size = "4em"),
          #   class = "overview-value-box"
          # )
        )
      ),
      
      card(
        card_header("Dashboard Aims"),
        card_body(
          p(HTML("This dashboard provides a visual assessment ...")),
          p("By integrating standardised, quality-controlled BRUV annotations with clear temporal comparisons, the dashboard helps highlight shifts in community structure and supports evidence-based management decisions.")
        )
      ),
    ),
    
    
    card(
      full_screen = TRUE,
      card_header("Map of sampling locations"),
      div(
        class = "map-full-wrapper",
        withSpinner(
          leafletOutput("map", height = "100%"),
          color = getOption("spinner.color", default = "#063F5C"),
          type = 6
        )
      )
    )
    
  )
),

nav_panel(
  "Bioregion Summary",
  layout_sidebar(
    sidebar = sidebar(
      width = "350px",
      
      h5("Select data:"),
      
      radioButtons(
        inputId  = "method",
        label    = "Choose a method to display: (TBA)",
        choices  = c("BRUVS", "Dive"),
        inline   = TRUE
      ),
      
      selectizeInput(
        "bioregion",
        "Choose a bioregion:",
        choices = NULL, multiple = FALSE,
        options = list(placeholder = "Choose a region...")
      ),
      
      hr(),
      
      h6("Years sampled:"),
      textOutput("years_for_bioregion"),
      br(),
      
      h6("Summary:"),
      uiOutput("bioregion_summary_text"),
      br(),
      
      helpText("")
    ),
    
    div(
      class = "container-fluid",
      
      layout_columns(
        col_widths = c(7, 5),
        
        
        card(
          min_height = 600,
          max_height = 800,
          full_screen = TRUE,
          card_header("Survey Effort"),
          div(
            class = "map-full-wrapper",
            withSpinner(
              leafletOutput("bioregion_survey_effort", height = "100%"),
              color = getOption("spinner.color", default = "#063F5C"),
              type = 6
            )
          )
        ),
        
        div(
          # card(
          #   card_header(
          #     div(
          #       "Overview",
          #       style = "display:inline-block;"
          #     )
          #   ),
          
          h3("Bioregion overview"),
            
            layout_columns(
              col_widths = c(6, 6),
            
            value_box(
              title = "Deployments",
              value = textOutput("bioregion_num_bruvs"),
              showcase = bs_icon("camera-video-fill", size = "1.5em"),
              class = "bioregion-value-box",
              height = "200px",
            ),
            
            value_box(
              title = "Fish counted",
              value = textOutput("bioregion_num_fish"),
              showcase = icon("fish"),
              class = "bioregion-value-box",
              height = "200px",
            ),
            
            value_box(
              title = "Fish measured",
              value = textOutput("bioregion_num_lengths"),
              showcase = bs_icon("rulers", size = "1.25em"),
              class = "bioregion-value-box",
              height = "200px",
            ),
            
            value_box(
              title = "Years sampled",
              value = textOutput("bioregion_years_included"),
              showcase = bs_icon("calendar", size = "1.25em"),
              class = "bioregion-value-box",
              height = "200px",
            ),
          ))
        # )
      ),
      
      card(
        min_height = 500,
        card_header("Most abundant species"),
        full_screen = TRUE,
        
        layout_sidebar(
          sidebar = div(
            h6(strong("Plot inputs:")),
            numericInput( 
              "bioregion_number_species", 
              "Choose number of species to plot", 
              value = 10, 
              min   = 1, 
              max   = 20 
            ),
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            div(
              class = "plot-full-wrapper",
              # style = "height:500px;",
              withSpinner(
                plotOutput("bioregion_top", height = "100%"),
                color = getOption("spinner.color", default = "#063F5C"),
                type = 6
              )
            ),
            
            div(
              class = "plot-full-wrapper",
              # style = "height:500px;",
              withSpinner(
                plotOutput("bioregion_top_status", height = "100%"),
                color = getOption("spinner.color", default = "#063F5C"),
                type = 6
              )
            )
          )
        )
      ),
      # br(),
      uiOutput("bioregion_tabset")   # tabset stays, now below the table
    )
  )
),

# nav_panel(
#   "Marine Park Summary",
#   layout_sidebar(
#     sidebar = sidebar(
#       width = "350px",
#       selectizeInput(
#         "location",
#         "Choose a location",
#         choices = NULL, multiple = FALSE,
#         options = list(placeholder = "Choose a location...")
#       ),
#       
#       h6("Years sampled:"),
#       textOutput("years_for_location"),
#       br(),
#       
#       h6("Summary:"),
#       uiOutput("location_summary_text"),
#       br(),
#       
#       helpText("")
#     ),
#     
#     div(
#       class = "container-fluid",
#       
#       layout_columns(
#         col_widths = c(7, 5),
#         
#         card(
#           min_height = 600,
#           full_screen = TRUE,
#           card_header("Survey Effort"),
#           div(
#             class = "map-full-wrapper",
#             withSpinner(
#               leafletOutput("location_survey_effort", height = "100%"),
#               color = getOption("spinner.color", default = "#063F5C"),
#               type = 6
#             )
#           )
#         ),
#         
#         div(
#           card(
#             card_header(
#               div(
#                 "Location Impact overview",
#                 style = "display:inline-block;"
#               ),
#               div(
#                 actionLink(
#                   inputId = "open_info_pointers_location",
#                   label = NULL,
#                   icon = icon("circle-info")
#                 ),
#                 style = "float:right; margin-top:-2px;"
#               )
#             ),
#             spinnerPlotOutput("location_impact_gauges", height = 350)
#           ),
#           
#           card(
#             card_header(
#               div(
#                 "Percentage change compared to pre-bloom levels",
#                 style = "display:inline-block;"
#               ),
#               div(
#                 actionLink(
#                   inputId = "open_info_table_location",
#                   label = NULL,
#                   icon = icon("circle-info")
#                 ),
#                 style = "float:right; margin-top:-2px;"
#               )
#             ),
#             card_body(
#               spinnerUiOutput("location_change_table")
#             )
#           )
#         )
#       ),
#       
#       card(
#         min_height = 500,
#         card_header("Common species"),
#         full_screen = TRUE,
#         
#         layout_sidebar(
#           sidebar = div(
#             h6(strong("Plot inputs:")),
#             numericInput(
#               "location_number_species",
#               "Choose number of species to plot",
#               value = 10,
#               min   = 1,
#               max   = 20
#             ),
#             checkboxInput(
#               "location_species_status",
#               "Show status (Fished vs No-take)",
#               FALSE
#             ),
#             checkboxInput(
#               "location_species_facet",
#               "Facet by status",
#               FALSE
#             )
#           ),
#           
#           layout_columns(
#             col_widths = c(6, 6),
#             
#             div(
#               class = "plot-full-wrapper",
#               withSpinner(
#                 plotOutput("location_common_pre", height = "100%"),
#                 color = getOption("spinner.color", default = "#063F5C"),
#                 type = 6
#               )
#             ),
#             div(
#               class = "plot-full-wrapper",
#               withSpinner(
#                 plotOutput("location_common_post", height = "100%"),
#                 color = getOption("spinner.color", default = "#063F5C"),
#                 type = 6
#               )
#             )
#           )
#         )
#       ),
#       
#       uiOutput("location_tabset")
#     )
#   )
# ),

nav_spacer(),

nav_item(
  tags$div(
    style = "display:flex; gap:10px; align-items:center; padding-right:15px;",
    tags$img(src = "nsw_dpi_logo.jpg", height = "70px")
  )
)
)
