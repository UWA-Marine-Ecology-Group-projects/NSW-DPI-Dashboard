library(bslib)

client_cols <- c(
  navy   = "#063F5C",
  crimson = "#A9173A",
  powder = "#CBEDFD",
  teal   = "#67C7BB",
  blue   = "#087CC1"
)

client_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  
  # Core Bootstrap colours
  bg = "#F6FBFE",
  fg = "#0B2533",
  
  primary = client_cols[["navy"]],
  secondary = client_cols[["teal"]],
  success = client_cols[["teal"]],
  info = client_cols[["blue"]],
  
  # No amber/orange in the client palette, so use crimson for clinical/risk alerts.
  warning = client_cols[["crimson"]],
  danger = client_cols[["crimson"]],
  
  # Typography: professional, scientific, readable.
  base_font = font_collection(
    font_google("Inter", local = TRUE),
    "system-ui",
    "-apple-system",
    "BlinkMacSystemFont",
    "'Segoe UI'",
    "sans-serif"
  ),
  heading_font = font_collection(
    font_google("IBM Plex Sans", local = TRUE),
    "system-ui",
    "-apple-system",
    "BlinkMacSystemFont",
    "'Segoe UI'",
    "sans-serif"
  ),
  code_font = font_collection(
    font_google("IBM Plex Mono", local = TRUE),
    "'SFMono-Regular'",
    "'Consolas'",
    "'Liberation Mono'",
    "monospace"
  ),
  
  # Lower-level Bootstrap variables
  "font-size-base" = "0.95rem",
  "body-bg" = "#F6FBFE",
  "body-color" = "#0B2533",
  
  "link-color" = client_cols[["blue"]],
  "link-hover-color" = client_cols[["navy"]],
  
  "component-active-bg" = client_cols[["navy"]],
  "component-active-color" = "#FFFFFF",
  
  "border-color" = "rgba(6, 63, 92, 0.18)",
  "border-radius" = "0.75rem",
  "border-radius-sm" = "0.45rem",
  "border-radius-lg" = "1rem",
  
  "box-shadow" = "0 0.45rem 1.4rem rgba(6, 63, 92, 0.10)",
  "box-shadow-sm" = "0 0.15rem 0.65rem rgba(6, 63, 92, 0.08)",
  
  "input-border-color" = "rgba(6, 63, 92, 0.25)",
  "input-focus-border-color" = client_cols[["blue"]],
  "input-focus-box-shadow" = "0 0 0 0.22rem rgba(8, 124, 193, 0.18)",
  
  "navbar-dark-color" = "rgba(255, 255, 255, 0.86)",
  "navbar-dark-hover-color" = "#FFFFFF",
  "navbar-dark-active-color" = "#FFFFFF",
  
  "table-hover-bg" = "rgba(203, 237, 253, 0.38)"
)