library(rsconnect)
# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = "shiny/ho_rate",                      # Current directory
  appFiles = c(                      # Files to include
    "app.R",       # Main app file
    "tract_data_simplified.rds",     # Data files
    "trend_data.rds",
    "va_co_shape.rds",
    "www/hfv_rgb_logo.png"           # Assets
  ),
  appPrimaryDoc = "app.R"  # Main file
)