library(rsconnect)

# Create a manifest file for deployment
rsconnect::writeManifest(
  appDir = ".",                      # Current directory
  appFiles = c(                      # Files to include
    "ho_rate_map_optimized.R",       # Main app file
    "tract_map_data.rds",            # Data files
    "trend_data.rds",
    "www/hfv_rgb_logo.png"           # Assets
  ),
  appPrimaryDoc = "ho_rate_map_optimized.R",  # Main file
  contentCategory = "application",
  appMode = "shiny",
  appTitle = "Virginia Homeownership Explorer"
)

# After creating the manifest, you can deploy with:
# rsconnect::deployApp(".", account = "your-account", server = "connect.posit.cloud")