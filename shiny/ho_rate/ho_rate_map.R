library(tidyverse)
library(mapgl)
library(tigris)
library(sf)
library(tidycensus)
library(shiny)
library(bslib)

# Get simplified state boundaries
va_counties <- counties(state = "VA") %>%  # Use lowest resolution
  select(GEOID, NAME) # Keep only necessary columns

write_rds(va_counties, "data/va_co_shape.rds")

# Read and prepare data more efficiently
b25003_state <- readRDS("data/b25003_state.rds") |>  
  filter(year == 2023,
         race == "All") |> 
  mutate(GEOID = fips,
         ho_rate = est_owner/est_all)

state_us_ho <- states_us |> 
  left_join(
    b25003_state, 
    by = "GEOID")


va_homeownership <- get_acs(
  geography = "tract",
  table = "B25003",
  state = "VA",
  year = 2022, 
  geometry = TRUE, 
  resolution = "5m"
) 

# Pull the table variables, excluding Puerto Rico.
b25003_defns <- load_variables(2023, "acs5") %>%
  filter(str_detect(name, "B25003_")) |> 
  separate(label, into = c("est", "total", "tenure")) |> 
  mutate(
    tenure = case_when(
    tenure == "Owner" ~ "Homeowner",
    tenure == "Renter" ~ "Renter",
    TRUE ~ "All"),
    variable = name)
    

va_homeownership_clean <- va_homeownership |> 
  left_join(b25003_defns, by = "variable") |> 
  select(NAME, estimate, tenure) |> 
  pivot_wider(
    names_from = tenure,
    values_from = estimate
  ) |> 
  mutate(ho_rate = ((Homeowner/All)*100)) |> 
  mutate(ho_rate = ifelse(ho_rate == 0, NA, ho_rate)) |> 
  separate(NAME, into = c("tract", "jurisdiction", "state"), sep = ";") |> 
  group_by(jurisdiction) |> 
  mutate(total = sum(All),
         total_homeowner = sum(Homeowner)) |> 
  mutate(local_rate = ((total_homeowner/total)*100)) |> 
  ungroup() |> 
  mutate(custom_tooltip = case_when(
    is.na(ho_rate) ~ paste0(tract,
                            "<br><b>Jurisdiction: </b>", jurisdiction,
                            "<br><b>Homeownership Rate:</b> No homeowners here",
                            "<br><b>Jurisdiction Homeownership Rate: </b>", round(local_rate, 1), "%"),
    TRUE ~ paste0(tract,
                  "<br><b>Jurisdiction: </b>", jurisdiction,
                  "<br><b>Homeownership Rate: </b>", round(ho_rate, 1), "%",
                  "<br><b>Jurisdiction Homeownership Rate: </b>", round(local_rate, 1), "%")))





# Create the map with improved color gradation
map <- maplibre(
  style = carto_style("positron"),
  bounds = va_homeownership_clean
) |> 
  add_fill_layer(id = "tract_data",
                 source = va_homeownership_clean,
                 # Viridis palette (colorblind-friendly)
                 fill_color = interpolate(
                   column = "ho_rate",
                   values = c(0, 20, 40, 60, 80, 100),
                   stops = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
                   na_color = "grey"
                 ),
                 fill_opacity = 0.8,
                 tooltip = "custom_tooltip",
                 hover_options = list(
                   fill_color = "darkblue",
                   fill_opacity = 1
                 )) |> 
                  add_line_layer(
                    id = "county",
                    source = va_counties,
                    line_color = "lightgrey"
                  ) |> 
  add_legend(
    "Homeownership Rate in Virginia (%)",
    values = c(0, 20, 40, 60, 80, 100),
    colors =  c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")) |> 
  add_geocoder_control(position = "top-right", placeholder = "Enter an address") 


# For use in a Shiny app
# In your ui.R or app.R file:
ui <- fluidPage(
  tags$div(
    style = "position: relative;",
    # The map
    maplibreOutput("map_id"),
    # Logo overlay
    tags$div(
      style = "position: absolute; bottom: 10px; left: 10px; z-index: 999;",
      tags$img(src = "hfv_rgb_logo.png", height = "auto", width = "150px")
    )
  ),
  # Debugging section
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; margin-top: 20px; border-radius: 5px;",
    h4("Debugging Information"),
    verbatimTextOutput("debug_info")
  )
)

# In your server.R or app.R:
# Add this to your server function to debug the image path issue
server <- function(input, output) {
  output$map_id <- renderMaplibre({
    map
  })
  
  # Add this debugging output
  output$debug_info <- renderText({
    # List files in the www directory to see what's available
    www_files <- list.files("www", recursive = TRUE)
    
    # Check if our specific file exists
    logo_exists <- "hfv_rgb_logo.png" %in% www_files
    
    # Return debugging information
    paste(
      "Files in www directory:", paste(www_files, collapse = ", "),
      "\nLogo file exists:", logo_exists,
      "\nWorking directory:", getwd()
    )
  })
}


shinyApp(ui = ui, server = server)