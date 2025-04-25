library(tidyverse)
library(tigris)
library(leaflet)
library(here)
library(sf)

# Load data - use simplified boundaries
options(tigris_use_cache = TRUE)
states <- states(cb = TRUE) # Using cb=TRUE for faster, simplified boundaries

# Load your homeownership data
b25003_state <- read_rds(here("data/b25003_state.rds")) |> 
  mutate(ho_rate = est_owner/est_all) %>% 
  mutate(GEOID = fips)

b25003_state_geo <- states %>% 
  left_join(b25003_state, by = "GEOID") %>%
  # Transform to WGS84
  st_transform(4326) %>%
  # Simplify geometry to reduce size
  st_simplify(dTolerance = 0.01)

# Create a simple app with the map
ui <- fluidPage(
  titlePanel("U.S. States Homeownership Rates"),
  
  mainPanel(
    leafletOutput("map", height = "700px")
  )
)

# Add debugging to check data before rendering
server <- function(input, output, session) {
  
  # Print data summary to console for debugging
  print(summary(b25003_state_geo$ho_rate))
  print(head(b25003_state_geo))
  
  # Create color palette
  pal <- colorBin(
    palette = "Blues",
    domain = b25003_state_geo$ho_rate,
    bins = seq(0.5, 0.8, by = 0.05)
  )
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet(b25003_state_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(ho_rate),
        weight = 1,
        opacity = 1,
        color = "#333333",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME, ": ", round(ho_rate * 100, 1), "% homeownership rate"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~ho_rate,
        title = "Homeownership Rate",
        opacity = 0.7,
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
      )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)