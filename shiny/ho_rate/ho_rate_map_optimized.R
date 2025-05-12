library(tidyverse)
library(mapgl)
library(tigris)
library(sf)
library(tidycensus)
library(shiny)
library(bslib)
library(plotly)
library(here)

# UI for the Shiny app
ui <- page_sidebar(
  title = "Virginia Homeownership Explorer",
  sidebar = sidebar(
    width = 300,
    h4("Selected Location"),
    textOutput("selected_tract"),
    textOutput("selected_county"),
    hr(),
    h4("About"),
    p("Click on any census tract to see historical homeownership rates."),
    p("Data source: US Census ACS 5-year estimates, 2010-2023")
  ),
  layout_columns(
    col_widths = c(12),
    card(
      full_screen = TRUE,
      card_header("Homeownership Rate by Census Tract"),
      # Show loading message during render
      div(
        id = "loading-content",
        style = "position: absolute; width: 100%; height: 100%; display: flex; justify-content: center; align-items: center; z-index: 1000;",
        div(
          style = "background-color: rgba(255, 255, 255, 0.8); padding: 20px; border-radius: 5px; text-align: center;",
          h4("Loading map data..."),
          span(class = "spinner-border", role = "status")
        )
      ),
      maplibreOutput("map_id", height = "500px")
    )
  ),
  # Add a new card for the plot below the map
  layout_columns(
    col_widths = c(12),
    card(
      card_header("Homeownership Rate Over Time"),
      plotlyOutput("ho_trend_plot", height = "350px")
    )
  ),
  # Brand with logo
  tags$div(
    style = "position: absolute; bottom: 10px; left: 10px; z-index: 999;",
    tags$img(src = "hfv_rgb_logo.png", height = "auto", width = "150px")
  ),
  # Add JavaScript to handle loading indicator
  tags$script(HTML("
    Shiny.addCustomMessageHandler('hideLoading', function(message) {
      document.getElementById('loading-content').style.display = 'none';
    });
  "))
)

# Server function
server <- function(input, output, session) {
  
  # Implement lazy loading with reactiveVal
  tract_map_data <- reactiveVal(NULL)
  va_counties <- reactiveVal(NULL)
  
  # Load data in a separate reactive process to avoid blocking UI
  observe({
    # Load counties first (smaller file)
    va_counties(read_rds(here("data", "va_co_shape.rds")))
    
    # Then load tract data
    withProgress(message = 'Loading map data...', value = 0, {
      tract_map_data(read_rds(here("shiny", "ho_rate", "tract_map_data.rds")))
      incProgress(1)
    })
    
    # Hide loading indicator when data is ready
    session$sendCustomMessage(type = 'hideLoading', message = list())
  })
  
  # Store the selected tract 
  selected_data <- reactiveVal(NULL)

  # Lazy load trend data only when needed
  trend_data <- reactive({
    read_rds(here("shiny", "ho_rate", "trend_data.rds"))
  })
  
  # Debug helper - print unique jurisdiction values in trend_data
  observe({
    message("Unique jurisdiction values in trend_data: ")
    td <- trend_data()
    jurisdictions <- unique(td$jurisdiction[td$geography == "Jurisdiction"])
    message(paste(jurisdictions, collapse = ", "))
  })
  
 # Render the map
output$map_id <- renderMaplibre({
  # Wait for data to be loaded
  req(tract_map_data(), va_counties())
  
  # Create map object
  m <- maplibre(
    style = mapgl::carto_style("positron"),
    bounds = tract_map_data()
  ) 
  
  # First add tract layer
  m <- m %>% add_fill_layer(
    id = "tract_data",  
    source = tract_map_data(),
    # Viridis palette (colorblind-friendly)
    fill_color = interpolate(
      column = "ho_rate",
      values = c(0, 20, 40, 60, 80, 100),
      stops = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
      na_color = "grey"
    ),
    fill_opacity = 0.8,
    tooltip = "custom_tooltip" # Use custom tooltip for better info display
  )
  
  # Then add county boundaries, but specify that they should appear above the tract layer
  m <- m %>% add_line_layer(
    id = "county_lines",
    source = va_counties(),
    line_color = "lightgrey",
    line_width = 1.5  # Made this slightly thicker for better visibility
  )
  
  # Add legend and other elements
  m <- m %>% add_legend(
    "Homeownership Rate in Virginia (%)",
    values = c(0, 20, 40, 60, 80, 100),
    colors = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")
  )
  
  # Add geocoder
  m <- m %>% add_geocoder_control(
    position = "top-right", 
    placeholder = "Enter an address"
  )
  
  # Return map
  return(m)
})
  
  # Handle tract clicks using the feature_click event 
  observeEvent(input$map_id_feature_click, {
    message("Tract click detected!")
    click_info <- input$map_id_feature_click
    
    # Debug info
    message("Click info structure: ", paste(capture.output(str(click_info)), collapse = "\n"))
    
    if (!is.null(click_info) && !is.null(click_info$properties)) {
      # Extract properties from the clicked feature
      properties <- click_info$properties
      
      # Extract GEOID from properties
      geoid <- properties$GEOID
      # Extract county name from properties
      county_name <- properties$jurisdiction
      tract <- properties$tract

      county_name <- trimws(county_name)
      
      message("Selected GEOID: ", geoid)
      message("Selected county name: ", county_name)
      
      # Load required data
      td <- trend_data()
      
      # Check if county name exists in trend_data
      county_exists <- county_name %in% td$jurisdiction[td$geography == "Jurisdiction"]
      message("County exists in trend_data: ", county_exists)
      
      if (!county_exists) {
        # Try to find the closest match
        message("Looking for similar county names...")
        similar_names <- td$jurisdiction[td$geography == "Jurisdiction"]
        for (name in similar_names) {
          message("Comparing '", county_name, "' with '", name, "'")
        }
      }
      
      # Get tract name or construct it
      tract_name <- tract
      
      # Get tract historical data
      tract_trend <- td %>%
        filter(geography == "Tract") %>%
        filter(GEOID == geoid) %>%
        select(year, geography, ho_rate) %>%
        mutate(level = "Census Tract")
      
      message("Found ", nrow(tract_trend), " rows for tract")
      
      # Get county historical data  
      county_trend <- td %>% 
        filter(geography == "Jurisdiction") %>% 
        filter(jurisdiction == county_name) %>% 
        select(year, geography, ho_rate) %>%
        mutate(level = "Jurisdiction")
      
      message("Found ", nrow(county_trend), " rows for county: ", county_name)
      
      # Get state historical data
      state_trend <- td %>% 
        filter(geography == "State") %>%
        select(year, geography, ho_rate) %>%
        mutate(level = "Virginia")
      
      message("Found ", nrow(state_trend), " rows for state")
      
      # Combine all data
      combined_trend <- bind_rows(
        tract_trend,
        county_trend,
        state_trend
      )
      
      message("Combined data has ", nrow(combined_trend), " rows")
      
      # Create a more complete data structure for your reactive value
      selected_data(list(
        geoid = geoid,
        county_name = county_name,
        tract_name = tract_name,
        trend_data = combined_trend
      ))
      
      message("Selected data updated successfully")
    } else {
      message("Click event doesn't have the expected properties")
    }
  })
  
  # Output tract name
  output$selected_tract <- renderText({
    data <- selected_data()
    if (is.null(data)) {
      "No tract selected"
    } else {
      paste("Selected Tract:", data$tract_name)
    }
  })
  
  # Output county name
  output$selected_county <- renderText({
    data <- selected_data()
    if (is.null(data)) {
      ""
    } else {
      paste("Selected Jurisdiction:", data$county_name)
    }
  })
  
  # Create plot with better structure
  output$ho_trend_plot <- renderPlotly({
    data <- selected_data()
    
    if (is.null(data)) {
      # Return an empty plot with a message using ggplot
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Click on a tract to see historical data", size = 4) +
        theme_void()
      
      ggplotly(empty_plot) %>%
        layout(
          xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
        )
    } else {
      plot_data <- data$trend_data
      
      # Check if we have valid data
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        # Create plot with the combined data
        p <- ggplot(plot_data, 
          aes(
            x = year, 
            y = ho_rate, 
            color = level)) +
          geom_line(size = 1, na.rm = TRUE) +
          geom_point(size = 3, na.rm = TRUE) +
          scale_color_manual(
            values = c("Census Tract" = "#0066CC", "Jurisdiction" = "#FF6600", "Virginia" = "#009933"),
            name = ""
          ) +
          labs(
            title = paste("Homeownership Rate for", data$county_name, "-", data$tract_name)
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.margin = margin(t = 10, b = 10),
            axis.title = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(20,100))
        
        # Convert ggplot to plotly - the simple solution
        ggplotly(p, tooltip = c("x", "y", "color")) %>%
          layout(
            autosize = TRUE,
            margin = list(l = 50, r = 50, b = 80, t = 75, pad = 4),
            legend = list(orientation = "h", y = -0.5, x = 0.5, xanchor = "center")
          )
      } else {
        # No valid data available for the plot
        no_data_plot <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("No historical data available for", data$county_name, "-", data$tract_name), 
                   size = 4) +
          theme_void()
        
        ggplotly(no_data_plot) %>%
          layout(
            xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
          )
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)