# housing_type_module.R

library(shiny)
library(tidyverse)
library(scales)
library(hdatools)  # For scale_fill_hfv(), theme_hfv() and scale_color_hfv()
library(here)
library(ggiraph)  # Replace plotly with ggiraph

# UI function for the module
housingTypeUI <- function(id, 
                          title = "Housing Type Distribution",
                          year_range = 2010:2023,
                          selected_year = 2023) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel(title),
    
    sidebarLayout(
      sidebarPanel(
        # Year filter for all tabs - now with modular parameters
        selectInput(ns("year"), "Select Year:",
                    choices = year_range,
                    selected = selected_year),
        
        # Conditional UI based on selected tab
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'cbsa'"),
          selectInput(ns("cbsa"), "Select Metro Area:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'local'"),
          selectInput(ns("locality"), "Select Locality:", choices = NULL)
        )
      ),
      
      mainPanel(
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("State", girafeOutput(ns("state_plot")), value = "state"),
          tabPanel("Metro Area", girafeOutput(ns("cbsa_plot")), value = "cbsa"),
          tabPanel("Locality", girafeOutput(ns("local_plot")), value = "local")
        )
      )
    )
  )
}

# Server function for the module
housingTypeServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # We'll use the provided data directly
    b25032 <- reactive({
      data
    })
    
    # Process data for state level
    state_housing <- reactive({
      b25032() %>% 
        group_by(year, tenure, type) %>% 
        summarise(estimate = sum(estimate), .groups = "drop") %>% 
        group_by(year, tenure) %>% 
        mutate(percent = estimate/sum(estimate)) %>% 
        group_by(year) %>% 
        mutate(percent_total = estimate/sum(estimate))
    })
    
    # Process data for CBSA level
    cbsa_housing <- reactive({
      b25032() %>% 
        group_by(year, cbsa_title, tenure, type) %>% 
        summarise(estimate = sum(estimate), .groups = "drop") %>% 
        group_by(year, cbsa_title, tenure) %>% 
        mutate(percent = estimate/sum(estimate)) %>% 
        group_by(year, cbsa_title) %>% 
        mutate(percent_total = estimate/sum(estimate))
    })
    
    # Process data for local level
    local_housing <- reactive({
      b25032() %>% 
        group_by(year, name_long, tenure, type) %>% 
        summarise(estimate = sum(estimate), .groups = "drop") %>%
        group_by(year, name_long, tenure) %>% 
        mutate(percent = estimate/sum(estimate)) %>% 
        group_by(year, name_long) %>% 
        mutate(percent_total = estimate/sum(estimate))
    })
    
    # Populate CBSA dropdown
    observe({
      cbsa_choices <- unique(cbsa_housing()$cbsa_title)
      updateSelectInput(session, "cbsa", choices = cbsa_choices, 
                        selected = if("Richmond, VA" %in% cbsa_choices) "Richmond, VA" else cbsa_choices[1])
    })
    
    # Populate locality dropdown
    observe({
      locality_choices <- unique(local_housing()$name_long)
      updateSelectInput(session, "locality", choices = locality_choices,
                        selected = if("Richmond City" %in% locality_choices) "Richmond City" else locality_choices[1])
    })
    
    # Filter data based on inputs
    filtered_state <- reactive({
      req(input$year)
      state_housing() %>% 
        filter(year == input$year)
    })
    
    filtered_cbsa <- reactive({
      req(input$year, input$cbsa)
      cbsa_housing() %>% 
        filter(year == input$year,
               cbsa_title == input$cbsa)
    })
    
    filtered_local <- reactive({
      req(input$year, input$locality)
      local_housing() %>% 
        filter(year == input$year,
               name_long == input$locality)
    })
    
    # Create title text
    title_text <- reactive({
      paste0("Distribution of housing type (", input$year, ")")
    })
    
    # Create a common plotting function using ggiraph
    create_plot <- function(data, subtitle = NULL) {
      # Ensure data is available and has rows
      if(is.null(data) || nrow(data) == 0) {
        return(ggplot() + theme_void() + 
                 labs(title = "No data available", subtitle = subtitle))
      }
      
      # Create a unique ID for each bar
      plot_data <- data %>%
        mutate(tooltip_text = paste0(
          type, ": ", scales::percent(percent_total, accuracy = 0.1),
          "\nCount: ", format(estimate, big.mark = ",")
        ),
        data_id = paste(type, tenure, sep = "_"))
      
      # Create interactive ggplot for ggiraph
      p <- ggplot(plot_data, 
                  aes(x = reorder(type, -percent_total), 
                      y = percent_total, 
                      fill = tenure)) +
        geom_col_interactive(
          aes(tooltip = tooltip_text,
              data_id = data_id),
          position = "dodge") +
        facet_wrap(~tenure) +
        scale_fill_manual(values = c("Owner" = "#011E41", "Renter" = "#40C0C0")) + 
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          title = title_text(),
          subtitle = subtitle,
          x = "",
          y = "Percent of Households"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          strip.text = element_text(size = 12, face = "bold"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)
        )
      
      # Convert to girafe object
      girafe_obj <- girafe(
        ggobj = p,
        width_svg = 10,
        height_svg = 6,
        options = list(
          opts_hover(css = "fill-opacity:0.7;"),
          opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;"),
          opts_sizing(rescale = TRUE)
        )
      )
      
      return(girafe_obj)
    }
    
    # Render the state plot with ggiraph
    output$state_plot <- renderGirafe({
      create_plot(filtered_state(), "Virginia")
    })
    
    # Render the CBSA plot with ggiraph
    output$cbsa_plot <- renderGirafe({
      create_plot(filtered_cbsa(), input$cbsa)
    })
    
    # Render the local plot with ggiraph
    output$local_plot <- renderGirafe({
      create_plot(filtered_local(), input$locality)
    })
  })
}

# app.R

# Sample data creation since we don't have the actual b25032.rds
sample_housing_data <- function() {
  # Create sample data matching your structure
  expand.grid(
    year = 2015:2023,
    cbsa_title = c("Richmond, VA", "Washington, DC", "Virginia Beach, VA"),
    name_long = c("Richmond City", "Henrico County", "Chesterfield County"),
    tenure = c("Owner", "Renter"),
    type = c("Single Family Detached", "Single Family Attached", 
             "2-4 Units", "5+ Units", "Mobile Home")
  ) %>%
    mutate(estimate = sample(100:10000, n(), replace = TRUE))
}

# Define UI
ui <- fluidPage(
  titlePanel("Virginia Housing Analysis"),
  
  # Using the module with custom settings
  housingTypeUI("housing_module1", 
                title = "Housing Type Distribution by Geography",
                year_range = 2015:2023,
                selected_year = 2022)
)

# Define server
server <- function(input, output, session) {
  # Use sample data or load your actual data
  housing_data <- reactive({
    tryCatch({
      readRDS(here::here("data", "rds", "b25032.rds"))
    }, error = function(e) {
      # Fallback to sample data if file not found
      message("Using sample data: ", e$message)
      sample_housing_data()
    })
  })
  
  # Initialize the housing module
  housingTypeServer("housing_module1", data = housing_data())
}

# Run the app
shinyApp(ui = ui, server = server)