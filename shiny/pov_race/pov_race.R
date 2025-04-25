library(shiny)
library(tidyverse)
library(scales)
library(here)

# Define UI
ui <- fluidPage(
  titlePanel("Poverty Rate by Race and Ethnicity"),
  
  tabsetPanel(
    # State Tab - No filters needed
    tabPanel("State", 
             sidebarLayout(
               sidebarPanel(
                 # No filters for state level
                 p("Virginia state-level poverty rates by race and ethnicity.")
               ),
               mainPanel(
                 plotOutput("state_plot", height = "600px")
               )
             )
    ),
    
    # CBSA Tab with CBSA filter
    tabPanel("CBSA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cbsa_select", "Select CBSA:", choices = NULL)
               ),
               mainPanel(
                 plotOutput("cbsa_plot", height = "600px")
               )
             )
    ),
    
    # Locality Tab with locality filter
    tabPanel("Locality",
             sidebarLayout(
               sidebarPanel(
                 selectInput("locality_select", "Select Locality:", choices = NULL)
               ),
               mainPanel(
                 plotOutput("locality_plot", height = "600px")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data
  poverty_race <- reactive({
    read_rds(here("data", "poverty_race.rds"))
  })
  
  # Process state data
  state_data <- reactive({
    req(poverty_race())
    
    pov_race_state <- poverty_race() %>% 
      group_by(year, race) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace)) %>% 
      mutate(rate = estimate/totalrace) %>% 
      ungroup()
    
    # Calculate the mean rate for each race to help determine order of facets
    state_summary <- pov_race_state %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    pov_race_state %>%
      mutate(race_ordered = factor(race, levels = state_summary$race))
  })
  
  # Define color palette based on unique race values
  race_colors <- reactive({
    req(state_data())
    
    # First, get the actual unique race values from your data
    race_levels <- unique(state_data()$race)
    
    # Create color vector without names first
    color_values <- c("#E0592A", "#259591", "#011E41", "#40C0C0", 
                      "#FFC658", "#FF7276", "#8B85CA", "#B1005F")
    
    # Then create a named vector matching your actual data values
    setNames(color_values[1:length(race_levels)], race_levels)
  })
  
  # Process CBSA data
  cbsa_data <- reactive({
    req(poverty_race())
    
    pov_race_cbsa <- poverty_race() %>% 
      group_by(year, race, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace)) %>% 
      mutate(rate = estimate/totalrace) %>% 
      ungroup()
    
    # Update CBSA choices in the UI
    cbsa_choices <- unique(pov_race_cbsa$cbsa_title)
    updateSelectInput(session, "cbsa_select", choices = cbsa_choices, 
                      selected = ifelse("Richmond, VA" %in% cbsa_choices, "Richmond, VA", cbsa_choices[1]))
    
    pov_race_cbsa
  })
  
  # Filter CBSA data based on selection
  filtered_cbsa_data <- reactive({
    req(cbsa_data(), input$cbsa_select)
    
    cbsa <- cbsa_data() %>% 
      filter(cbsa_title == input$cbsa_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    cbsa_summary <- cbsa %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    cbsa %>%
      mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  })
  
  # Process locality data
  locality_data <- reactive({
    req(poverty_race())
    
    pov_race_local <- poverty_race()
    
    # Update locality choices in the UI
    locality_choices <- unique(pov_race_local$locality)
    updateSelectInput(session, "locality_select", choices = locality_choices, 
                      selected = ifelse("Richmond city" %in% locality_choices, "Richmond city", locality_choices[1]))
    
    pov_race_local
  })
  
  # Filter locality data based on selection
  filtered_locality_data <- reactive({
    req(locality_data(), input$locality_select)
    
    local <- locality_data() %>% 
      filter(locality == input$locality_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    local_summary <- local %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    local %>%
      mutate(race_ordered = factor(race, levels = local_summary$race))
  })
  
  # State Plot
  output$state_plot <- renderPlot({
    req(state_data(), race_colors())
    state <- state_data()
    
    # Get latest year data for labels
    latest_year <- max(state$year)
    latest_data <- state %>% filter(year == latest_year)
    
    ggplot(state,
           aes(
             x = year,
             y = rate,
             color = race_ordered,
             group = race_ordered)) +
      geom_line(linewidth = 1) +  # Make lines thicker
      geom_point(size = 2) +      # Make points larger
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns
      scale_color_manual(values = race_colors()) +
      # Better x-axis formatting - show fewer years
      scale_x_discrete(breaks = seq(min(state$year), max(state$year), by = 5)) +  
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +  # Start y-axis at 0
      # Improve theme elements
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
        legend.position = "none",  # Remove redundant legend
        panel.spacing = unit(1.5, "lines"),  # More space between facets
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.title = element_blank() # More margin space
      ) +
      labs(title = "Poverty Rate by Race and Ethnicity - Virginia")
  })
  
  # CBSA Plot
  output$cbsa_plot <- renderPlot({
    req(filtered_cbsa_data(), race_colors())
    cbsa <- filtered_cbsa_data()
    
    # Get latest year data for labels
    latest_year <- max(cbsa$year)
    latest_data <- cbsa %>% filter(year == latest_year)
    
    ggplot(cbsa,
           aes(
             x = year,
             y = rate,
             color = race_ordered,
             group = race_ordered)) +
      geom_line(linewidth = 1) +  # Make lines thicker
      geom_point(size = 2) +      # Make points larger
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns
      scale_color_manual(values = race_colors()) +
      # Better x-axis formatting - show fewer years
      scale_x_discrete(breaks = seq(min(cbsa$year), max(cbsa$year), by = 5)) +  
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +  # Start y-axis at 0
      # Improve theme elements
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
        legend.position = "none",  # Remove redundant legend
        panel.spacing = unit(1.5, "lines"),  # More space between facets
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.title = element_blank() # More margin space
      ) +
      labs(title = paste0("Poverty Rate by Race and Ethnicity - ", input$cbsa_select))
  })
  
  # Locality Plot
  output$locality_plot <- renderPlot({
    req(filtered_locality_data(), race_colors())
    local <- filtered_locality_data()
    
    # Get latest year data for labels
    latest_year <- max(local$year)
    latest_data <- local %>% filter(year == latest_year)
    
    ggplot(local,
           aes(
             x = year,
             y = rate,
             color = race_ordered,
             group = race_ordered)) +
      geom_line(linewidth = 1) +  # Make lines thicker
      geom_point(size = 2) +      # Make points larger
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~race_ordered, ncol = 3) +  # Use 3 columns
      scale_color_manual(values = race_colors()) +
      # Better x-axis formatting - show fewer years
      scale_x_discrete(breaks = seq(min(local$year), max(local$year), by = 5)) +  
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +  # Start y-axis at 0
      # Improve theme elements
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12, face = "bold"),  # Larger facet titles
        legend.position = "none",  # Remove redundant legend
        panel.spacing = unit(1.5, "lines"),  # More space between facets
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        plot.title = element_text(size = 14, face = "bold"),  # Larger plot title
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.title = element_blank() # More margin space
      ) +
      labs(title = paste0("Poverty Rate by Race and Ethnicity - ", input$locality_select))
  })
}

# Run the app
shinyApp(ui = ui, server = server)