library(shiny)
library(tidyverse)
library(scales)
library(here)

# Define UI
ui <- fluidPage(
  titlePanel("Poverty Rate Analysis in Virginia"),
  
  # Main tabs for selecting data type
  navbarPage("Poverty Analysis",
             # First main tab - Poverty by Race/Ethnicity
             tabPanel("Poverty by Race/Ethnicity",
                      # Subtabs for geographic levels
                      tabsetPanel(
                        # State Tab - No filters needed
                        tabPanel("State", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     # No filters for state level
                                     p("Virginia state-level poverty rates by race and ethnicity.")
                                   ),
                                   mainPanel(
                                     plotOutput("race_state_plot", height = "600px", 
                                                hover = hoverOpts("race_state_hover", 
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        ),
                        
                        # CBSA Tab with CBSA filter
                        tabPanel("CBSA",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("race_cbsa_select", "Select CBSA:", choices = NULL)
                                   ),
                                   mainPanel(
                                     plotOutput("race_cbsa_plot", height = "600px",
                                                hover = hoverOpts("race_cbsa_hover",
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        ),
                        
                        # Locality Tab with locality filter
                        tabPanel("Locality",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("race_locality_select", "Select Locality:", choices = NULL)
                                   ),
                                   mainPanel(
                                     plotOutput("race_locality_plot", height = "600px",
                                                hover = hoverOpts("race_locality_hover",
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        )
                      )
             ),
             
             # Second main tab - Poverty by Age
             tabPanel("Poverty by Age",
                      # Subtabs for geographic levels
                      tabsetPanel(
                        # State Tab - No filters needed
                        tabPanel("State", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     # No filters for state level
                                     p("Virginia state-level poverty rates by age groups.")
                                   ),
                                   mainPanel(
                                     plotOutput("age_state_plot", height = "600px",
                                                hover = hoverOpts("age_state_hover",
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        ),
                        
                        # CBSA Tab with CBSA filter
                        tabPanel("CBSA",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("age_cbsa_select", "Select CBSA:", choices = NULL)
                                   ),
                                   mainPanel(
                                     plotOutput("age_cbsa_plot", height = "600px",
                                                hover = hoverOpts("age_cbsa_hover",
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        ),
                        
                        # Locality Tab with locality filter
                        tabPanel("Locality",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("age_locality_select", "Select Locality:", choices = NULL)
                                   ),
                                   mainPanel(
                                     plotOutput("age_locality_plot", height = "600px",
                                                hover = hoverOpts("age_locality_hover",
                                                                  delay = 100, delayType = "debounce"))
                                   )
                                 )
                        )
                      )
             )
  ),
  
  # Add tooltips for hover functionality
  wellPanel(
    p("Hover over points to see details:"),
    verbatimTextOutput("hover_info")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load poverty by race data
  poverty_race <- reactive({
    read_rds(here("data", "poverty_race.rds"))
  })
  
  # Load poverty by age data
  poverty_age <- reactive({
    read_rds(here("data", "poverty_age.rds"))
  })
  
  # Define race colors
  race_colors <- reactive({
    req(race_state_data())
    
    # First, get the actual unique race values from your data
    race_levels <- unique(race_state_data()$race)
    
    # Create color vector without names first
    color_values <- c("#E0592A", "#259591", "#011E41", "#40C0C0", 
                      "#FFC658", "#FF7276", "#8B85CA", "#B1005F")
    
    # Then create a named vector matching your actual data values
    setNames(color_values[1:length(race_levels)], race_levels)
  })
  
  # Define age colors
  age_colors <- c(
    "17 years and under" = "#FFC658", # Desert
    "18 to 24 years" = "#E0592A",     # HousingX Orange
    "25 to 34 years" = "#259591",     # Grass
    "35 to 44 years" = "#40C0C0",     # Sky
    "45 to 54 years" = "#8B85CA",     # Lilac
    "55 to 64 years" = "#B1005F",     # Berry
    "65 years and over" = "#FF7276"   # HousingX Red
  )
  
  #----- RACE DATA PROCESSING -----#
  
  # Process race state data
  race_state_data <- reactive({
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
  
  # Process race CBSA data
  race_cbsa_data <- reactive({
    req(poverty_race())
    
    pov_race_cbsa <- poverty_race() %>% 
      group_by(year, race, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace)) %>% 
      mutate(rate = estimate/totalrace) %>% 
      ungroup()
    
    # Update CBSA choices in the UI
    cbsa_choices <- unique(pov_race_cbsa$cbsa_title)
    updateSelectInput(session, "race_cbsa_select", choices = cbsa_choices, 
                      selected = ifelse("Richmond, VA" %in% cbsa_choices, "Richmond, VA", cbsa_choices[1]))
    
    pov_race_cbsa
  })
  
  # Filter race CBSA data based on selection
  filtered_race_cbsa_data <- reactive({
    req(race_cbsa_data(), input$race_cbsa_select)
    
    cbsa <- race_cbsa_data() %>% 
      filter(cbsa_title == input$race_cbsa_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    cbsa_summary <- cbsa %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    cbsa %>%
      mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  })
  
  # Process race locality data
  race_locality_data <- reactive({
    req(poverty_race())
    
    pov_race_local <- poverty_race()
    
    # Update locality choices in the UI
    locality_choices <- unique(pov_race_local$locality)
    updateSelectInput(session, "race_locality_select", choices = locality_choices, 
                      selected = ifelse("Richmond city" %in% locality_choices, "Richmond city", locality_choices[1]))
    
    pov_race_local
  })
  
  # Filter race locality data based on selection
  filtered_race_locality_data <- reactive({
    req(race_locality_data(), input$race_locality_select)
    
    local <- race_locality_data() %>% 
      filter(locality == input$race_locality_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    local_summary <- local %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    local %>%
      mutate(race_ordered = factor(race, levels = local_summary$race))
  })
  
  #----- AGE DATA PROCESSING -----#
  
  # Process age state data
  age_state_data <- reactive({
    req(poverty_age())
    
    poverty_age() %>% 
      group_by(year, age, age_group) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage) %>% 
      ungroup()
  })
  
  # Process age CBSA data
  age_cbsa_data <- reactive({
    req(poverty_age())
    
    # Add debugging
    print("Processing age CBSA data")
    
    pov_age_cbsa <- poverty_age() %>% 
      group_by(year, age, age_group, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage) %>% 
      ungroup()
    
    # Update CBSA choices in the UI
    cbsa_choices <- unique(pov_age_cbsa$cbsa_title)
    
    # Print number of choices for debugging
    print(paste("Number of CBSA choices:", length(cbsa_choices)))
    
    updateSelectInput(session, "age_cbsa_select", choices = cbsa_choices, 
                      selected = ifelse("Richmond, VA" %in% cbsa_choices, "Richmond, VA", cbsa_choices[1]))
    
    pov_age_cbsa
  })
  
  # Filter age CBSA data based on selection
  filtered_age_cbsa_data <- reactive({
    req(age_cbsa_data(), input$age_cbsa_select)
    
    # Add debugging
    print(paste("Filtering for CBSA:", input$age_cbsa_select))
    
    filtered_data <- age_cbsa_data() %>% 
      filter(cbsa_title == input$age_cbsa_select)
    
    # Print dimensions for debugging
    print(paste("Filtered data dimensions:", nrow(filtered_data), "rows,", ncol(filtered_data), "columns"))
    
    filtered_data
  })
  
  # Process age locality data
  age_locality_data <- reactive({
    req(poverty_age())
    
    # Update locality choices in the UI
    locality_choices <- unique(poverty_age()$locality)
    updateSelectInput(session, "age_locality_select", choices = locality_choices, 
                      selected = ifelse("Richmond city" %in% locality_choices, "Richmond city", locality_choices[1]))
    
    poverty_age()
  })
  
  # Filter age locality data based on selection
  filtered_age_locality_data <- reactive({
    req(age_locality_data(), input$age_locality_select)
    
    age_locality_data() %>% 
      filter(locality == input$age_locality_select) %>%
      group_by(year, age, age_group) %>%
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>%
      mutate(rate = estimate/totalage) %>%
      ungroup()
  })
  
  #----- RACE PLOTS -----#
  
  # Race State Plot
  output$race_state_plot <- renderPlot({
    req(race_state_data(), race_colors())
    state <- race_state_data()
    
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
  
  # Race CBSA Plot
  output$race_cbsa_plot <- renderPlot({
    req(filtered_race_cbsa_data(), race_colors())
    cbsa <- filtered_race_cbsa_data()
    
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
      labs(title = paste0("Poverty Rate by Race and Ethnicity - ", input$race_cbsa_select))
  })
  
  # Race Locality Plot
  output$race_locality_plot <- renderPlot({
    req(filtered_race_locality_data(), race_colors())
    local <- filtered_race_locality_data()
    
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
      labs(title = paste0("Poverty Rate by Race and Ethnicity - ", input$race_locality_select))
  })
  
  #----- AGE PLOTS -----#
  
  # Age State Plot
  output$age_state_plot <- renderPlot({
    req(age_state_data())
    state <- age_state_data()
    
    # Get latest year data for labels
    latest_year <- max(state$year)
    latest_data <- state %>% filter(year == latest_year)
    
    ggplot(state,
           aes(
             x = year,
             y = rate,
             color = age,
             group = age
           )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~age_group, nrow = 1) +
      scale_color_manual(values = age_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format()) +
      # Show fewer years on x-axis
      scale_x_discrete(breaks = seq(min(state$year), max(state$year), by = 5)) +
      # Clean up the appearance
      theme_minimal() +
      theme(
        # Remove redundant legend
        legend.position = "none",
        # Increase facet title size
        strip.text = element_text(size = 12, face = "bold"),
        # More space between facets
        panel.spacing = unit(1.5, "lines"),
        # Remove minor gridlines
        panel.grid.minor = element_blank(),
        # Add more margin space
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        # Increase title text size
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank()
      ) +
      labs(title = "Poverty Rate by Age - Virginia")
  })
  
  # Age CBSA Plot
  output$age_cbsa_plot <- renderPlot({
    req(filtered_age_cbsa_data())
    cbsa <- filtered_age_cbsa_data()
    
    # Add try/catch with print for debugging
    tryCatch({
      # Print column names to debug
      print("Column names in age CBSA data:")
      print(names(cbsa))
      
      # Get latest year data for labels
      latest_year <- max(cbsa$year, na.rm = TRUE)
      latest_data <- cbsa %>% filter(year == latest_year)
      
      p <- ggplot(cbsa,
                  aes(
                    x = year,
                    y = rate,
                    color = age,
                    group = age
                  )) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        # Add labels for latest values
        geom_text(data = latest_data, 
                  aes(label = scales::percent(rate, accuracy = 0.1)),
                  hjust = -0.3, vjust = 0.5) +
        facet_wrap(~age_group, nrow = 1) +
        scale_color_manual(values = age_colors) +
        # Format y-axis as percentage
        scale_y_continuous(labels = scales::percent_format()) +
        # Show fewer years on x-axis
        scale_x_discrete(breaks = seq(min(cbsa$year), max(cbsa$year), by = 5)) +
        # Clean up the appearance
        theme_minimal() +
        theme(
          # Remove redundant legend
          legend.position = "none",
          # Increase facet title size
          strip.text = element_text(size = 12, face = "bold"),
          # More space between facets
          panel.spacing = unit(1.5, "lines"),
          # Remove minor gridlines
          panel.grid.minor = element_blank(),
          # Add more margin space
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          # Increase title text size
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_blank()
        ) +
        labs(title = paste0("Poverty Rate by Age - ", input$age_cbsa_select))
      
      print("Plot created successfully")
      return(p)
    }, error = function(e) {
      # Print error details
      print(paste("Error in age CBSA plot:", e$message))
      
      # Return a blank plot with error message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error creating plot:", e$message)) +
        theme_void()
    })
  })
  
  # Age Locality Plot
  output$age_locality_plot <- renderPlot({
    req(filtered_age_locality_data())
    local <- filtered_age_locality_data()
    
    # Get latest year data for labels
    latest_year <- max(local$year)
    latest_data <- local %>% filter(year == latest_year)
    
    ggplot(local,
           aes(
             x = year,
             y = rate,
             color = age,
             group = age
           )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~age_group, nrow = 1) +
      scale_color_manual(values = age_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format()) +
      # Show fewer years on x-axis
      scale_x_discrete(breaks = seq(min(local$year), max(local$year), by = 5)) +
      # Clean up the appearance
      theme_minimal() +
      theme(
        # Remove redundant legend
        legend.position = "none",
        # Increase facet title size
        strip.text = element_text(size = 12, face = "bold"),
        # More space between facets
        panel.spacing = unit(1.5, "lines"),
        # Remove minor gridlines
        panel.grid.minor = element_blank(),
        # Add more margin space
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        # Increase title text size
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_blank()
      ) +
      labs(title = paste0("Poverty Rate by Age - ", input$age_locality_select))
  })
  
  #----- HOVER FUNCTIONALITY -----#
  
  # Handle hover info for all plots
  output$hover_info <- renderPrint({
    # Check which plot is being hovered over
    hover_data <- NULL
    
    if (!is.null(input$race_state_hover)) {
      hover_data <- nearPoints(race_state_data(), input$race_state_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("Race/Ethnicity: ", hover_data$race, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    } else if (!is.null(input$race_cbsa_hover)) {
      hover_data <- nearPoints(filtered_race_cbsa_data(), input$race_cbsa_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("CBSA: ", hover_data$cbsa_title, "\n")
        cat("Race/Ethnicity: ", hover_data$race, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    } else if (!is.null(input$race_locality_hover)) {
      hover_data <- nearPoints(filtered_race_locality_data(), input$race_locality_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("Locality: ", hover_data$locality, "\n")
        cat("Race/Ethnicity: ", hover_data$race, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    } else if (!is.null(input$age_state_hover)) {
      hover_data <- nearPoints(age_state_data(), input$age_state_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("Age Group: ", hover_data$age, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    } else if (!is.null(input$age_cbsa_hover)) {
      hover_data <- nearPoints(filtered_age_cbsa_data(), input$age_cbsa_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("CBSA: ", hover_data$cbsa_title, "\n")
        cat("Age Group: ", hover_data$age, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    } else if (!is.null(input$age_locality_hover)) {
      hover_data <- nearPoints(filtered_age_locality_data(), input$age_locality_hover, threshold = 5, maxpoints = 1)
      if (nrow(hover_data) > 0) {
        cat("Locality: ", hover_data$locality, "\n")
        cat("Age Group: ", hover_data$age, "\n")
        cat("Year: ", hover_data$year, "\n")
        cat("Poverty Rate: ", scales::percent(hover_data$rate, accuracy = 0.1), "\n")
        cat("Number in Poverty: ", format(hover_data$estimate, big.mark = ","), "\n")
      }
    }
    
    if (is.null(hover_data) || nrow(hover_data) == 0) {
      cat("Hover over a point to see details")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)