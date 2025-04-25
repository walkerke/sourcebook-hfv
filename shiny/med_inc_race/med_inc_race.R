# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(here)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Median Household Income by Race/Ethnicity"),
  
  sidebarLayout(
    sidebarPanel(
      # Tab selection
      tabsetPanel(
        id = "geo_tabs",
        tabPanel("State", 
                 selectInput("state_select", "Select State:", choices = NULL),
                 selectInput("state_year", "Select Year:", choices = NULL),
                 checkboxInput("state_adjusted", "Show Inflation-Adjusted Values", FALSE)
        ),
        tabPanel("Metro Area", 
                 selectInput("cbsa_select", "Select Metro Area:", choices = NULL),
                 selectInput("cbsa_year", "Select Year:", choices = NULL),
                 checkboxInput("cbsa_adjusted", "Show Inflation-Adjusted Values", FALSE)
        ),
        tabPanel("Locality", 
                 selectInput("locality_select", "Select Locality:", choices = NULL),
                 selectInput("locality_year", "Select Year:", choices = NULL),
                 checkboxInput("locality_adjusted", "Show Inflation-Adjusted Values", FALSE)
        )
      ),
      
      # Download button
      downloadButton("download_data", "Download Data")
    ),
    
    mainPanel(
      plotOutput("income_plot", height = "600px")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Load data when app starts
  data_loaded <- reactiveVal(FALSE)
  
  locality_data <- reactive({
    # Use here() package for file paths
    read_rds(here("data", "b19013_locality.rds"))
  })
  
  cbsa_data <- reactive({
    read_rds(here("data", "b19013_cbsa.rds"))
  })
  
  state_data <- reactive({
    read_rds(here("data", "b19013_state.rds"))
  })
  
  # Determine max y-axis value for consistency
  max_income <- reactive({
    # Calculate maximum across all datasets to maintain consistent scale
    max_val <- max(
      max(state_data()$estimate, na.rm = TRUE),
      max(state_data()$adjusted, na.rm = TRUE),
      max(cbsa_data()$estimate, na.rm = TRUE),
      max(cbsa_data()$adjusted, na.rm = TRUE),
      max(locality_data()$estimate, na.rm = TRUE),
      max(locality_data()$adjusted, na.rm = TRUE)
    )
    
    # Round up to nearest 10,000 for a clean scale
    ceiling(max_val / 10000) * 10000
  })
  
  # Update dropdown choices when data is loaded
  observe({
    req(locality_data(), cbsa_data(), state_data())
    
    # Update state choices
    states <- unique(state_data()$state)
    updateSelectInput(session, "state_select", choices = states, selected = "Virginia")
    
    # Update year choices for state
    state_years <- unique(state_data()$year)
    updateSelectInput(session, "state_year", choices = state_years, selected = max(state_years))
    
    # Update CBSA choices
    cbsas <- unique(cbsa_data()$CBSA)
    updateSelectInput(session, "cbsa_select", choices = cbsas, selected = "Richmond, VA Metro Area")
    
    # Update year choices for CBSA
    cbsa_years <- unique(cbsa_data()$year)
    updateSelectInput(session, "cbsa_year", choices = cbsa_years, selected = max(cbsa_years))
    
    # Update locality choices
    localities <- unique(locality_data()$locality)
    updateSelectInput(session, "locality_select", choices = localities, selected = "Richmond city")
    
    # Update year choices for locality
    locality_years <- unique(locality_data()$year)
    updateSelectInput(session, "locality_year", choices = locality_years, selected = max(locality_years))
    
    data_loaded(TRUE)
  })
  
  # Define color scheme
  race_colors <- reactive({
    # Get all unique race values across all datasets
    all_races <- unique(c(
      state_data()$race,
      cbsa_data()$race,
      locality_data()$race
    ))
    
    # Create color vector
    color_values <- c("#E0592A", "#259591", "#011E41", "#40C0C0", 
                      "#FFC658", "#FF7276", "#8B85CA", "#B1005F")
    
    # Create named vector for colors
    setNames(color_values[1:length(all_races)], all_races)
  })
  
  # Theme function for plots
  theme_hfv <- function() {
    theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#333333"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#f0f0f0"),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(0.5, 2, 0.5, 0.5, "cm")
      )
  }
  
  # Function to create the plot
  create_plot <- function(data, use_adjusted = FALSE) {
    # Select which value to plot based on checkbox
    value_col <- if(use_adjusted) "adjusted" else "estimate"
    
    # Filter out NA values
    plot_data <- data %>% drop_na()
    
    # Create the plot
    ggplot(plot_data,
           aes(
             x = reorder(race, .data[[value_col]]),
             y = .data[[value_col]],
             fill = race)) +
      geom_col() +
      # Add the value labels at the end of each bar
      geom_text(aes(label = scales::dollar(.data[[value_col]]), color = race),
                hjust = -0.2) +
      # Make the text colors match the fill colors
      scale_color_manual(values = race_colors()) +
      # Set the fill colors
      scale_fill_manual(values = race_colors()) +
      # Extend the plot area to make room for labels
      coord_flip(clip = "off") +
      # Format y-axis with dollar signs and set consistent scale
      scale_y_continuous(
        labels = scales::dollar_format()) +
      # Apply custom theme
      theme_hfv() +
      # Add labels
      labs(
        x = NULL,
        y = if(use_adjusted) "Median Household Income (Inflation-Adjusted)" else "Median Household Income",
        title = paste("Median Household Income by Race/Ethnicity")
      )
  }
  
  # Get filtered data based on selected tab and inputs
  filtered_data <- reactive({
    req(data_loaded())
    
    current_tab <- input$geo_tabs
    
    if (current_tab == "State") {
      data <- state_data() %>%
        filter(
          state == input$state_select,
          year == input$state_year
        )
      
      title_suffix <- paste(input$state_select, input$state_year)
    } else if (current_tab == "Metro Area") {
      data <- cbsa_data() %>%
        filter(
          CBSA == input$cbsa_select,
          year == input$cbsa_year
        )
      
      title_suffix <- paste(input$cbsa_select, input$cbsa_year)
    } else {  # Locality
      data <- locality_data() %>%
        filter(
          locality == input$locality_select,
          year == input$locality_year
        )
      
      title_suffix <- paste(input$locality_select, input$locality_year)
    }
    
    list(data = data, title_suffix = title_suffix)
  })
  
  # Render the plot
  output$income_plot <- renderPlot({
    req(filtered_data())
    
    current_tab <- input$geo_tabs
    use_adjusted <- FALSE
    
    if (current_tab == "State") {
      use_adjusted <- input$state_adjusted
    } else if (current_tab == "Metro Area") {
      use_adjusted <- input$cbsa_adjusted
    } else {  # Locality
      use_adjusted <- input$locality_adjusted
    }
    
    data <- filtered_data()$data
    title_suffix <- filtered_data()$title_suffix
    
    p <- create_plot(data, use_adjusted)
    p + labs(title = paste("Median Household Income by Race/Ethnicity -", title_suffix))
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      current_tab <- input$geo_tabs
      suffix <- filtered_data()$title_suffix
      paste0("median_income_", gsub(" ", "_", suffix), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data()$data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)