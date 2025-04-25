library(shiny)
library(tidyverse)
library(scales)

# Assuming theme_hfv is defined elsewhere in your code
# If not, define it here or replace with another theme like theme_minimal()
# For this example, I'll create a simple version
theme_hfv <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA)
    )
}

# UI
ui <- fluidPage(
  titlePanel("Average Household Size in Virginia"),
  
  sidebarLayout(
    sidebarPanel(
      # Geography type selection
      selectInput(
        "geography_type", 
        "Geography Type:",
        choices = c("State" = "state", 
                    "Locality" = "locality", 
                    "Metro Area" = "cbsa"),
        selected = "state"
      ),
      
      # Geography name selection (will be dynamically updated)
      uiOutput("geography_selector"),
      
      # Tenure selection (multi-select)
      checkboxGroupInput(
        "tenure", 
        "Tenure:",
        choices = c("All", "Homeowner", "Renter"),
        selected = "All"
      ),
      
      # Year range slider
      uiOutput("year_range")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("household_plot", height = "500px")),
        tabPanel("Data", DT::dataTableOutput("data_table")),
        tabPanel("About", 
                 h3("About this Data"),
                 p("This interactive dashboard visualizes average household size in Virginia from 2010-2023."),
                 p("Data is sourced from American Community Survey estimates and organized by:"),
                 tags$ul(
                   tags$li("Geography Type (state, locality, or metro area)"),
                   tags$li("Specific geography selection"),
                   tags$li("Housing tenure (all households, homeowners, or renters)")
                 ),
                 p("The visualization includes both the actual data points and a LOESS smoothing line to show trends over time.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  data <- reactive({
    # In real app, this would be: read_rds("data/avg_hh_size.rds")
    # For demonstration, we'll simulate loading the data
    req(file.exists("data/avg_hh_size.rds"))
    
    read_rds("data/avg_hh_size.rds") %>%
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Get unique geography names based on selected type
  geo_names <- reactive({
    req(data())
    data() %>%
      filter(geography == input$geography_type) %>%
      pull(name) %>%
      unique() %>%
      sort()
  })
  
  # Dynamic geography selector
  output$geography_selector <- renderUI({
    selectInput(
      "geography_name",
      "Select Geography:",
      choices = geo_names(),
      selected = if(input$geography_type == "state") "Virginia" else geo_names()[1]
    )
  })
  
  # Year range selector
  output$year_range <- renderUI({
    req(data())
    years <- data() %>%
      filter(geography == input$geography_type) %>%
      pull(year) %>%
      unique() %>%
      sort()
    
    sliderInput(
      "years",
      "Year Range:",
      min = min(years),
      max = max(years),
      value = c(min(years), max(years)),
      step = 1,
      sep = ""
    )
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(data(), input$geography_type, input$geography_name, input$tenure, input$years)
    
    data() %>%
      filter(
        geography == input$geography_type,
        name == input$geography_name,
        tenure %in% input$tenure,
        year >= input$years[1],
        year <= input$years[2]
      )
  })
  
  # Generate plot
  output$household_plot <- renderPlot({
    req(filtered_data())
    
    df <- filtered_data()
    
    # If no data matches the filters
    if(nrow(df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters") + 
               theme_void())
    }
    
    # Prepare data for labels
    df_labels <- df %>%
      group_by(tenure) %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate)) %>%
      filter(label_point)
    
    # Create plot
    p <- ggplot(df, aes(x = year, y = estimate, color = tenure, group = tenure)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_smooth(aes(fill = tenure), method = "loess", se = TRUE, alpha = 0.2) +
      geom_text(data = df_labels,
                aes(label = scales::number(estimate, accuracy = 0.01)),
                vjust = -0.8, hjust = 0.5, size = 3.5) +
      labs(
        title = "Average Household Size Over Time",
        subtitle = input$geography_name,
        x = "Year",
        y = "Average Household Size"
      ) +
      scale_y_continuous(
        limits = function(x) c(min(x) * 0.93, max(x) * 1.07),
        labels = scales::number_format(accuracy = 0.01)
      ) +
      scale_color_manual(values = c("All" = "#011E41", "Homeowner" = "#40C0C0", "Renter" = "#E57200")) +
      scale_fill_manual(values = c("All" = "#011E41", "Homeowner" = "#40C0C0", "Renter" = "#E57200")) +
      theme_hfv() +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    p
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    req(filtered_data())
    
    filtered_data() %>%
      select(Year = year, Geography = name, Tenure = tenure, 
             `Average Household Size` = estimate, 
             `Margin of Error` = moe) %>%
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
