library(shiny)
library(tidyverse)
library(scales)
library(hdatools)  # For scale_fill_hfv(), theme_hfv() and scale_color_hfv()
library(here)

# Define UI
ui <- fluidPage(
  titlePanel("Housing Type Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      # Year filter for all tabs
      selectInput("year", "Select Year:",
                  choices = 2010:2023,
                  selected = 2023),
      
      # Conditional UI based on selected tab
      conditionalPanel(
        condition = "input.tabs == 'cbsa'",
        selectInput("cbsa", "Select Metro Area:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'local'",
        selectInput("locality", "Select Locality:", choices = NULL)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("State", plotOutput("state_plot"), value = "state"),
        tabPanel("Metro Area", plotOutput("cbsa_plot"), value = "cbsa"),
        tabPanel("Locality", plotOutput("local_plot"), value = "local")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Read in data
  b25032 <- reactive({
    read_rds(here("data/b25032.rds"))
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
      mutate(percent = estimate/sum(estimate)) %>% 
      group_by(year, name_long) %>% 
      mutate(percent_total = estimate/sum(estimate))
  })
  
  # Populate CBSA dropdown
  observe({
    cbsa_choices <- unique(cbsa_housing()$cbsa_title)
    updateSelectInput(session, "cbsa", choices = cbsa_choices, 
                      selected = "Richmond, VA")
  })
  
  # Populate locality dropdown
  observe({
    locality_choices <- unique(local_housing()$name_long)
    updateSelectInput(session, "locality", choices = locality_choices,
                      selected = "Richmond City")
  })
  
  # Filter data based on inputs
  filtered_state <- reactive({
    state_housing() %>% 
      filter(year == input$year)
  })
  
  filtered_cbsa <- reactive({
    cbsa_housing() %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa)
  })
  
  filtered_local <- reactive({
    local_housing() %>% 
      filter(year == input$year,
             name_long == input$locality)
  })
  
  # Create title text
  title_text <- reactive({
    paste0("Distribution of housing type (", input$year, ")")
  })
  
  # Create a common plotting function
  create_plot <- function(data) {
    ggplot(data, 
           aes(x = reorder(type, -percent_total), 
               y = percent_total, 
               fill = tenure)) +
      geom_col(position = "dodge") +
      # Add labels that match fill color
      geom_text(aes(label = scales::percent(percent_total, accuracy = 1), 
                    color = tenure),
                position = position_dodge(width = 0.9),
                hjust = -0.2) +
      facet_wrap(~tenure) +
      scale_fill_hfv() + 
      scale_color_hfv() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = c(0, 0.2))) +
      labs(
        title = title_text()
      ) +
      theme_hfv() +
      theme(legend.position = "none",
            strip.text = element_blank())
  }
  
  # Render the state plot
  output$state_plot <- renderPlot({
    create_plot(filtered_state())
  })
  
  # Render the CBSA plot
  output$cbsa_plot <- renderPlot({
    create_plot(filtered_cbsa())
  })
  
  # Render the local plot
  output$local_plot <- renderPlot({
    create_plot(filtered_local())
  })
  
  # Add hover interactivity with tooltips
  # Note: This requires the plotly package, which translates ggplot objects to interactive plots
  # If you want to use this, add library(plotly) to the top and change renderPlot to renderPlotly
  # Then wrap the ggplot object with ggplotly() function
}

# Run the application
shinyApp(ui = ui, server = server)
