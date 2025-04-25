library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(scales)

# UI
ui <- fluidPage(
  titlePanel("Median Household Income by Householder Age"),
  
  sidebarLayout(
    sidebarPanel(
      # Dollar type selector
      radioButtons("dollar_type", "Dollar Type:",
                   choices = list("Current Dollars" = "estimate", 
                                  "Inflation-Adjusted Dollars" = "adjusted"),
                   selected = "estimate"),
      
      # Geography selectors (one for each tab)
      conditionalPanel(
        condition = "input.tabset_id == 'state_tab'",
        selectInput("state_select", "Select State:",
                    choices = NULL)  # Will be populated in server
      ),
      
      conditionalPanel(
        condition = "input.tabset_id == 'cbsa_tab'",
        selectInput("cbsa_select", "Select Metro Area:",
                    choices = NULL)  # Will be populated in server
      ),
      
      conditionalPanel(
        condition = "input.tabset_id == 'local_tab'",
        selectInput("local_select", "Select Locality:",
                    choices = NULL)  # Will be populated in server
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabset_id",
                  tabPanel("State", value = "state_tab", plotOutput("state_plot")),
                  tabPanel("Metro Area", value = "cbsa_tab", plotOutput("cbsa_plot")),
                  tabPanel("Locality", value = "local_tab", plotOutput("local_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Read in data
  state_inc_age <- reactive({
    read_rds(here("data/b19049_state.rds"))
  })
  
  cbsa_inc_age <- reactive({
    read_rds(here("data/b19049_cbsa.rds"))
  })
  
  local_inc_age <- reactive({
    read_rds(here("data/b19049_locality.rds"))
  })
  
  # Populate dropdown menus
  observe({
    # For states
    states <- unique(state_inc_age()$state)
    updateSelectInput(session, "state_select", choices = states, 
                      selected = "Virginia")
    
    # For CBSAs
    cbsas <- unique(cbsa_inc_age()$cbsa)
    updateSelectInput(session, "cbsa_select", choices = cbsas,
                      selected = "Richmond, VA Metro Area")
    
    # For localities
    localities <- unique(local_inc_age()$locality)
    updateSelectInput(session, "local_select", choices = localities,
                      selected = "Chesterfield County")
  })
  
  # Create theme_hfv function if it doesn't exist in your environment
  theme_hfv <- function() {
    theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  }
  
  # Create scale_color_hfv function if it doesn't exist
  scale_color_hfv <- function() {
    scale_color_brewer(palette = "Set2")
  }
  
  # State Plot
  output$state_plot <- renderPlot({
    req(input$state_select)
    
    filtered_data <- state_inc_age() %>%
      filter(state == input$state_select)
    
    y_var <- input$dollar_type
    
    ggplot(filtered_data,
           aes(
             x = year,
             y = .data[[y_var]],  # Use .data pronoun to refer to the column dynamically
             color = age
           )) +
      geom_line() +
      geom_point() +
      theme_hfv() +
      scale_color_hfv() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(
        breaks = unique(filtered_data$year),
        labels = unique(filtered_data$year)
      ) +
      labs(
        title = paste("Median Household Income by Age in", input$state_select),
        y = ifelse(input$dollar_type == "adjusted", 
                   "Inflation-Adjusted Dollars", 
                   "Current Dollars"),
        x = "Year"
      )
  })
  
  # CBSA Plot
  output$cbsa_plot <- renderPlot({
    req(input$cbsa_select)
    
    filtered_data <- cbsa_inc_age() %>%
      filter(cbsa == input$cbsa_select)
    
    y_var <- input$dollar_type
    
    ggplot(filtered_data,
           aes(
             x = year,
             y = .data[[y_var]],
             color = age
           )) +
      geom_line() +
      geom_point() +
      theme_hfv() +
      scale_color_hfv() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_continuous(
        breaks = unique(filtered_data$year),
        labels = unique(filtered_data$year)
      ) +
      labs(
        title = paste("Median Household Income by Age in", input$cbsa_select),
        y = ifelse(input$dollar_type == "adjusted", 
                   "Inflation-Adjusted Dollars", 
                   "Current Dollars"),
        x = "Year"
      )
  })
  
  # Locality Plot
  output$local_plot <- renderPlot({
    req(input$local_select)
    
    filtered_data <- local_inc_age() %>%
      filter(locality == input$local_select) %>%
      mutate(
        estimate = as.numeric(estimate),
        adjusted = as.numeric(adjusted)
      )
    
    y_var <- input$dollar_type
    
    ggplot(filtered_data,
           aes(
             x = year,
             y = .data[[y_var]],
             color = age,
             group = age
           )) +
      geom_line() +
      geom_point() +
      theme_hfv() +
      scale_color_hfv() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_discrete(
        breaks = unique(filtered_data$year),
        labels = unique(filtered_data$year)
      ) +
      labs(
        title = paste("Median Household Income by Age in", input$local_select),
        y = ifelse(input$dollar_type == "adjusted", 
                   "Inflation-Adjusted Dollars", 
                   "Current Dollars"),
        x = "Year"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)