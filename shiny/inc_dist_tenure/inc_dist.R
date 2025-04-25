library(shiny)
library(tidyverse)
library(here)

# UI
ui <- fluidPage(
  titlePanel("Income Distribution by Tenure"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:",
                  choices = NULL,  # Will be updated in server
                  selected = NULL),
      
      # Container for the geography filters
      conditionalPanel(
        condition = "input.tabselected == 'state'",
        # No additional filters for state
      ),
      conditionalPanel(
        condition = "input.tabselected == 'cbsa'",
        selectInput("cbsa", "Select Metro Area:",
                    choices = NULL,  # Will be updated in server
                    selected = NULL)
      ),
      conditionalPanel(
        condition = "input.tabselected == 'local'",
        selectInput("county", "Select County:",
                    choices = NULL,  # Will be updated in server
                    selected = NULL)
      ),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        tabPanel("State", value = "state", plotOutput("statePlot")),
        tabPanel("Metro", value = "cbsa", plotOutput("cbsaPlot")),
        tabPanel("County", value = "local", plotOutput("localPlot"))
      ),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  inc_dist <- reactive({
    read_rds(here("data/b25118_data.rds"))
  })
  
  # Define income order once
  income_order <- c("Less than $15,000", "$15,000 to $24,999", "$25,000 to $49,999",
                    "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999",
                    "$150,000 or more")
  
  # Update year filter choices
  observe({
    years <- unique(inc_dist()$year)
    updateSelectInput(session, "year", 
                      choices = years,
                      selected = max(years))
  })
  
  # Update CBSA filter choices
  observe({
    req(input$year)
    cbsas <- inc_dist() %>%
      filter(year == input$year) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsas,
                      selected = "Non-Metro")
  })
  
  # Update county filter choices
  observe({
    req(input$year)
    counties <- inc_dist() %>%
      filter(year == input$year) %>%
      pull(name_long) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "county", 
                      choices = counties,
                      selected = counties[1])
  })
  
  # Create state-level data
  state_data <- reactive({
    req(input$year)
    
    inc_dist() %>% 
      group_by(year, tenure, income) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      filter(year == input$year) %>%
      mutate(income = factor(income, levels = income_order))
  })
  
  # Create CBSA-level data
  cbsa_data <- reactive({
    req(input$year, input$cbsa)
    
    inc_dist() %>% 
      group_by(year, cbsa_title, tenure, income) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa) %>%
      mutate(income = factor(income, levels = income_order))
  })
  
  # Create local-level data
  local_data <- reactive({
    req(input$year, input$county)
    
    inc_dist() %>% 
      filter(year == input$year,
             name_long == input$county) %>%
      mutate(income = factor(income, levels = income_order))
  })
  
  # Plot creation function to avoid repetition
  create_tenure_plot <- function(data) {
    ggplot(data,
           aes(
             x = income,
             y = estimate,
             fill = tenure)) +
      geom_col() +
      facet_wrap(~tenure, ncol = 1) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      theme_hfv() +
      scale_fill_hfv() +
      theme(
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45,  # Angle the text
                                   hjust = 1,    # Horizontal justification
                                   vjust = 1,    # Vertical justification
                                   size = 10,     # Smaller text size
                                   lineheight = 0.9)) +  # Reduced line height
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +  # Wrap text at 10 characters
      labs(title = paste("Income Distribution by Tenure", 
                         ifelse(exists("cbsa_title", data), paste("in", unique(data$cbsa_title)), ""),
                         ifelse(exists("name_long", data), paste("in", unique(data$name_long)), ""),
                         input$year),
           y = "Number of Households",
           x = "Household Income") +
      theme(legend.position = "none")
  }
  
  # Render state plot
  output$statePlot <- renderPlot({
    req(state_data())
    create_tenure_plot(state_data())
  })
  
  # Render CBSA plot
  output$cbsaPlot <- renderPlot({
    req(cbsa_data())
    create_tenure_plot(cbsa_data())
  })
  
  # Render local plot
  output$localPlot <- renderPlot({
    req(local_data())
    create_tenure_plot(local_data())
  })
}

# Run the app
shinyApp(ui = ui, server = server)