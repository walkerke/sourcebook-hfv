library(shiny)
library(tidyverse)
library(scales)
library(ggtext)
library(here)


# Load data
lvng_arr <- readRDS(here("data", "lvng_arr.rds"))

# Create lists for filters
cbsa_list <- sort(unique(lvng_arr$cbsa_title))
locality_list <- sort(unique(lvng_arr$name_long))
year_list <- sort(unique(lvng_arr$year), decreasing = TRUE)
age_list <- sort(unique(lvng_arr$age))

# Pre-aggregate data
# Locality data
locality_la <- lvng_arr %>% 
  group_by(year, name_long, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>%
  group_by(year, name_long, age) %>% 
  mutate(percent = estimate/sum(estimate))

# CBSA data
cbsa_la <- lvng_arr %>% 
  group_by(year, cbsa_title, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, cbsa_title, age) %>% 
  mutate(percent = estimate/sum(estimate))

# State data
state_la <- lvng_arr %>% 
  group_by(year, age, type) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  group_by(year, age) %>% 
  mutate(percent = estimate/sum(estimate))

# UI
ui <- fluidPage(
  titlePanel("Living Arrangements of Adults in Virginia"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = year_list, selected = max(year_list)),
      selectInput("age", "Select Age Group:", choices = age_list, selected = "All ages"),
      
      # Conditional panels for geography selections
      conditionalPanel(
        condition = "input.tabset == 'CBSA'",
        selectInput("cbsa", "Select CBSA:", choices = cbsa_list, selected = cbsa_list[1])
      ),
      conditionalPanel(
        condition = "input.tabset == 'Locality'",
        selectInput("locality", "Select Locality:", choices = locality_list, selected = locality_list[1])
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Statewide", plotOutput("state_plot")),
                  tabPanel("CBSA", plotOutput("cbsa_plot")),
                  tabPanel("Locality", plotOutput("locality_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Helper function for plotting
  create_plot <- function(data, subtitle) {
    ggplot(data,
           aes(x = reorder(type, percent),
               y = percent,
               fill = type)) + 
      geom_col() +
      # Match text color to bar fill color
      geom_text(aes(label = scales::percent(percent, accuracy = 1),
                    color = type),  # Use the same grouping variable for color
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.5) +
      # Make sure text colors match fill colors
      scale_color_hfv() +  # This should use the same palette as scale_fill_hfv()
      labs(title = "<b><span style='color:#011E41'>Living arrangement of adults</span></b>",
           subtitle = subtitle) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_hfv() +
      scale_fill_hfv() +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          lineheight = 0.8,
          margin = margin(t = 5)
        ),
        strip.text = element_blank()
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  }
  
  # State plot
  output$state_plot <- renderPlot({
    filtered_state <- state_la %>%
      filter(year == input$year,
             age == input$age)
    
    create_plot(filtered_state, paste("Virginia -", input$year, "-", input$age))
  })
  
  # CBSA plot
  output$cbsa_plot <- renderPlot({
    filtered_cbsa <- cbsa_la %>%
      filter(year == input$year,
             age == input$age,
             cbsa_title == input$cbsa)
    
    create_plot(filtered_cbsa, paste(input$cbsa, "-", input$year, "-", input$age))
  })
  
  # Locality plot
  output$locality_plot <- renderPlot({
    filtered_locality <- locality_la %>%
      filter(year == input$year,
             age == input$age,
             name_long == input$locality)
    
    create_plot(filtered_locality, paste(input$locality, "-", input$year, "-", input$age))
  })
}

# Run the app
shinyApp(ui = ui, server = server)