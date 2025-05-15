# app.R
# Load required packages
library(shiny)
library(tidyverse)
library(here)
library(plotly)
library(hdatools)
# You'll need to ensure the hdatools package is installed or replace scale_fill_hfv() with another color scale

# Read in the data
b25042 <- read_rds(here("data", "rds", "b25042.rds"))

# Define the bedroom order
bedroom_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", 
                   "4 bedrooms", "5 or more bedrooms")

# Pre-process the data
state_bed <- b25042 |> 
  group_by(year, tenure, br) |> 
  summarise(estimate = sum(estimate)) |>
  mutate(br = factor(br, levels = bedroom_order))

cbsa_bed <- b25042 |> 
  group_by(year, cbsa_title, tenure, br) |> 
  summarise(estimate = sum(estimate)) |>
  mutate(br = factor(br, levels = bedroom_order))

local_bed <- b25042 |> 
  group_by(year, name_long, tenure, br) |> 
  summarise(estimate = sum(estimate)) |>
  mutate(br = factor(br, levels = bedroom_order))

# Get unique values for filters
available_years <- sort(unique(b25042$year), decreasing = TRUE)
available_cbsas <- sort(unique(b25042$cbsa_title))
available_localities <- sort(unique(b25042$name_long))

# Define UI
ui <- fluidPage(
  titlePanel("Distribution of Housing by Bedroom Count and Tenure"),
  
  fluidRow(
    # Common filter for all tabs
    column(2,
           selectInput("year", "Select Year:", 
                       choices = available_years,
                       selected = max(available_years))
    ),
    
    # CBSA filter - only visible on CBSA tab
    column(3,
           # Only show on CBSA tab
           conditionalPanel(
             condition = "input.tabset == 'CBSA'",
             selectInput("cbsa", "Select CBSA:", 
                         choices = available_cbsas)
           ),
           
           # Only show on Local tab
           conditionalPanel(
             condition = "input.tabset == 'Locality'",
             selectInput("locality", "Select Locality:", 
                         choices = available_localities)
           )
    ),
    column(7) # Empty column for spacing
  ),
  
  # Tab panel with plots
  tabsetPanel(id = "tabset",
              tabPanel("State", 
                       plotlyOutput("state_plot", height = "600px")),
              tabPanel("CBSA", 
                       plotlyOutput("cbsa_plot", height = "600px")),
              tabPanel("Locality", 
                       plotlyOutput("local_plot", height = "600px"))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Update CBSA choices based on selected year
  observe({
    filtered_cbsas <- sort(unique(cbsa_bed$cbsa_title[cbsa_bed$year == input$year]))
    
    updateSelectInput(session, "cbsa",
                      choices = filtered_cbsas,
                      selected = if("Richmond, VA" %in% filtered_cbsas) "Richmond, VA" else filtered_cbsas[1])
  })
  
  # Update locality choices based on selected year
  observe({
    filtered_localities <- sort(unique(local_bed$name_long[local_bed$year == input$year]))
    
    updateSelectInput(session, "locality",
                      choices = filtered_localities,
                      selected = if("Richmond City" %in% filtered_localities) "Richmond City" else filtered_localities[1])
  })
  
  # State-level plot
  output$state_plot <- renderPlotly({
    state_data <- state_bed |> 
      filter(year == input$year)
    
    p <- ggplot(state_data,
                aes(x = br,
                    y = estimate,
                    fill = tenure,
                    text = paste0(br, "<br>", 
                                  "Tenure: ", tenure, "<br>",
                                  "Count: ", scales::comma(estimate)))) +
      geom_col() +
      coord_flip() +
      facet_wrap(~tenure) +
      scale_fill_hfv() +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(title = paste("Virginia:", input$year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # CBSA-level plot
  output$cbsa_plot <- renderPlotly({
    # Check if the selected CBSA exists in the filtered data
    selected_cbsa <- input$cbsa
    filtered_cbsas <- unique(cbsa_bed$cbsa_title[cbsa_bed$year == input$year])
    
    # Default to first available CBSA if selected one doesn't exist for this year
    if(!(selected_cbsa %in% filtered_cbsas)) {
      selected_cbsa <- filtered_cbsas[1]
    }
    
    cbsa_data <- cbsa_bed |> 
      filter(year == input$year,
             cbsa_title == selected_cbsa)
    
    p <- ggplot(cbsa_data,
                aes(x = br,
                    y = estimate,
                    fill = tenure,
                    text = paste0(br, "<br>", 
                                  "Tenure: ", tenure, "<br>",
                                  "Count: ", scales::comma(estimate)))) +
      geom_col() +
      coord_flip() +
      facet_wrap(~tenure) +
      scale_fill_hfv() +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(title = paste(selected_cbsa, ":", input$year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Locality-level plot
  output$local_plot <- renderPlotly({
    # Check if the selected locality exists in the filtered data
    selected_locality <- input$locality
    filtered_localities <- unique(local_bed$name_long[local_bed$year == input$year])
    
    # Default to first available locality if selected one doesn't exist for this year
    if(!(selected_locality %in% filtered_localities)) {
      selected_locality <- filtered_localities[1]
    }
    
    local_data <- local_bed |> 
      filter(year == input$year,
             name_long == selected_locality)
    
    p <- ggplot(local_data,
                aes(x = br,
                    y = estimate,
                    fill = tenure,
                    text = paste0(br, "<br>", 
                                  "Tenure: ", tenure, "<br>",
                                  "Count: ", scales::comma(estimate)))) +
      geom_col() +
      coord_flip() +
      facet_wrap(~tenure) +
      scale_fill_hfv() +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(title = paste(selected_locality, ":", input$year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0),
        plot.title.position = "plot",
        legend.position = "none",
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)