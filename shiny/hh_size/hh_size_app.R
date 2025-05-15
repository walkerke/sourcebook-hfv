library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(here)
library(hdatools)  # Your custom package with theme_hfv and scale_fill_hfv
library(shinyjs)   # Added for dynamic UI updates

# Load the data using here package for path management
hh_size <- readRDS(here("data", "rds", "hh_size.rds")) %>% 
  mutate(tenure = case_when(
    tenure == "Owner" ~ "Homeowner",
    TRUE ~ tenure
  ))

# Create a list of all unique CBSAs and localities in Virginia
cbsa_list <- sort(unique(hh_size$cbsa_title))
locality_list <- sort(unique(hh_size$name_long))
year_list <- sort(unique(hh_size$year))

# Note: theme_hfv and scale_fill_hfv are imported from hdatools package

# Pre-process data
# Locality data
locality_size <- hh_size %>% 
  pivot_wider(
    id_cols = c(year, name_long, hhsize),
    names_from = tenure,
    values_from = estimate
  ) %>% 
  mutate(All = Renter + Homeowner) %>% 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) %>%
  arrange(name_long, tenure, year) %>% 
  group_by(name_long, tenure, hhsize) %>% 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
  group_by(year, name_long, tenure) %>% 
  mutate(percent = estimate/sum(estimate)) %>% 
  ungroup()

# CBSA data  
cbsa_size <- hh_size %>% 
  group_by(year, cbsa_title, tenure, hhsize) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  pivot_wider(
    id_cols = c(year, cbsa_title, hhsize),
    names_from = tenure,
    values_from = estimate
  ) %>% 
  mutate(All = Renter + Homeowner) %>% 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) %>% 
  arrange(cbsa_title, tenure, year) %>% 
  group_by(cbsa_title, tenure, hhsize) %>% 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
  group_by(year, cbsa_title, tenure) %>% 
  mutate(percent = estimate/sum(estimate)) %>%
  ungroup()

# State data
state_size <- hh_size %>%
  group_by(year, tenure, hhsize) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>%  
  pivot_wider(
    id_cols = c(year, hhsize),
    names_from = tenure,
    values_from = estimate
  ) %>% 
  mutate(All = Renter + Homeowner) %>% 
  pivot_longer(
    cols = c(Renter, Homeowner, All),
    names_to = "tenure",
    values_to = "estimate"
  ) %>%
  group_by(year, tenure, hhsize) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  arrange(tenure, year) %>% 
  group_by(tenure, hhsize) %>% 
  mutate(pct_change = (estimate - lag(estimate))/lag(estimate)) %>% 
  group_by(year, tenure) %>% 
  mutate(percent = estimate/sum(estimate)) %>% 
  ungroup()

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Household Distribution by Size in Virginia"),
  
  # Tabs for different geographic levels
  tabsetPanel(
    # State Tab
    tabPanel("Statewide", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("state_tenure", "Tenure:", 
                             choices = c("All", "Homeowner", "Renter"),
                             selected = "All"),
                 fluidRow(
                   column(6, selectInput("state_year_start", "Start Year:", 
                                         choices = year_list,
                                         selected = min(year_list))),
                   column(6, selectInput("state_year_end", "End Year:", 
                                         choices = year_list,
                                         selected = max(year_list)))
                 )
               ),
               mainPanel(
                 plotOutput("state_plot")
               )
             )
    ),
    
    # CBSA Tab
    tabPanel("CBSA", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("cbsa_selection", "Select CBSA:", 
                             choices = cbsa_list,
                             selected = cbsa_list[1]),
                 selectInput("cbsa_tenure", "Tenure:", 
                             choices = c("All", "Homeowner", "Renter"),
                             selected = "All"),
                 fluidRow(
                   column(6, selectInput("cbsa_year_start", "Start Year:", 
                                         choices = year_list,
                                         selected = min(year_list))),
                   column(6, selectInput("cbsa_year_end", "End Year:", 
                                         choices = year_list,
                                         selected = max(year_list)))
                 )
               ),
               mainPanel(
                 plotOutput("cbsa_plot")
               )
             )
    ),
    
    # Locality Tab
    tabPanel("Locality", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("locality_selection", "Select Locality:", 
                             choices = locality_list,
                             selected = locality_list[1]),
                 selectInput("locality_tenure", "Tenure:", 
                             choices = c("All", "Homeowner", "Renter"),
                             selected = "All"),
                 fluidRow(
                   column(6, selectInput("locality_year_start", "Start Year:", 
                                         choices = year_list,
                                         selected = min(year_list))),
                   column(6, selectInput("locality_year_end", "End Year:", 
                                         choices = year_list,
                                         selected = max(year_list)))
                 )
               ),
               mainPanel(
                 plotOutput("locality_plot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Ensure end year is not earlier than start year - State tab
  observe({
    if (as.numeric(input$state_year_start) > as.numeric(input$state_year_end)) {
      updateSelectInput(session, "state_year_end", selected = input$state_year_start)
    }
  })
  
  # Ensure end year is not earlier than start year - CBSA tab
  observe({
    if (as.numeric(input$cbsa_year_start) > as.numeric(input$cbsa_year_end)) {
      updateSelectInput(session, "cbsa_year_end", selected = input$cbsa_year_start)
    }
  })
  
  # Ensure end year is not earlier than start year - Locality tab
  observe({
    if (as.numeric(input$locality_year_start) > as.numeric(input$locality_year_end)) {
      updateSelectInput(session, "locality_year_end", selected = input$locality_year_start)
    }
  })
  
  # Create filtered datasets based on user selections
  state_filtered <- reactive({
    years <- c(input$state_year_start, input$state_year_end)
    if(input$state_year_start == input$state_year_end) {
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$state_year_start) == min(year_list)) {
        # If it's the earliest year, use the next available year
        next_year <- sort(year_list)[2]
        years <- c(input$state_year_start, next_year)
      } else {
        # Otherwise use the previous year
        prev_year <- sort(year_list)[which(year_list == input$state_year_start) - 1]
        years <- c(prev_year, input$state_year_start)
      }
    }
    
    state_size %>%
      filter(tenure == input$state_tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize) %>% 
      group_by(hhsize) %>%
      mutate(pct_change = ifelse(year == as.character(max(years)), 
                                 (estimate - estimate[year == as.character(min(years))]) / 
                                   estimate[year == as.character(min(years))],
                                 NA_real_)) %>%
      ungroup()
  })
  
  cbsa_filtered <- reactive({
    years <- c(input$cbsa_year_start, input$cbsa_year_end)
    if(input$cbsa_year_start == input$cbsa_year_end) {
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$cbsa_year_start) == min(year_list)) {
        # If it's the earliest year, use the next available year
        next_year <- sort(year_list)[2]
        years <- c(input$cbsa_year_start, next_year)
      } else {
        # Otherwise use the previous year
        prev_year <- sort(year_list)[which(year_list == input$cbsa_year_start) - 1]
        years <- c(prev_year, input$cbsa_year_start)
      }
    }
    
    cbsa_size %>%
      filter(cbsa_title == input$cbsa_selection,
             tenure == input$cbsa_tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize) %>% 
      group_by(hhsize) %>%
      mutate(pct_change = ifelse(year == as.character(max(years)), 
                                 (estimate - estimate[year == as.character(min(years))]) / 
                                   estimate[year == as.character(min(years))],
                                 NA_real_)) %>%
      ungroup()
  })
  
  locality_filtered <- reactive({
    years <- c(input$locality_year_start, input$locality_year_end)
    if(input$locality_year_start == input$locality_year_end) {
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$locality_year_start) == min(year_list)) {
        # If it's the earliest year, use the next available year
        next_year <- sort(year_list)[2]
        years <- c(input$locality_year_start, next_year)
      } else {
        # Otherwise use the previous year
        prev_year <- sort(year_list)[which(year_list == input$locality_year_start) - 1]
        years <- c(prev_year, input$locality_year_start)
      }
    }
    
    locality_size %>%
      filter(name_long == input$locality_selection,
             tenure == input$locality_tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize) %>% 
      group_by(hhsize) %>%
      mutate(pct_change = ifelse(year == as.character(max(years)), 
                                 (estimate - estimate[year == as.character(min(years))]) / 
                                   estimate[year == as.character(min(years))],
                                 NA_real_)) %>%
      ungroup()
  })
  
  # State plot
  output$state_plot <- renderPlot({
    data <- state_filtered()
    req(nrow(data) > 0)
    
    latest_year <- as.character(max(as.numeric(data$year)))
    earliest_year <- as.character(min(as.numeric(data$year)))
    
    ggplot(data, aes(x = year, y = estimate, fill = year)) + 
      geom_col() +
      facet_wrap(~hhsize, nrow = 1, scales = "free_y") +
      geom_text(
        data = filter(data, year == latest_year & !is.na(pct_change)),
        aes(label = scales::percent(pct_change, accuracy = 0.1)),
        position = position_stack(),
        vjust = -0.5,
        size = 3.5
      ) +
      theme_hfv() +
      scale_fill_hfv() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank()
      ) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(title = paste(input$state_tenure, "households by size"),
           subtitle = paste("Comparing", earliest_year, "to", latest_year))
  })
  
  # CBSA plot
  output$cbsa_plot <- renderPlot({
    data <- cbsa_filtered()
    req(nrow(data) > 0)
    
    latest_year <- as.character(max(as.numeric(data$year)))
    earliest_year <- as.character(min(as.numeric(data$year)))
    
    ggplot(data, aes(x = year, y = estimate, fill = year)) + 
      geom_col() +
      facet_wrap(~hhsize, nrow = 1, scales = "free_y") +
      geom_text(
        data = filter(data, year == latest_year & !is.na(pct_change)),
        aes(label = scales::percent(pct_change, accuracy = 0.1)),
        position = position_stack(),
        vjust = -0.5,
        size = 3.5
      ) +
      theme_hfv() +
      scale_fill_hfv() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank()
      ) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(title = paste(input$cbsa_tenure, "households by size in", input$cbsa_selection),
           subtitle = paste("Comparing", earliest_year, "to", latest_year))
  })
  
  # Locality plot
  output$locality_plot <- renderPlot({
    data <- locality_filtered()
    req(nrow(data) > 0)
    
    latest_year <- as.character(max(as.numeric(data$year)))
    earliest_year <- as.character(min(as.numeric(data$year)))
    
    ggplot(data, aes(x = year, y = estimate, fill = year)) + 
      geom_col() +
      facet_wrap(~hhsize, nrow = 1, scales = "free_y") +
      geom_text(
        data = filter(data, year == latest_year & !is.na(pct_change)),
        aes(label = scales::percent(pct_change, accuracy = 0.1)),
        position = position_stack(),
        vjust = -0.5,
        size = 3.5
      ) +
      theme_hfv() +
      scale_fill_hfv() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank()
      ) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(title = paste(input$locality_tenure, "households by size in", input$locality_selection),
           subtitle = paste("Comparing", earliest_year, "to", latest_year))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)