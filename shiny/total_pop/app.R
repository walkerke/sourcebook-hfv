# Household Composition Shiny App
# This app visualizes household composition data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
library(hdatools)  # Assuming this is your custom package
library(ggiraph)
library(scales)
library(here)  # For better path handling

# Determine the app directory and set the data path
# Option 1: Using here package (recommended)
data_path <- here::here("data", "hh_type.rds")

# Option 2: Using parent directory notation
# data_path <- "../data/hh_type.rds" 

# Option 3: Using file.path and dynamically finding the parent directory
# app_dir <- getwd()
# parent_dir <- dirname(app_dir)
# data_path <- file.path(parent_dir, "data", "hh_type.rds")

# Load the data with error handling
tryCatch({
  hh_type <- read_rds(data_path)
  # Create a list of all unique CBSAs and localities in Virginia
  cbsa_list <- sort(unique(hh_type$cbsa_title))
  locality_list <- sort(unique(hh_type$name_long))
  year_list <- sort(unique(hh_type$year), decreasing = TRUE)
}, error = function(e) {
  # This will be displayed when the app starts if there's an error loading the data
  stop(paste("Error loading data file:", e$message, 
             "\nPlease check the path to your data file. If your app is in a subdirectory of your project,",
             "you may need to adjust the path in the app.R file."))
})

# UI
ui <- fluidPage(
  # App title and styling
  titlePanel("Virginia Household Composition"),
  
  # Add CSS for better styling
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; border-color: #ddd; }
      .shiny-download-link { width: 100%; margin-bottom: 10px; }
      .nav-tabs { margin-bottom: 15px; }
      h2 { color: #011E41; }
      .header-panel { padding: 15px; margin-bottom: 15px; background-color: #f8f9fa; border-radius: 5px; }
    "))
  ),
  
  # Add header info
  div(class = "header-panel",
      h4("About This Dashboard"),
      p("This dashboard displays household composition data from the American Community Survey (ACS).
       Data is available for the state of Virginia, Core-Based Statistical Areas (CBSAs), and all Virginia localities."),
      p("Source: U.S. Census Bureau, American Community Survey 5-year estimates, Table B11021.")
  ),
  
  # Create tabs for different geographic levels
  tabsetPanel(
    # Tab 1: Statewide
    tabPanel("Statewide", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("state_year", 
                             "Select Year:", 
                             choices = year_list,
                             selected = max(year_list)),
                 hr(),
                 downloadButton("download_state", "Download Plot"),
                 hr(),
                 helpText("This visualization shows household composition across Virginia.")
               ),
               mainPanel(
                 girafeOutput("state_plot", height = "600px")
               )
             )
    ),
    
    # Tab 2: CBSA
    tabPanel("CBSA", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("cbsa", 
                             "Select CBSA:", 
                             choices = cbsa_list,
                             selected = cbsa_list[1]),
                 selectInput("cbsa_year", 
                             "Select Year:", 
                             choices = year_list,
                             selected = max(year_list)),
                 hr(),
                 downloadButton("download_cbsa", "Download Plot"),
                 hr(),
                 helpText("Core-Based Statistical Areas (CBSAs) represent metropolitan regions.")
               ),
               mainPanel(
                 girafeOutput("cbsa_plot", height = "600px")
               )
             )
    ),
    
    # Tab 3: Locality
    tabPanel("Locality", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("locality", 
                             "Select Locality:", 
                             choices = locality_list,
                             selected = locality_list[1]),
                 selectInput("locality_year", 
                             "Select Year:", 
                             choices = year_list,
                             selected = max(year_list)),
                 hr(),
                 downloadButton("download_locality", "Download Plot"),
                 hr(),
                 helpText("Localities are counties and independent cities in Virginia.")
               ),
               mainPanel(
                 girafeOutput("locality_plot", height = "600px")
               )
             )
    ),
    
    # Tab 4: Comparison
    tabPanel("Compare", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("compare_type", 
                             "Compare by:", 
                             choices = c("Years", "Localities"),
                             selected = "Years"),
                 
                 # Conditional panels based on comparison type
                 conditionalPanel(
                   condition = "input.compare_type == 'Years'",
                   selectInput("compare_locality", 
                               "Select Locality:", 
                               choices = locality_list,
                               selected = locality_list[1]),
                   selectInput("compare_year1", 
                               "First Year:", 
                               choices = year_list,
                               selected = year_list[2]),
                   selectInput("compare_year2", 
                               "Second Year:", 
                               choices = year_list,
                               selected = year_list[1])
                 ),
                 
                 conditionalPanel(
                   condition = "input.compare_type == 'Localities'",
                   selectInput("compare_year", 
                               "Select Year:", 
                               choices = year_list,
                               selected = max(year_list)),
                   selectInput("compare_locality1", 
                               "First Locality:", 
                               choices = locality_list,
                               selected = locality_list[1]),
                   selectInput("compare_locality2", 
                               "Second Locality:", 
                               choices = locality_list,
                               selected = locality_list[2])
                 ),
                 hr(),
                 downloadButton("download_compare", "Download Plot"),
                 hr(),
                 helpText("Compare household composition across different years or localities.")
               ),
               mainPanel(
                 girafeOutput("compare_plot", height = "600px")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Pre-process data for better performance
  # Aggregate data to the locality-level
  locality_hh <- reactive({
    hh_type %>% 
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == input$locality_year, 
             name_long == input$locality) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the CBSA-level
  cbsa_hh <- reactive({
    hh_type %>% 
      group_by(year, cbsa_title, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == input$cbsa_year, 
             cbsa_title == input$cbsa) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the state-level
  state_hh <- reactive({
    hh_type %>% 
      group_by(year, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == input$state_year) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Generate title text
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
<b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Create interactive plot for Statewide tab
  output$state_plot <- renderGirafe({
    state_data <- state_hh()
    
    p <- ggplot(state_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = type,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      # Make sure text colors match fill colors
      scale_color_hfv() +
      labs(title = title_text,
           subtitle = paste("Virginia:", input$state_year),
           x = NULL,
           y = "Percent of Households") +
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
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Create interactive plot for CBSA tab
  output$cbsa_plot <- renderGirafe({
    cbsa_data <- cbsa_hh()
    
    p <- ggplot(cbsa_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = type,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      # Make sure text colors match fill colors
      scale_color_hfv() +
      labs(title = title_text,
           subtitle = paste(input$cbsa, ":", input$cbsa_year),
           x = NULL,
           y = "Percent of Households") +
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
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Create interactive plot for Locality tab
  output$locality_plot <- renderGirafe({
    locality_data <- locality_hh()
    
    p <- ggplot(locality_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = type,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      # Make sure text colors match fill colors
      scale_color_hfv() +
      labs(title = title_text,
           subtitle = paste(input$locality, ":", input$locality_year),
           x = NULL,
           y = "Percent of Households") +
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
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Helper function to create static plots for downloads
  create_static_plot <- function(data, subtitle) {
    ggplot(data,
           aes(x = reorder(subtype, rank_within_type),
               y = percent,
               fill = type)) + 
      geom_col() +
      # Match text color to bar fill color
      geom_text(aes(label = scales::percent(percent, accuracy = 1),
                    color = type),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.5) +
      # Make sure text colors match fill colors
      scale_color_hfv() +
      labs(title = "Household Composition by Type",
           subtitle = subtitle,
           caption = "Source: ACS 5-year estimates",
           x = NULL,
           y = "Percent of Households") +
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
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
  }
  
  # Download handlers for each plot
  output$download_state <- downloadHandler(
    filename = function() {
      paste("virginia-household-composition-", input$state_year, ".png", sep = "")
    },
    content = function(file) {
      p <- create_static_plot(state_hh(), paste("Virginia:", input$state_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_cbsa <- downloadHandler(
    filename = function() {
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$cbsa)
      paste(clean_name, "-household-composition-", input$cbsa_year, ".png", sep = "")
    },
    content = function(file) {
      p <- create_static_plot(cbsa_hh(), paste(input$cbsa, ":", input$cbsa_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_locality <- downloadHandler(
    filename = function() {
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$locality)
      paste(clean_name, "-household-composition-", input$locality_year, ".png", sep = "")
    },
    content = function(file) {
      p <- create_static_plot(locality_hh(), paste(input$locality, ":", input$locality_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Comparison plot data preparation
  compare_data <- reactive({
    if (input$compare_type == "Years") {
      # Compare the same locality across different years
      data1 <- hh_type %>% 
        filter(name_long == input$compare_locality, year == input$compare_year1) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = paste(name_long, ":", input$compare_year1))
      
      data2 <- hh_type %>% 
        filter(name_long == input$compare_locality, year == input$compare_year2) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = paste(name_long, ":", input$compare_year2))
      
      bind_rows(data1, data2)
    } else {
      # Compare different localities in the same year
      data1 <- hh_type %>% 
        filter(name_long == input$compare_locality1, year == input$compare_year) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = input$compare_locality1)
      
      data2 <- hh_type %>% 
        filter(name_long == input$compare_locality2, year == input$compare_year) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = input$compare_locality2)
      
      bind_rows(data1, data2)
    }
  })
  
  # Render comparison plot
  output$compare_plot <- renderGirafe({
    comparison_data <- compare_data()
    
    # Generate appropriate title based on comparison type
    if (input$compare_type == "Years") {
      plot_title <- paste("Comparing", input$compare_locality, "between", input$compare_year1, "and", input$compare_year2)
    } else {
      plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", input$compare_year)
    }
    
    p <- ggplot(comparison_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = comparison)) + 
      geom_col_interactive(
        aes(tooltip = paste0(subtype, ": ", scales::percent(percent, accuracy = 0.1))),
        position = "dodge",
        hover_nearest = TRUE
      ) +
      # Match text color to bar fill color
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 1),
            color = comparison,
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
      ) +
      # Make sure text colors match fill colors
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = plot_title,
           x = NULL,
           y = "Percent of Households",
           fill = "Comparison",
           color = "Comparison") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_hfv() +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      theme(
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          lineheight = 0.8,
          margin = margin(t = 5)
        ),
        legend.position = "top",
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10))
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      facet_grid(cols = vars(type), scales = "free_x", space = "free")
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "fill-opacity: 0.8;"),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # Download handler for comparison plot
  output$download_compare <- downloadHandler(
    filename = function() {
      if (input$compare_type == "Years") {
        clean_name <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality)
        paste(clean_name, "-comparison-", input$compare_year1, "-vs-", input$compare_year2, ".png", sep = "")
      } else {
        clean_name1 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality1)
        clean_name2 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality2)
        paste(clean_name1, "-vs-", clean_name2, "-", input$compare_year, ".png", sep = "")
      }
    },
    content = function(file) {
      comparison_data <- compare_data()
      
      # Generate appropriate title based on comparison type
      if (input$compare_type == "Years") {
        plot_title <- paste("Comparing", input$compare_locality, "between", input$compare_year1, "and", input$compare_year2)
      } else {
        plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", input$compare_year)
      }
      
      p <- ggplot(comparison_data,
                  aes(x = reorder(subtype, rank_within_type),
                      y = percent,
                      fill = comparison)) + 
        geom_col(position = "dodge") +
        # Match text color to bar fill color
        geom_text(aes(label = scales::percent(percent, accuracy = 1),
                      color = comparison),
                  position = position_dodge(width = 0.9),
                  vjust = -0.5,
                  size = 3) +
        # Make sure text colors match fill colors
        scale_color_manual(values = c("#011E41", "#40C0C0")) +
        labs(title = "Household Composition by Type",
             subtitle = plot_title,
             caption = "Source: ACS 5-year estimates",
             x = NULL,
             y = "Percent of Households",
             fill = "Comparison",
             color = "Comparison") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_hfv() +
        scale_fill_manual(values = c("#011E41", "#40C0C0")) +
        theme(
          axis.text.x = element_text(
            angle = 0,
            hjust = 0.5,
            vjust = 0.5,
            lineheight = 0.8,
            margin = margin(t = 5)
          ),
          legend.position = "top",
          plot.title = element_markdown(),
          plot.subtitle = element_text(size = 12, margin = margin(b = 10))
        ) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        facet_grid(cols = vars(type), scales = "free_x", space = "free")
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)