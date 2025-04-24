# Household Composition Shiny App
# This app visualizes household composition data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
library(hdatools) 
library(ggiraph)
library(scales)
library(here)  # For better path handling
library(shinyWidgets) # Added for toggle switch

# Determine the app directory and set the data path
# Option 1: Using here package (recommended)
data_path <- here::here("data", "hh_type.rds")

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
  titlePanel("Household Composition"),
  
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
                 # Replace select input with toggle switch
                 switchInput(
                   inputId = "state_year_toggle",
                   label = paste("Toggle between", min(year_list), "and", max(year_list)),
                   value = TRUE,
                   onLabel = max(year_list),
                   offLabel = min(year_list),
                   size = "large"
                 ),
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
                 # Replace select input with toggle switch
                 switchInput(
                   inputId = "cbsa_year_toggle",
                   label = "Switch Year",
                   value = TRUE,
                   onLabel = max(year_list),
                   offLabel = min(year_list),
                   size = "large"
                 ),
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
                 # Replace select input with toggle switch
                 switchInput(
                   inputId = "locality_year_toggle",
                   label = paste("Toggle between", min(year_list), "and", max(year_list)),
                   value = TRUE,
                   onLabel = max(year_list),
                   offLabel = min(year_list),
                   size = "large"
                 ),
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
                   # Keep both years for comparison - we need both for this tab
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
                   # Replace select input with toggle switch
                   switchInput(
                     inputId = "compare_year_toggle",
                     label = paste("Toggle between", min(year_list), "and", max(year_list)),
                     value = TRUE,
                     onLabel = max(year_list),
                     offLabel = min(year_list),
                     size = "large"
                   ),
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
  
  # Create reactive expressions for toggle switches to get selected years
  state_year <- reactive({
    if(input$state_year_toggle) max(year_list) else min(year_list)
  })
  
  cbsa_year <- reactive({
    if(input$cbsa_year_toggle) max(year_list) else min(year_list)
  })
  
  locality_year <- reactive({
    if(input$locality_year_toggle) max(year_list) else min(year_list)
  })
  
  compare_year <- reactive({
    if(input$compare_year_toggle) max(year_list) else min(year_list)
  })
  
  # Pre-process data for better performance
  # Aggregate data to the locality-level
  locality_hh <- reactive({
    selected_year <- locality_year()
    
    hh_type %>% 
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year, 
             name_long == input$locality) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the CBSA-level
  cbsa_hh <- reactive({
    selected_year <- cbsa_year()
    
    hh_type %>% 
      group_by(year, cbsa_title, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year, 
             cbsa_title == input$cbsa) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Aggregate data to the state-level
  state_hh <- reactive({
    selected_year <- state_year()
    
    hh_type %>% 
      group_by(year, type, subtype) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      filter(year == selected_year) %>% 
      group_by(type) %>% 
      mutate(rank_within_type = rank(percent, ties.method = "first")) %>% 
      ungroup()
  })
  
  # Generate title text
  title_text <- "<b><span style='color:#011E41'>Householder with no partner</span></b> and 
<b><span style='color:#40C0C0'>Married or cohabitating couple</span></b>"
  
  # Create a custom theme function to be applied consistently
  custom_theme <- function() {
    theme_hfv() %+replace%
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
      )
  }
  
  # Create interactive plot for Statewide tab
  output$state_plot <- renderGirafe({
    state_data <- state_hh()
    selected_year <- state_year()
    
    p <- ggplot(state_data,
                aes(x = reorder(subtype, rank_within_type),
                    y = percent,
                    fill = type)) + 
      # Make sure text colors match fill colors
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
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
      labs(title = title_text,
           subtitle = paste("Virginia:", selected_year),
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
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
    selected_year <- cbsa_year()
    
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
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = paste(input$cbsa, ":", selected_year),
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
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
    selected_year <- locality_year()
    
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
      scale_color_manual(values = c("#011E41", "#40C0C0")) +
      labs(title = title_text,
           subtitle = paste(input$locality, ":", selected_year),
           x = NULL,
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      # Hide the color legend since it's redundant with the fill legend
      guides(color = "none") +
      custom_theme() +
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
      selected_year <- state_year()
      paste("virginia-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- state_year()
      p <- create_static_plot(state_hh(), paste("Virginia:", selected_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_cbsa <- downloadHandler(
    filename = function() {
      selected_year <- cbsa_year()
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$cbsa)
      paste(clean_name, "-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- cbsa_year()
      p <- create_static_plot(cbsa_hh(), paste(input$cbsa, ":", selected_year))
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_locality <- downloadHandler(
    filename = function() {
      selected_year <- locality_year()
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$locality)
      paste(clean_name, "-household-composition-", selected_year, ".png", sep = "")
    },
    content = function(file) {
      selected_year <- locality_year()
      p <- create_static_plot(locality_hh(), paste(input$locality, ":", selected_year))
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
      selected_year <- compare_year()
      
      data1 <- hh_type %>% 
        filter(name_long == input$compare_locality1, year == selected_year) %>%
        group_by(year, name_long) %>% 
        mutate(percent = estimate/sum(estimate)) %>%
        group_by(type) %>% 
        mutate(rank_within_type = rank(percent, ties.method = "first")) %>%
        ungroup() %>%
        mutate(comparison = input$compare_locality1)
      
      data2 <- hh_type %>% 
        filter(name_long == input$compare_locality2, year == selected_year) %>%
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
      selected_year <- compare_year()
      plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", selected_year)
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
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      custom_theme() +
      theme(
        legend.position = "top"
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
        selected_year <- compare_year()
        clean_name1 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality1)
        clean_name2 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality2)
        paste(clean_name1, "-vs-", clean_name2, "-", selected_year, ".png", sep = "")
      }
    },
    content = function(file) {
      comparison_data <- compare_data()
      
      # Generate appropriate title based on comparison type
      if (input$compare_type == "Years") {
        plot_title <- paste("Comparing", input$compare_locality, "between", input$compare_year1, "and", input$compare_year2)
      } else {
        selected_year <- compare_year()
        plot_title <- paste("Comparing", input$compare_locality1, "and", input$compare_locality2, "in", selected_year)
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