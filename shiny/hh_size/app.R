# Household Size Distribution Shiny App
# This app visualizes household size data across Virginia
# with tabs for statewide, CBSA, and locality views

library(shiny)
library(tidyverse)
library(ggtext)
library(ggiraph)  # For interactive plots
library(scales)
library(here)     # For better path handling

# Determine the app directory and set the data path
# Option 1: Using here package (recommended)
data_path <- here::here("data", "hh_size.rds")

# Option 2: Using parent directory notation
# data_path <- "../data/hh_size.rds" 

# Load the data with error handling
tryCatch({
  hh_size <- read_rds(data_path) %>% 
    mutate(tenure = case_when(
      tenure == "Owner" ~ "Homeowner",
      TRUE ~ tenure
    ))
  
  # Create lists for filtering
  cbsa_list <- sort(unique(hh_size$cbsa_title))
  locality_list <- sort(unique(hh_size$name_long))
  year_list <- sort(unique(hh_size$year), decreasing = TRUE)
  tenure_list <- c("All", "Homeowner", "Renter")
  
}, error = function(e) {
  stop(paste("Error loading data file:", e$message, 
             "\nPlease check the path to your data file. If your app is in a subdirectory of your project,",
             "you may need to adjust the path in the app.R file."))
})

# Pre-process data at each level
# State level data
process_state_data <- function(df, selected_years, selected_tenure) {
  state_size <- df %>%
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
    filter(tenure == selected_tenure,
           year %in% selected_years) %>%
    arrange(year, hhsize) %>% 
    group_by(year) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    ungroup()
  
  # Calculate percent change
  if (length(selected_years) > 1) {
    state_size <- state_size %>%
      arrange(hhsize, year) %>%
      group_by(hhsize) %>%
      mutate(pct_change = case_when(
        year == max(selected_years) ~ ((estimate - first(estimate)) / first(estimate)),
        TRUE ~ NA_real_
      )) %>%
      ungroup()
  }
  
  return(state_size)
}

# CBSA level data
process_cbsa_data <- function(df, selected_cbsa, selected_years, selected_tenure) {
  cbsa_size <- df %>% 
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
    filter(cbsa_title == selected_cbsa,
           tenure == selected_tenure,
           year %in% selected_years) %>%
    arrange(year, hhsize) %>% 
    group_by(year) %>% 
    mutate(percent = estimate/sum(estimate)) %>%
    ungroup()
  
  # Calculate percent change
  if (length(selected_years) > 1) {
    cbsa_size <- cbsa_size %>%
      arrange(hhsize, year) %>%
      group_by(hhsize) %>%
      mutate(pct_change = case_when(
        year == max(selected_years) ~ ((estimate - first(estimate)) / first(estimate)),
        TRUE ~ NA_real_
      )) %>%
      ungroup()
  }
  
  return(cbsa_size)
}

# Locality level data
process_locality_data <- function(df, selected_locality, selected_years, selected_tenure) {
  locality_size <- df %>% 
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
    filter(name_long == selected_locality,
           tenure == selected_tenure,
           year %in% selected_years) %>%
    arrange(year, hhsize) %>% 
    group_by(year) %>% 
    mutate(percent = estimate/sum(estimate)) %>% 
    ungroup()
  
  # Calculate percent change
  if (length(selected_years) > 1) {
    locality_size <- locality_size %>%
      arrange(hhsize, year) %>%
      group_by(hhsize) %>%
      mutate(pct_change = case_when(
        year == max(selected_years) ~ ((estimate - first(estimate)) / first(estimate)),
        TRUE ~ NA_real_
      )) %>%
      ungroup()
  }
  
  return(locality_size)
}

# Comparison data processor
process_comparison_data <- function(df, compare_type, locality1, locality2, year1, year2, selected_tenure) {
  if (compare_type == "Years") {
    # Compare the same locality across different years
    data1 <- df %>% 
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
      filter(name_long == locality1, 
             year == year1,
             tenure == selected_tenure) %>%
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      mutate(comparison = paste(year1))
    
    data2 <- df %>% 
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
      filter(name_long == locality1, 
             year == year2,
             tenure == selected_tenure) %>%
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      mutate(comparison = paste(year2))
    
    bind_rows(data1, data2)
  } else {
    # Compare different localities in the same year
    data1 <- df %>% 
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
      filter(name_long == locality1, 
             year == year1,
             tenure == selected_tenure) %>%
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      mutate(comparison = locality1)
    
    data2 <- df %>% 
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
      filter(name_long == locality2, 
             year == year1,
             tenure == selected_tenure) %>%
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      mutate(comparison = locality2)
    
    bind_rows(data1, data2)
  }
}

# Create interactive plot
create_interactive_plot <- function(data, years, title_text, show_pct_change = TRUE) {
  if (length(years) == 1) {
    # Single year plot
    p <- ggplot(data,
                aes(x = hhsize,
                    y = percent,
                    fill = factor(year))) + 
      geom_col_interactive(
        aes(tooltip = paste0("Household size ", hhsize, ": ", 
                             scales::percent(percent, accuracy = 0.1),
                             "\nEstimate: ", format(estimate, big.mark = ",")))
      ) +
      geom_text_interactive(
        aes(label = scales::percent(percent, accuracy = 0.1),
            tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
        position = position_stack(),
        vjust = -0.5,
        size = 3.5
      )
  } else {
    # Multiple years comparison plot
    p <- ggplot(data,
                aes(x = hhsize,
                    y = estimate,
                    fill = factor(year))) + 
      geom_col_interactive(
        aes(tooltip = paste0("Household size ", hhsize, ": ", 
                             format(estimate, big.mark = ","),
                             " households (",
                             scales::percent(percent, accuracy = 0.1), ")")),
        position = "dodge"
      )
    
    # Add percent change labels if requested and if we have multiple years
    if (show_pct_change) {
      # Calculate percent change
      pct_change_data <- data %>%
        arrange(hhsize, year) %>%
        group_by(hhsize) %>%
        summarize(
          year = max(year),
          estimate = last(estimate),
          percent = last(percent),
          pct_change = (last(estimate) - first(estimate)) / first(estimate),
          .groups = "drop"
        )
      
      p <- p + 
        geom_text_interactive(
          data = pct_change_data,
          aes(label = scales::percent(pct_change, accuracy = 0.1),
              y = estimate + max(data$estimate) * 0.05,
              tooltip = paste0("Change since ", min(years), ": ", 
                               scales::percent(pct_change, accuracy = 0.1))),
          position = position_dodge(width = 0.9),
          size = 3
        )
    }
  }
  
  # Complete the plot with common elements
  p <- p +
    labs(title = title_text,
         x = "Household Size",
         y = ifelse(length(years) == 1, "Percent of Households", "Number of Households")) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      strip.background = element_blank(),
      plot.title = element_markdown(),
      legend.title = element_blank(),
      legend.position = "top"
    )
  
  # Set appropriate y-axis scale
  if (length(years) == 1) {
    p <- p + scale_y_continuous(labels = scales::percent_format())
  } else {
    p <- p + scale_y_continuous(labels = scales::comma_format())
  }
  
  # Create the interactive plot
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
}

# Normalize UI to match the screenshot
ui <- fluidPage(
  # App title and styling
  titlePanel("Virginia Household Size Distribution"),
  
  # Add CSS for better styling
  tags$head(
    tags$style(HTML("
        .well { background-color: #f8f9fa; border-color: #ddd; }
        .shiny-download-link { width: 100%; margin-bottom: 10px; }
        .nav-tabs { margin-bottom: 15px; }
        h2 { color: #011E41; }
        .header-panel { padding: 15px; margin-bottom: 15px; background-color: #f8f9fa; border-radius: 5px; }
        .btn-default { width: 100%; margin-bottom: 10px; }
      "))
  ),
  
  # Add header info
  div(class = "header-panel",
      h4("About This Dashboard"),
      p("This dashboard displays household size distribution data from the American Community Survey (ACS).
           Data is available for the state of Virginia, Core-Based Statistical Areas (CBSAs), and all Virginia localities."),
      p("Source: U.S. Census Bureau, American Community Survey 5-year estimates, Table B09021.")
  ),
  
  # Create tabs for different geographic levels
  tabsetPanel(id = "tabs",
              # Tab 1: Statewide
              tabPanel("Statewide", value = "state",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("state_tenure", 
                                       "Select Tenure:", 
                                       choices = tenure_list,
                                       selected = "All"),
                           selectInput("state_year1", 
                                       "First Year:", 
                                       choices = year_list,
                                       selected = min(year_list)),
                           selectInput("state_year2", 
                                       "Second Year:", 
                                       choices = year_list,
                                       selected = max(year_list)),
                           hr(),
                           downloadButton("download_state", "Download Plot", class = "btn-default"),
                           hr(),
                           helpText("This visualization shows household size distribution across Virginia.")
                         ),
                         mainPanel(
                           girafeOutput("state_plot", height = "600px"),
                           hr(),
                           dataTableOutput("state_table")
                         )
                       )
              ),
              
              # Tab 2: CBSA
              tabPanel("CBSA", value = "cbsa",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("cbsa", 
                                       "Select CBSA:", 
                                       choices = cbsa_list,
                                       selected = cbsa_list[1]),
                           selectInput("cbsa_tenure", 
                                       "Select Tenure:", 
                                       choices = tenure_list,
                                       selected = "All"),
                           selectInput("cbsa_year1", 
                                       "First Year:", 
                                       choices = year_list,
                                       selected = min(year_list)),
                           selectInput("cbsa_year2", 
                                       "Second Year:", 
                                       choices = year_list,
                                       selected = max(year_list)),
                           hr(),
                           downloadButton("download_cbsa", "Download Plot", class = "btn-default"),
                           hr(),
                           helpText("Core-Based Statistical Areas (CBSAs) represent metropolitan regions.")
                         ),
                         mainPanel(
                           girafeOutput("cbsa_plot", height = "600px"),
                           hr(),
                           dataTableOutput("cbsa_table")
                         )
                       )
              ),
              
              # Tab 3: Locality
              tabPanel("Locality", value = "locality",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("locality", 
                                       "Select Locality:", 
                                       choices = locality_list,
                                       selected = locality_list[1]),
                           selectInput("locality_tenure", 
                                       "Select Tenure:", 
                                       choices = tenure_list,
                                       selected = "All"),
                           selectInput("locality_year1", 
                                       "First Year:", 
                                       choices = year_list,
                                       selected = min(year_list)),
                           selectInput("locality_year2", 
                                       "Second Year:", 
                                       choices = year_list,
                                       selected = max(year_list)),
                           hr(),
                           downloadButton("download_locality", "Download Plot", class = "btn-default"),
                           hr(),
                           helpText("Localities are counties and independent cities in Virginia.")
                         ),
                         mainPanel(
                           girafeOutput("locality_plot", height = "600px"),
                           hr(),
                           dataTableOutput("locality_table")
                         )
                       )
              ),
              
              # Tab 4: Compare
              tabPanel("Compare", value = "compare",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("compare_tenure", 
                                       "Select Tenure:", 
                                       choices = tenure_list,
                                       selected = "All"),
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
                                         selected = min(year_list)),
                             selectInput("compare_year2", 
                                         "Second Year:", 
                                         choices = year_list,
                                         selected = max(year_list))
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
                           downloadButton("download_compare", "Download Plot", class = "btn-default"),
                           hr(),
                           helpText("Compare household size distribution across different years or localities.")
                         ),
                         mainPanel(
                           girafeOutput("compare_plot", height = "600px"),
                           hr(),
                           dataTableOutput("compare_table")
                         )
                       )
              )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for state
  state_data <- reactive({
    # Get the selected years from the dropdowns
    selected_years <- unique(c(input$state_year1, input$state_year2))
    process_state_data(hh_size, as.numeric(selected_years), input$state_tenure)
  })
  
  # Reactive data for CBSA
  cbsa_data <- reactive({
    # Get the selected years from the dropdowns
    selected_years <- unique(c(input$cbsa_year1, input$cbsa_year2))
    process_cbsa_data(hh_size, input$cbsa, as.numeric(selected_years), input$cbsa_tenure)
  })
  
  # Reactive data for locality
  locality_data <- reactive({
    # Get the selected years from the dropdowns
    selected_years <- unique(c(input$locality_year1, input$locality_year2))
    process_locality_data(hh_size, input$locality, as.numeric(selected_years), input$locality_tenure)
  })
  
  # Reactive data for comparison
  comparison_data <- reactive({
    if(input$compare_type == "Years") {
      process_comparison_data(
        hh_size, 
        input$compare_type,
        input$compare_locality,
        NULL,
        as.numeric(input$compare_year1),
        as.numeric(input$compare_year2),
        input$compare_tenure
      )
    } else {
      process_comparison_data(
        hh_size, 
        input$compare_type,
        input$compare_locality1,
        input$compare_locality2,
        as.numeric(input$compare_year),
        NULL,
        input$compare_tenure
      )
    }
  })
  
  # Generate plot titles
  state_title <- reactive({
    selected_years <- unique(c(input$state_year1, input$state_year2))
    years_text <- if(length(selected_years) == 1) {
      paste("(", selected_years, ")")
    } else {
      paste("(", min(selected_years), "-", max(selected_years), ")")
    }
    paste("<span style='color:#011E41'>", input$state_tenure, "households by size - Virginia</span> ", years_text)
  })
  
  cbsa_title <- reactive({
    selected_years <- unique(c(input$cbsa_year1, input$cbsa_year2))
    years_text <- if(length(selected_years) == 1) {
      paste("(", selected_years, ")")
    } else {
      paste("(", min(selected_years), "-", max(selected_years), ")")
    }
    paste("<span style='color:#011E41'>", input$cbsa_tenure, "households by size -", input$cbsa, "</span> ", years_text)
  })
  
  locality_title <- reactive({
    selected_years <- unique(c(input$locality_year1, input$locality_year2))
    years_text <- if(length(selected_years) == 1) {
      paste("(", selected_years, ")")
    } else {
      paste("(", min(selected_years), "-", max(selected_years), ")")
    }
    paste("<span style='color:#011E41'>", input$locality_tenure, "households by size -", input$locality, "</span> ", years_text)
  })
  
  compare_title <- reactive({
    if(input$compare_type == "Years") {
      paste("<span style='color:#011E41'>", input$compare_tenure, "households by size -", 
            input$compare_locality, ":</span> Comparing", input$compare_year1, "vs", input$compare_year2)
    } else {
      paste("<span style='color:#011E41'>", input$compare_tenure, "households by size (", 
            input$compare_year, "):</span> Comparing", input$compare_locality1, "vs", input$compare_locality2)
    }
  })
  
  # Render plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(
      state_data(), 
      as.numeric(input$state_years),
      state_title()
    )
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(
      cbsa_data(), 
      as.numeric(input$cbsa_years),
      cbsa_title()
    )
  })
  
  output$locality_plot <- renderGirafe({
    create_interactive_plot(
      locality_data(), 
      as.numeric(input$locality_years),
      locality_title()
    )
  })
  
  output$compare_plot <- renderGirafe({
    if(input$compare_type == "Years") {
      years <- c(as.numeric(input$compare_year1), as.numeric(input$compare_year2))
    } else {
      years <- c(as.numeric(input$compare_year))
    }
    
    comp_data <- comparison_data()
    
    if(input$compare_type == "Years") {
      # For Years comparison, add percent change calculation
      comp_data <- comp_data %>%
        group_by(comparison) %>%
        mutate(percent = estimate/sum(estimate)) %>%
        ungroup() %>%
        arrange(hhsize, comparison) %>%
        group_by(hhsize) %>%
        mutate(pct_change = case_when(
          comparison == as.character(max(years)) ~ 
            ((estimate - first(estimate)) / first(estimate)),
          TRUE ~ NA_real_
        )) %>%
        ungroup()
      
      p <- ggplot(comp_data,
                  aes(x = hhsize,
                      y = percent,
                      fill = comparison)) + 
        geom_col_interactive(
          aes(tooltip = paste0(hhsize, "-person household: ", 
                               scales::percent(percent, accuracy = 0.1),
                               "\nEstimate: ", format(estimate, big.mark = ","))),
          position = "dodge",
          hover_nearest = TRUE
        ) +
        # Add percent change labels
        geom_text_interactive(
          data = filter(comp_data, !is.na(pct_change)),
          aes(label = scales::percent(pct_change, accuracy = 0.1),
              y = percent + 0.05,
              tooltip = paste0("Change since ", min(years), ": ", 
                               scales::percent(pct_change, accuracy = 0.1))),
          position = position_dodge(width = 0.9),
          size = 3
        )
    } else {
      # For Localities comparison
      comp_data <- comp_data %>%
        group_by(comparison) %>%
        mutate(percent = estimate/sum(estimate)) %>%
        ungroup()
      
      p <- ggplot(comp_data,
                  aes(x = hhsize,
                      y = percent,
                      fill = comparison)) + 
        geom_col_interactive(
          aes(tooltip = paste0(hhsize, "-person household: ", 
                               scales::percent(percent, accuracy = 0.1),
                               "\nEstimate: ", format(estimate, big.mark = ","))),
          position = "dodge",
          hover_nearest = TRUE
        ) +
        geom_text_interactive(
          aes(label = scales::percent(percent, accuracy = 0.1),
              tooltip = paste0("Count: ", format(estimate, big.mark = ","))),
          position = position_dodge(width = 0.9),
          vjust = -0.5,
          size = 3
        )
    }
    
    p <- p +
      labs(title = compare_title(),
           x = "Household Size",
           y = "Percent of Households") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#011E41", "#40C0C0")) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title = element_markdown(),
        legend.title = element_blank()
      )
    
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
  
  # Render data tables
  output$state_table <- renderDataTable({
    state_data() %>%
      select(year, hhsize, estimate, percent) %>%
      mutate(percent = scales::percent(percent, accuracy = 0.1)) %>%
      arrange(year, hhsize)
  })
  
  output$cbsa_table <- renderDataTable({
    cbsa_data() %>%
      select(year, cbsa_title, hhsize, estimate, percent) %>%
      mutate(percent = scales::percent(percent, accuracy = 0.1)) %>%
      arrange(year, hhsize)
  })
  
  output$locality_table <- renderDataTable({
    locality_data() %>%
      select(year, name_long, hhsize, estimate, percent) %>%
      mutate(percent = scales::percent(percent, accuracy = 0.1)) %>%
      arrange(year, hhsize)
  })
  
  output$compare_table <- renderDataTable({
    comparison_data() %>%
      select(comparison, hhsize, estimate, percent) %>%
      mutate(percent = scales::percent(percent, accuracy = 0.1)) %>%
      arrange(comparison, hhsize)
  })
  
  # Helper function to create static plots for downloads
  create_static_plot <- function(data, title_text, years) {
    if (length(years) == 1) {
      # Single year plot
      p <- ggplot(data,
                  aes(x = hhsize,
                      y = percent,
                      fill = factor(year))) + 
        geom_col() +
        geom_text(
          aes(label = scales::percent(percent, accuracy = 0.1)),
          position = position_stack(),
          vjust = -0.5,
          size = 3.5
        )
    } else {
      # Multiple years comparison plot
      p <- ggplot(data,
                  aes(x = hhsize,
                      y = estimate,
                      fill = factor(year))) + 
        geom_col(position = "dodge")
      
      # Add percent change labels if we have multiple years
      p <- p + 
        geom_text(
          data = filter(data, year == max(years) & !is.na(pct_change)),
          aes(label = scales::percent(pct_change, accuracy = 0.1),
              y = estimate + max(data$estimate) * 0.05),
          position = position_dodge(width = 0.9),
          size = 3
        )
    }
    
    # Complete the plot with common elements
    p <- p +
      labs(title = gsub("<.*?>", "", title_text),  # Remove HTML tags for static plot
           x = "Household Size",
           y = ifelse(length(years) == 1, "Percent of Households", "Number of Households"),
           caption = "Source: ACS 5-year estimates") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top"
      )
    
    # Set appropriate y-axis scale
    if (length(years) == 1) {
      p <- p + scale_y_continuous(labels = scales::percent_format())
    } else {
      p <- p + scale_y_continuous(labels = scales::comma_format())
    }
    
    return(p)
  }
  
  # Download handlers
  output$download_state <- downloadHandler(
    filename = function() {
      selected_years <- unique(c(input$state_year1, input$state_year2))
      paste("virginia-household-size-", 
            input$state_tenure, "-", 
            paste(selected_years, collapse = "-"), 
            ".png", sep = "")
    },
    content = function(file) {
      selected_years <- unique(c(as.numeric(input$state_year1), as.numeric(input$state_year2)))
      p <- create_static_plot(state_data(), state_title(), selected_years)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_cbsa <- downloadHandler(
    filename = function() {
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$cbsa)
      selected_years <- unique(c(input$cbsa_year1, input$cbsa_year2))
      paste(clean_name, "-household-size-", 
            input$cbsa_tenure, "-", 
            paste(selected_years, collapse = "-"), 
            ".png", sep = "")
    },
    content = function(file) {
      selected_years <- unique(c(as.numeric(input$cbsa_year1), as.numeric(input$cbsa_year2)))
      p <- create_static_plot(cbsa_data(), cbsa_title(), selected_years)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_locality <- downloadHandler(
    filename = function() {
      clean_name <- gsub("[^a-zA-Z0-9]", "-", input$locality)
      selected_years <- unique(c(input$locality_year1, input$locality_year2))
      paste(clean_name, "-household-size-", 
            input$locality_tenure, "-", 
            paste(selected_years, collapse = "-"), 
            ".png", sep = "")
    },
    content = function(file) {
      selected_years <- unique(c(as.numeric(input$locality_year1), as.numeric(input$locality_year2)))
      p <- create_static_plot(locality_data(), locality_title(), selected_years)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_compare <- downloadHandler(
    filename = function() {
      if (input$compare_type == "Years") {
        clean_name <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality)
        paste(clean_name, "-comparison-", 
              input$compare_tenure, "-",
              input$compare_year1, "-vs-", 
              input$compare_year2, ".png", sep = "")
      } else {
        clean_name1 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality1)
        clean_name2 <- gsub("[^a-zA-Z0-9]", "-", input$compare_locality2)
        paste(clean_name1, "-vs-", clean_name2, "-", 
              input$compare_tenure, "-",
              input$compare_year, ".png", sep = "")
      }
    },
    content = function(file) {
      comp_data <- comparison_data()
      
      if(input$compare_type == "Years") {
        years <- c(as.numeric(input$compare_year1), as.numeric(input$compare_year2))
        
        # For Years comparison, add percent change calculation
        comp_data <- comp_data %>%
          group_by(comparison) %>%
          mutate(percent = estimate/sum(estimate)) %>%
          ungroup() %>%
          arrange(hhsize, comparison) %>%
          group_by(hhsize) %>%
          mutate(pct_change = case_when(
            comparison == as.character(max(years)) ~ 
              ((estimate - first(estimate)) / first(estimate)),
            TRUE ~ NA_real_
          )) %>%
          ungroup()
        
        p <- ggplot(comp_data,
                    aes(x = hhsize,
                        y = percent,
                        fill = comparison)) + 
          geom_col(position = "dodge") +
          # Add percent change labels
          geom_text(
            data = filter(comp_data, !is.na(pct_change)),
            aes(label = scales::percent(pct_change, accuracy = 0.1),
                y = percent + 0.05),
            position = position_dodge(width = 0.9),
            size = 3
          )
      } else {
        # For Localities comparison
        comp_data <- comp_data %>%
          group_by(comparison) %>%
          mutate(percent = estimate/sum(estimate)) %>%
          ungroup()
        
        p <- ggplot(comp_data,
                    aes(x = hhsize,
                        y = percent,
                        fill = comparison)) + 
          geom_col(position = "dodge") +
          geom_text(
            aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3
          )
      }
      
      p <- p +
        labs(title = gsub("<.*?>", "", compare_title()),  # Remove HTML tags for static plot
             x = "Household Size",
             y = "Percent of Households",
             caption = "Source: ACS 5-year estimates") +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_manual(values = c("#011E41", "#40C0C0")) +
        theme_minimal() +
        theme(
          panel.grid.major.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank()
        )
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)