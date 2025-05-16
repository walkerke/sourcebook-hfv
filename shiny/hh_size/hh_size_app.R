library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(shinyjs)     # For dynamic UI updates
library(cowplot)     # For adding logo to plots

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",  # Lighter shade of shadow color
  berry = "#B1005F",
  desert = "#E0592A"
)

# Create a Bootstrap theme
hfv_theme <- bs_theme(
  version = 5,                        # Use Bootstrap 5
  bg = "#ffffff",                     # Background color
  fg = "#333333",                     # Text color
  primary = hfv_colors$sky,           # Primary color
  secondary = hfv_colors$shadow,      # Secondary color
  success = hfv_colors$grass,         # Success color
  info = hfv_colors$lilac,            # Info color
  warning = hfv_colors$desert,        # Warning color
  danger = hfv_colors$berry,          # Danger color
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins"),
  font_scale = 0.8                    # Compact the text more for small window
)

# Define UI
ui <- page_fluid(
  theme = hfv_theme,
  useShinyjs(),  # Initialize shinyjs
  
  # Fixed dimensions and border
  tags$div(
    style = "width: 800px; height: 500px; margin: 0 auto; border: 2px solid #011E41; border-radius: 5px; overflow: hidden; padding: 10px;",
    
    # Header with logo and title
    div(
      style = "display: flex; align-items: center; margin-bottom: 10px; border-bottom: 2px solid #40C0C0; padding-bottom: 5px;",
      img(src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png", 
          height = "30px", style = "margin-right: 10px;"),
      h4("Household Distribution by Size", style = "margin: 0; color: #011E41;")
    ),
    
    # Main content area with reduced margins
    div(
      style = "height: 435px; overflow: hidden;",
      
      # Use layout_columns for a compact layout
      layout_columns(
        col_widths = c(3, 9),
        gap = "10px",
        
        # Sidebar Panel (more compact) - now with the lighter background color
        card(
          height = "435px",
          padding = "8px",  # Reduced from 10px
          margin = 0,
          full_screen = FALSE,
          style = "background-color: #E8EDF2;",  # Light shade derived from shadow color
          
          # Common filters for all tabs
          div(
            style = "margin-bottom: 0;",
            selectInput("tenure", "Tenure:", 
                        choices = c("All", "Homeowner", "Renter"),
                        selected = "All",
                        width = "100%", 
                        selectize = FALSE)
          ),
          
          # Year range selectors
          div(
            style = "margin-bottom: 0;",
            layout_columns(
              col_widths = c(6, 6),
              gap = "2px",
              selectInput("year_start", "Start Year:", 
                          choices = NULL, 
                          selected = NULL, 
                          width = "100%",
                          selectize = FALSE),
              selectInput("year_end", "End Year:", 
                          choices = NULL, 
                          selected = NULL, 
                          width = "100%",
                          selectize = FALSE)
            )
          ),
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa", "Metro Area:", choices = NULL, width = "100%", selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local'",
              selectInput("locality", "Locality:", choices = NULL, width = "100%", selectize = FALSE)
            )
          ),
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B25009.",
              style = "margin-bottom: 0;"
            )
          )
        ),
        
        # Main Panel (tabs)
        card(
          height = "435px",
          padding = 0,
          margin = 0,
          full_screen = FALSE,
          
          navset_tab(
            id = "tabs",
            nav_panel(
              title = "State", 
              value = "state",
              padding = 5,
              girafeOutput("state_plot", height = "390px")
            ),
            nav_panel(
              title = "Metro Area", 
              value = "cbsa",
              padding = 5,
              girafeOutput("cbsa_plot", height = "390px")
            ),
            nav_panel(
              title = "Locality", 
              value = "local",
              padding = 5,
              girafeOutput("local_plot", height = "390px")
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  # Load the data
  hh_size <- reactive({
    readRDS(here("data", "rds", "hh_size.rds")) %>% 
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Create a list of all unique CBSAs and localities in Virginia
  cbsa_list <- reactive({
    sort(unique(hh_size()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(hh_size()$name_long))
  })
  
  year_list <- reactive({
    sort(unique(hh_size()$year))
  })
  
  # Initialize dropdowns
  observe({
    years <- year_list()
    updateSelectInput(session, "year_start", 
                      choices = years,
                      selected = min(years))
    updateSelectInput(session, "year_end", 
                      choices = years,
                      selected = max(years))
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Ensure end year is not earlier than start year
  observe({
    req(input$year_start, input$year_end)
    if (!is.null(input$year_start) && !is.null(input$year_end)) {
      if (as.numeric(input$year_start) > as.numeric(input$year_end)) {
        updateSelectInput(session, "year_end", selected = input$year_start)
      }
    }
  })
  
  # Pre-process data - Locality data
  locality_size <- reactive({
    hh_size() %>% 
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
  })
  
  # CBSA data  
  cbsa_size <- reactive({
    hh_size() %>% 
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
  })
  
  # State data
  state_size <- reactive({
    hh_size() %>%
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
  })
  
  # Create filtered datasets based on user selections
  state_filtered <- reactive({
    req(input$year_start, input$year_end, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- state_size() %>%
      filter(tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  cbsa_filtered <- reactive({
    req(input$year_start, input$year_end, input$cbsa, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- cbsa_size() %>%
      filter(cbsa_title == input$cbsa,
             tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  locality_filtered <- reactive({
    req(input$year_start, input$year_end, input$locality, input$tenure)
    years <- c(input$year_start, input$year_end)
    
    # Only modify years if start and end are the same
    if(input$year_start == input$year_end) {
      years_list <- year_list()
      # If same year is selected for both, add another year to compare with
      if(as.numeric(input$year_start) == min(years_list)) {
        # If it's the earliest year, use the next available year
        if(length(years_list) > 1) {
          next_year <- sort(years_list)[2]
          years <- c(input$year_start, next_year)
        }
      } else {
        # Otherwise use the previous year
        year_pos <- which(years_list == input$year_start)
        if(length(year_pos) > 0 && year_pos > 1) {
          prev_year <- sort(years_list)[year_pos - 1]
          years <- c(prev_year, input$year_start)
        }
      }
    }
    
    # Filter the data
    filtered_data <- locality_size() %>%
      filter(name_long == input$locality,
             tenure == input$tenure,
             year %in% years) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, hhsize)
    
    # Add percent change if we have multiple years
    if(length(unique(filtered_data$year)) > 1) {
      filtered_data <- filtered_data %>%
        group_by(hhsize) %>%
        mutate(pct_change = ifelse(year == max(year), 
                                   (estimate - estimate[year == min(year)]) / 
                                     estimate[year == min(year)],
                                   NA_real_)) %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        mutate(pct_change = NA_real_)
    }
    
    # Add tooltip data
    filtered_data %>%
      mutate(tooltip = paste0(
        "Household Size: ", hhsize, "\n",
        "Year: ", year, "\n",
        "Households: ", format(estimate, big.mark = ","), 
        ifelse(!is.na(pct_change) & year == max(year), 
               paste0("\nChange: ", scales::percent(pct_change, accuracy = 0.1)), 
               "")
      ))
  })
  
  # Create title text
  title_text <- reactive({
    if (input$tabs == "state") {
      paste(input$tenure, "Households by Size in Virginia")
    } else if (input$tabs == "cbsa") {
      paste(input$tenure, "Households by Size in", input$cbsa)
    } else {
      paste(input$tenure, "Households by Size in", input$locality)
    }
  })
  
  subtitle_text <- reactive({
    earliest_year <- input$year_start
    latest_year <- input$year_end
    paste("Comparing", earliest_year, "to", latest_year)
  })
  
  # Function to create an interactive plot
  create_interactive_plot <- function(data) {
    req(nrow(data) > 0)
    
    latest_year <- max(as.character(data$year))
    
    # Create base plot
    p <- ggplot(data, aes(x = year, y = estimate, fill = year)) + 
      geom_col_interactive(aes(tooltip = tooltip, data_id = paste(hhsize, year))) +
      facet_wrap(~hhsize, nrow = 1, scales = "free_y") +
      # Add percent change text only if we have data for multiple years
      {
        if(length(unique(data$year)) > 1)
          geom_text(
            data = filter(data, year == latest_year & !is.na(pct_change)),
            aes(label = scales::percent(pct_change, accuracy = 0.1)),
            position = position_stack(),
            vjust = -0.5,
            size = 3
          )
      } +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      scale_fill_manual(values = c(hfv_colors$sky, hfv_colors$shadow)) +
      labs(
        title = title_text(),
        subtitle = subtitle_text(),
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),  # Tighter spacing for compact view
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 8),  # Smaller facet titles
        axis.text = element_text(size = 8),   # Smaller axis text
        axis.title = element_blank(),         # Remove axis titles
        plot.title = element_text(size = 11), # Smaller plot title
        plot.subtitle = element_text(size = 9), # Smaller subtitle
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      )
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    # Return interactive plot with logo
    girafe(
      ggobj = p_with_logo,
      width_svg = 7.5,
      height_svg = 4.5,
      options = list(
        opts_hover(css = "fill-opacity:0.8;"),
        opts_tooltip(
          opacity = 0.9, 
          css = "background-color:#011E41;color:white;padding:8px;border-radius:3px;",
          use_fill = TRUE
        ),
        opts_toolbar(hidden = c("lasso_deselect", "lasso_select")),
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
  }
  
  # Render plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(state_filtered())
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(cbsa_filtered())
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(locality_filtered())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)