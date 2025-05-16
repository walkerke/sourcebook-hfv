library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
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
  
  # Fixed dimensions and border
  tags$div(
    style = "width: 800px; height: 500px; margin: 0 auto; border: 2px solid #011E41; border-radius: 5px; overflow: hidden; padding: 10px;",
    
    # Header with logo and title
    div(
      style = "display: flex; align-items: center; margin-bottom: 10px; border-bottom: 2px solid #40C0C0; padding-bottom: 5px;",
      img(src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png", 
          height = "30px", style = "margin-right: 10px;"),
      h4("Average Household Size Over Time", style = "margin: 0; color: #011E41;")
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
          padding = "8px",  # Reduced padding for compactness
          margin = 0,
          full_screen = FALSE,
          style = "background-color: #E8EDF2;",  # Light shade derived from shadow color
          
          # Tenure selector
          div(
            style = "margin-bottom: 0;",
            selectInput("tenure", "Tenure:", 
                        choices = c("All", "Homeowner", "Renter"),
                        selected = "All",
                        width = "100%", 
                        selectize = FALSE)
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
          
          # Year range checkbox (optional feature)
          div(
            style = "margin-bottom: 0;",
            checkboxInput("show_all_years", "Show All Years", value = TRUE)
          ),
          conditionalPanel(
            condition = "!input.show_all_years",
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
          
          # Show trend line option
          div(
            style = "margin-bottom: 0;",
            checkboxInput("show_trend", "Show Trend Line", value = TRUE)
          ),
          
          # Show point labels option
          div(
            style = "margin-bottom: 0;",
            checkboxInput("show_labels", "Show Point Labels", value = FALSE)
          ),
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates.",
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
  avg_size <- reactive({
    read_rds(here("data", "rds", "avg_hh_size.rds")) %>% 
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Get available years
  year_list <- reactive({
    sort(unique(avg_size()$year))
  })
  
  # Get available localities
  locality_list <- reactive({
    avg_size() %>%
      filter(geography == "locality") %>%
      pull(name) %>%
      unique() %>%
      sort()
  })
  
  # Get available CBSAs
  cbsa_list <- reactive({
    avg_size() %>%
      filter(geography == "cbsa") %>%
      pull(name) %>%
      unique() %>%
      sort()
  })
  
  # Initialize dropdowns
  observe({
    # Years
    years <- year_list()
    updateSelectInput(session, "year_start", 
                      choices = years,
                      selected = min(years))
    updateSelectInput(session, "year_end", 
                      choices = years,
                      selected = max(years))
    
    # CBSAs
    cbsas <- cbsa_list()
    updateSelectInput(session, "cbsa", 
                      choices = cbsas,
                      selected = if("Richmond, VA" %in% cbsas) "Richmond, VA" else cbsas[1])
    
    # Localities
    localities <- locality_list()
    updateSelectInput(session, "locality", 
                      choices = localities,
                      selected = if("Richmond City" %in% localities) "Richmond City" else localities[1])
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
  
  # Filter data based on selections
  filtered_state <- reactive({
    req(input$tenure)
    
    data <- avg_size() %>%
      filter(geography == "state",
             tenure == input$tenure)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  filtered_cbsa <- reactive({
    req(input$tenure, input$cbsa)
    
    data <- avg_size() %>%
      filter(geography == "cbsa",
             tenure == input$tenure,
             name == input$cbsa)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  filtered_locality <- reactive({
    req(input$tenure, input$locality)
    
    data <- avg_size() %>%
      filter(geography == "locality",
             tenure == input$tenure,
             name == input$locality)
    
    # Apply year filter if needed
    if (!input$show_all_years) {
      req(input$year_start, input$year_end)
      data <- data %>%
        filter(year >= input$year_start, 
               year <= input$year_end)
    }
    
    # Calculate min/max points for labeling
    data %>%
      mutate(label_point = year == min(year) | year == max(year) | 
               estimate == max(estimate) | estimate == min(estimate))
  })
  
  # Create title text
  title_text <- reactive({
    if (input$tabs == "state") {
      paste(input$tenure, "Average Household Size in Virginia")
    } else if (input$tabs == "cbsa") {
      paste(input$tenure, "Average Household Size in", input$cbsa)
    } else {
      paste(input$tenure, "Average Household Size in", input$locality)
    }
  })
  
  # Subtitle text with year range
  subtitle_text <- reactive({
    if (input$show_all_years) {
      "All Available Years"
    } else {
      paste(input$year_start, "to", input$year_end)
    }
  })
  
  # Function to create an interactive plot
  create_interactive_plot <- function(data) {
    req(nrow(data) > 0)
    
    # Calculate y-axis limits with some padding
    y_min <- min(data$estimate) * 0.95
    y_max <- max(data$estimate) * 1.05
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Average Size: ", format(estimate, nsmall = 2)
      ))
    
    # Add margin of error to tooltip if it exists in the data
    if("moe" %in% colnames(plot_data)) {
      plot_data <- plot_data %>%
        mutate(tooltip = ifelse(
          !is.na(moe),
          paste0(tooltip, "\nMargin of Error: ±", format(moe, nsmall = 2)),
          tooltip
        ))
    }
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = estimate)) +
      # Add interactive line
      geom_line(linewidth = 1, color = hfv_colors$shadow) +
      # Add interactive points
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = year),
        size = 3, 
        color = hfv_colors$shadow
      ) 
    
    # Add trend line if requested
    if (input$show_trend && nrow(data) >= 4) {
      p <- p + geom_smooth(method = "loess", 
                           se = TRUE, 
                           color = hfv_colors$sky, 
                           fill = hfv_colors$sky, 
                           alpha = 0.2)
    }
    
    # Add point labels if requested
    if (input$show_labels) {
      p <- p + geom_text(
        data = filter(plot_data, label_point),
        aes(label = format(estimate, nsmall = 2)),
        vjust = -0.8, 
        hjust = 0.5, 
        size = 3.5
      )
    }
    
    # Complete the plot
    p <- p + 
      scale_y_continuous(limits = c(y_min, y_max),
                         labels = scales::number_format(accuracy = 0.01)) +
      labs(
        title = title_text(),
        subtitle = subtitle_text(),
        x = "Year",
        y = "Average Household Size",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey80", fill = NA),
        legend.position = "none",
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
    create_interactive_plot(filtered_state())
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(filtered_cbsa())
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(filtered_locality())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)