library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For dollar_format

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

# Load data outside of server
state_inc_age_data <- read_rds(here("data", "rds", "b19049_state.rds"))
cbsa_inc_age_data <- read_rds(here("data", "rds", "b19049_cbsa.rds"))
local_inc_age_data <- read_rds(here("data", "rds", "b19049_locality.rds"))

# Create lists for filters
state_list <- sort(unique(state_inc_age_data$state))
cbsa_list <- sort(unique(cbsa_inc_age_data$cbsa))
locality_list <- sort(unique(local_inc_age_data$locality))

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
      h4("Median Household Income by Householder Age", style = "margin: 0; color: #011E41;")
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
          
          # Dollar type selector
          div(
            style = "margin-bottom: 0;",
            radioButtons("dollar_type", "Dollar Type:",
                         choices = list("Current Dollars" = "estimate", 
                                        "Inflation-Adjusted Dollars" = "adjusted"),
                         selected = "estimate",
                         inline = FALSE)
          ),
          
          # Geography selectors
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'state'",
              selectInput("state_select", "Select State:", 
                          choices = state_list,
                          selected = "Virginia",
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa_select", "Metro Area:", 
                          choices = cbsa_list,
                          selected = "Richmond, VA Metro Area",
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local'",
              selectInput("local_select", "Locality:", 
                          choices = locality_list,
                          selected = "Chesterfield County",
                          width = "100%", 
                          selectize = FALSE)
            )
          ),
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B19049.",
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
  
  # Create filtered datasets
  filtered_state <- reactive({
    req(input$state_select)
    state_inc_age_data %>%
      filter(state == input$state_select)
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select)
    cbsa_inc_age_data %>%
      filter(cbsa == input$cbsa_select)
  })
  
  filtered_local <- reactive({
    req(input$local_select)
    local_inc_age_data %>%
      filter(locality == input$local_select) %>%
      mutate(
        estimate = as.numeric(estimate),
        adjusted = as.numeric(adjusted)
      )
  })
  
  # Y-axis label based on dollar type
  y_label <- reactive({
    ifelse(input$dollar_type == "adjusted", 
           "Inflation-Adjusted Dollars", 
           "Current Dollars")
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Median Household Income by Age in", input$state_select)
  })
  
  cbsa_title <- reactive({
    paste("Median Household Income by Age in", input$cbsa_select)
  })
  
  local_title <- reactive({
    paste("Median Household Income by Age in", input$local_select)
  })
  
  # Function to create interactive line plots
  create_line_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get selected column for y-axis
    y_var <- input$dollar_type
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Age: ", age, "\n",
        "Income: ", scales::dollar(get(y_var))
      ))
    
    # Age categories
    unique_ages <- unique(plot_data$age)
    
    # Create color mapping with HFV colors
    color_values <- c(
      "Householder under 25 years" = hfv_colors$sky,
      "Householder 25 to 44 years" = hfv_colors$grass,
      "Householder 45 to 64 years" = hfv_colors$shadow,
      "Householder 65 years and over" = hfv_colors$berry
    )
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = year,
                  y = .data[[y_var]],
                  color = age,
                  group = age
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = age),
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, age)),
        size = 3
      ) +
      scale_color_manual(values = color_values) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = title_text,
        y = y_label(),
        x = "Year",
        caption = " " # Add empty caption to leave space for logo
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      )
    
    # Handle different x-axis scales between locality and other levels
    if("locality" %in% names(data)) {
      p <- p + scale_x_discrete(
        breaks = unique(plot_data$year),
        labels = unique(plot_data$year)
      )
    } else {
      p <- p + scale_x_continuous(
        breaks = unique(plot_data$year),
        labels = unique(plot_data$year)
      )
    }
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    return(p_with_logo)
  }
  
  # Convert to interactive girafe for each plot
  create_interactive_plot <- function(plot_obj) {
    girafe(
      ggobj = plot_obj,
      width_svg = 7.5,    # Width in inches
      height_svg = 4.5,   # Height in inches
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
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_line_plot(filtered_local(), local_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)