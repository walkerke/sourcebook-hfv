library(shiny)
library(tidyverse)  # Includes dplyr, ggplot2, etc.
library(scales)     # For percent_format
library(ggiraph)    # For interactive ggplots
library(systemfonts) # For font_google
library(here)       # For here() function in file paths
library(grid)       # For grobs
library(png)        # For reading PNG files
library(bslib)      # For modern UI components
library(cowplot)    # For adding logo to plots

# Define HFV color palette
hfv_colors <- list(
  sky = "#40C0C0",
  grass = "#259591",
  lilac = "#8B85CA", 
  shadow = "#011E41",
  shadow_light = "#102C54",  # New lighter shade of shadow color
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
      h4("Housing Type Distribution", style = "margin: 0; color: #011E41;")
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
          padding = "10px",
          margin = 0,
          full_screen = FALSE,
          style = "background-color: #E8EDF2;",  # Light shade derived from shadow color
          
          # Year select with minimal padding
          div(
            style = "margin-bottom: 5px;",
            selectInput("year", "Select Year:", 
                        choices = 2010:2023, 
                        selected = 2023, 
                        width = "100%")
          ),
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 5px;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa", "Metro Area:", choices = NULL, width = "100%")
            ),
            conditionalPanel(
              condition = "input.tabs == 'local'",
              selectInput("locality", "Locality:", choices = NULL, width = "100%")
            )
          ),
          
          # Horizontal line
          hr(style = "margin: 5px 0;"),
          
          # Source information
          div(
            style = "font-size: 11px; color: #666;",
            p(
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B25032.",
              style = "margin-bottom: 5px;"
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

# Server function remains mostly the same
server <- function(input, output, session) {
  # Define the add_logo_to_plot function inside the server function
  add_logo_to_plot <- function(plot) {
    # Path to the logo in parent's www folder
    logo_path <- "C:/Users/ericv/Desktop/github/sourcebook-hfv/shiny/www/hfv_rgb_logo.png"
    
    # Check if the file exists
    if (!file.exists(logo_path)) {
      warning("Logo file not found at ", logo_path, ". Please check the path.")
      return(plot)  # Return original plot if logo not found
    }
    
    # Read the logo with error handling
    logo_img <- tryCatch({
      png::readPNG(logo_path)
    }, error = function(e) {
      warning("Failed to read logo: ", e$message)
      return(NULL)
    })
    
    # If logo couldn't be read, return original plot
    if (is.null(logo_img)) {
      return(plot)
    }
    
    # Convert to grob
    logo_grob <- grid::rasterGrob(
      logo_img, 
      x = unit(0.5, "npc"), 
      y = unit(0.01, "npc"), 
      just = c("center", "bottom"),
      width = unit(0.15, "npc") # Adjust width as needed
    )
    
    # Add logo to plot
    plot + 
      annotation_custom(
        grob = logo_grob,
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -Inf
      )
  }
  
  # Read in data
  b25032 <- reactive({
    read_rds(here("data", "rds", "b25032.rds"))
  })
  
  # Process data for state level
  state_housing <- reactive({
    b25032() %>% 
      group_by(year, tenure, type) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      group_by(year) %>% 
      mutate(percent_total = estimate/sum(estimate))
  })
  
  # Process data for CBSA level
  cbsa_housing <- reactive({
    b25032() %>% 
      group_by(year, cbsa_title, tenure, type) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      group_by(year, cbsa_title) %>% 
      mutate(percent_total = estimate/sum(estimate))
  })
  
  # Process data for local level
  local_housing <- reactive({
    b25032() %>% 
      group_by(year, name_long, tenure, type) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      group_by(year, name_long) %>% 
      mutate(percent_total = estimate/sum(estimate))
  })
  
  # Populate CBSA dropdown
  observe({
    cbsa_choices <- unique(cbsa_housing()$cbsa_title)
    updateSelectInput(session, "cbsa", choices = cbsa_choices, 
                      selected = "Richmond, VA")
  })
  
  # Populate locality dropdown
  observe({
    locality_choices <- unique(local_housing()$name_long)
    updateSelectInput(session, "locality", choices = locality_choices,
                      selected = "Richmond City")
  })
  
  # Filter data based on inputs
  filtered_state <- reactive({
    state_housing() %>% 
      filter(year == input$year)
  })
  
  filtered_cbsa <- reactive({
    cbsa_housing() %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa)
  })
  
  filtered_local <- reactive({
    local_housing() %>% 
      filter(year == input$year,
             name_long == input$locality)
  })
  
  # Create title text
  title_text <- reactive({
    paste("In", input$year)
  })
  
  # Create a simplified plotting approach to completely avoid theme conflicts
  create_plot <- function(data) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Type: ", type, "\n",
        "Tenure: ", tenure, "\n",
        "Percentage: ", scales::percent(percent_total, accuracy = 0.1), "\n",
        "Count: ", format(estimate, big.mark = ",")
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = reorder(type, -percent_total), 
                    y = percent_total, 
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = type),
        position = "dodge"
      ) +
      geom_text(aes(label = scales::percent(percent_total, accuracy = 1), 
                    color = tenure),
                position = position_dodge(width = 0.9),
                hjust = -0.2,
                size = 3.5) + # Reduced text size slightly for compact view
      facet_wrap(~tenure) +
      # Use Housing Forward Virginia colors
      scale_fill_manual(values = c("Owner" = "#40C0C0", "Renter" = "#011E41")) +
      scale_color_manual(values = c("Owner" = "#40C0C0", "Renter" = "#011E41")) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         expand = expansion(mult = c(0, 0.3))) +
      labs(
        title = title_text(),
        caption = " " # Add empty caption to leave space for logo
      ) +
      # Use the most basic theme with minimal customization
      theme_bw() +
      theme(
        legend.position = "none",
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 11), # Smaller text
        axis.text.x = element_text(hjust = 0.1),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        # Make the plot more compact overall
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Get logo and add it to the plot
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15) # Smaller logo
    
    return(p_with_logo)
  }
  
  # Convert to interactive girafe for each plot
  create_interactive_plot <- function(plot_obj) {
    girafe(
      ggobj = plot_obj,
      width_svg = 7.5,    # Width in inches - smaller to fit the window
      height_svg = 4.5,   # Height in inches - smaller to fit the window
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
  
  # Render the state plot
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_state()))
  })
  
  # Render the CBSA plot
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_cbsa()))
  })
  
  # Render the local plot
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_local()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)