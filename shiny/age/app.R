library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number formatting

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
      h4("Population by Age Group", style = "margin: 0; color: #011E41;")
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
          
          # Year select with minimal padding
          div(
            style = "margin-bottom: 0;",
            selectInput("year", "Select Year:", 
                        choices = 2010:2023, 
                        selected = 2023, 
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
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: U.S. Census Bureau, Population Estimates Program and Decennial Census.",
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
  pop_age <- reactive({
    readRDS(here("data", "rds", "pop_age.rds"))
  })
  
  # Pre-compute CBSA and state population
  cbsa_pop <- reactive({
    pop_age() %>% 
      group_by(year, cbsa_title, agegroup) %>% 
      summarise(value = sum(value), .groups = "drop")
  })
  
  state_pop <- reactive({
    pop_age() %>% 
      group_by(year, agegroup) %>% 
      summarise(value = sum(value), .groups = "drop")
  })
  
  # Define the order of age groups
  age_order <- c("Under 10", "10 to 17", "18 to 24", "25 to 29", "30 to 34", 
                 "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 and over")
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    sort(unique(pop_age()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(pop_age()$name_long))
  })
  
  # Initialize dropdowns
  observe({
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Filter data for selected locality and year
  filtered_locality <- reactive({
    req(input$locality, input$year)
    
    pop_age() %>%
      filter(name_long == input$locality,
             year == input$year) %>%
      mutate(agegroup = factor(agegroup, levels = age_order))
  })
  
  # Filter data for selected CBSA
  filtered_cbsa <- reactive({
    req(input$cbsa)
    
    cbsa_pop() %>%
      filter(cbsa_title == input$cbsa) %>%
      mutate(agegroup = factor(agegroup, levels = age_order))
  })
  
  # Title text for locality plot
  locality_title <- reactive({
    paste("Population by Age Group in", input$locality)
  })
  
  # Title text for CBSA plot
  cbsa_title <- reactive({
    paste("Population by Age Group in", input$cbsa)
  })
  
  # Function to create an interactive plot for locality (bar chart by age group)
  create_locality_plot <- function(data) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Age Group: ", agegroup, "\n",
        "Population: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = agegroup,
                    y = value,
                    fill = agegroup)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = agegroup),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Under 10" = hfv_colors$sky,
        "10 to 17" = hfv_colors$grass,
        "18 to 24" = hfv_colors$lilac,
        "25 to 29" = hfv_colors$shadow_light,
        "30 to 34" = hfv_colors$shadow,
        "35 to 44" = hfv_colors$berry,
        "45 to 54" = "#D3447E", # Lighter berry
        "55 to 64" = hfv_colors$desert,
        "65 to 74" = "#F08A65", # Lighter desert
        "75 and over" = "#FAC172" # Gold/yellow shade
      )) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = locality_title(),
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = "Population",
        x = NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      )
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    return(p_with_logo)
  }
  
  # Function to create an interactive plot for CBSA and state (stacked bar chart by age group for a specific year)
  create_time_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Filter for the selected year
    filtered_data <- data %>%
      filter(year == input$year)
    
    # Add tooltips to the data
    plot_data <- filtered_data %>%
      mutate(tooltip = paste0(
        "Age Group: ", agegroup, "\n",
        "Population: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = agegroup,
                    y = value,
                    fill = agegroup)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = agegroup),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Under 10" = hfv_colors$sky,
        "10 to 17" = hfv_colors$grass,
        "18 to 24" = hfv_colors$lilac,
        "25 to 29" = hfv_colors$shadow_light,
        "30 to 34" = hfv_colors$shadow,
        "35 to 44" = hfv_colors$berry,
        "45 to 54" = "#D3447E", # Lighter berry
        "55 to 64" = hfv_colors$desert,
        "65 to 74" = "#F08A65", # Lighter desert
        "75 and over" = "#FAC172" # Gold/yellow shade
      )) +
      scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = "Population",
        x = "Age Group"
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      )
    
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
  
  # Render the state plot
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_time_plot(state_pop(), "Virginia Population by Age Group"))
  })
  
  # Render the CBSA plot
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_time_plot(filtered_cbsa(), cbsa_title()))
  })
  
  # Render the local plot
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_locality_plot(filtered_locality()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)