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
      h4("Household Overcrowding by Tenure", style = "margin: 0; color: #011E41;")
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
                        choices = 2017:2023, 
                        selected = 2023, 
                        width = "100%",
                        selectize = FALSE)
          ),
          
          # Display options
          div(
            style = "margin-bottom: 0;",
            radioButtons("displayType", "Display:", 
                         choices = c("Percent" = "percent", "Count" = "count"),
                         selected = "percent",
                         inline = TRUE)
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
              "Source: U.S. Census Bureau, American Community Survey, Table B25014.",
              style = "margin-bottom: 0;"
            ),
            p(
              "Note: Severely overcrowded = more than 1.5 persons per room. Overcrowded = 1.01 to 1.5 persons per room.",
              style = "margin-bottom: 0; margin-top: 4px; font-size: 9px;"
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
  b25014 <- reactive({
    # Load data and convert "Owner" tenure to "Homeowner"
    readRDS(here("data", "rds", "b25014.rds")) %>%
      mutate(tenure = case_when(
        tenure == "Owner" ~ "Homeowner",
        TRUE ~ tenure
      ))
  })
  
  # Pre-compute datasets
  state_data <- reactive({
    b25014() %>% 
      group_by(year, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  cbsa_data <- reactive({
    b25014() %>% 
      group_by(year, cbsa_title, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  locality_data <- reactive({
    b25014() %>% 
      group_by(year, name_long, tenure, overcrowded) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, name_long, tenure) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>%
      filter(overcrowded != "Not overcrowded")
  })
  
  # Get available CBSAs and localities
  cbsa_list <- reactive({
    sort(unique(cbsa_data()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(locality_data()$name_long))
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
  
  # Filter data for state
  filtered_state <- reactive({
    req(input$year)
    
    state_data() %>%
      filter(year == input$year)
  })
  
  # Filter data for selected CBSA
  filtered_cbsa <- reactive({
    req(input$cbsa, input$year)
    
    cbsa_data() %>%
      filter(cbsa_title == input$cbsa,
             year == input$year)
  })
  
  # Filter data for selected locality
  filtered_locality <- reactive({
    req(input$locality, input$year)
    
    locality_data() %>%
      filter(name_long == input$locality,
             year == input$year)
  })
  
  # Function to create interactive faceted bar chart
  create_overcrowding_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Check if we have data for both tenure types
    tenure_count <- length(unique(data$tenure))
    
    # Determine whether to use percent or count
    if (input$displayType == "percent") {
      plot_data <- data %>%
        mutate(value = percent,
               tooltip = paste0(
                 "Tenure: ", tenure, "\n",
                 "Category: ", overcrowded, "\n",
                 "Percent: ", scales::percent(percent, accuracy = 0.1)
               ))
      y_label <- "Percent of Households"
      y_scale <- scale_y_continuous(labels = scales::percent_format())
    } else {
      plot_data <- data %>%
        mutate(value = estimate,
               tooltip = paste0(
                 "Tenure: ", tenure, "\n",
                 "Category: ", overcrowded, "\n",
                 "Households: ", format(estimate, big.mark = ",")
               ))
      y_label <- "Number of Households"
      y_scale <- scale_y_continuous(labels = scales::number_format(big.mark = ","))
    }
    
    # Color by overcrowding category rather than tenure
    overcrowding_colors <- c(
      "Overcrowded" = hfv_colors$desert,
      "Severely overcrowded" = hfv_colors$berry
    )
    
    # Create base plot with facets by tenure
    p <- ggplot(plot_data, 
                aes(x = overcrowded,
                    y = value,
                    fill = overcrowded)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = interaction(tenure, overcrowded)),
        position = "stack",
        width = 0.7
      ) +
      facet_wrap(~ tenure, nrow = 1, scales = "free_x") +
      scale_fill_manual(values = overcrowding_colors) +
      y_scale +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.background = element_rect(fill = hfv_colors$shadow_light),
        strip.text = element_text(color = "white", face = "bold"),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),  # Hide x-axis labels since they're redundant with legend
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      )
    
    # Add logo to the plot
    logo_path <- "./www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    return(p_with_logo)
  }
  
  # Convert to interactive girafe plot
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
  
  # Set plot titles
  state_title <- reactive({
    "Virginia Household Overcrowding by Tenure"
  })
  
  cbsa_title <- reactive({
    paste("Household Overcrowding by Tenure in", input$cbsa)
  })
  
  locality_title <- reactive({
    paste("Household Overcrowding by Tenure in", input$locality)
  })
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_overcrowding_plot(filtered_locality(), locality_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
