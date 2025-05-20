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
      h4("Housing Units by Structure Type", style = "margin: 0; color: #011E41;")
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
              "Source: U.S. Census Bureau, American Community Survey, Table B25127.",
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
  b25127 <- reactive({
    readRDS(here("data", "rds", "b25127.rds"))
  })
  
  # Define structure order
  structure_order <- c("1, detached or attached", "2 to 4", "5 to 19", "20 to 49", 
                       "50 or more", "Mobile home, boat, RV, van, etc.")
  
  # Pre-compute datasets
  state_data <- reactive({
    b25127() %>% 
      group_by(year, tenure, structure) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year) %>% 
      mutate(percent = estimate/sum(estimate)) %>%
      ungroup() %>% 
      mutate(structure = factor(structure, levels = structure_order))
  })
  
  cbsa_data <- reactive({
    b25127() %>% 
      group_by(year, cbsa_title, tenure, structure) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>% 
      mutate(structure = factor(structure, levels = structure_order))
  })
  
  locality_data <- reactive({
    b25127() %>% 
      group_by(year, name_long, tenure, structure) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, name_long) %>% 
      mutate(percent = estimate/sum(estimate)) %>% 
      ungroup() %>% 
      mutate(structure = factor(structure, levels = structure_order))
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
  
  # Function to create interactive stacked bar chart
  create_structure_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Determine whether to use percent or count
    if (input$displayType == "percent") {
      plot_data <- data %>%
        mutate(value = percent,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Percent: ", scales::percent(percent, accuracy = 0.1)
               ))
      y_label <- "Percent of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::percent_format())
    } else {
      plot_data <- data %>%
        mutate(value = estimate,
               tooltip = paste0(
                 "Structure Type: ", structure, "\n",
                 "Tenure: ", tenure, "\n",
                 "Units: ", format(estimate, big.mark = ",")
               ))
      y_label <- "Number of Housing Units"
      y_scale <- scale_y_continuous(labels = scales::number_format(big.mark = ","))
    }
    
    # Create base plot
    p <- ggplot(plot_data, 
                aes(x = structure,
                    y = value,
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = interaction(structure, tenure)),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "Homeowner" = hfv_colors$shadow,
        "Renter" = hfv_colors$sky
      )) +
      y_scale +
      labs(
        title = title_text,
        subtitle = paste("Year:", input$year),
        caption = " ", # Add empty caption to leave space for logo
        y = y_label,
        x = NULL,
        fill = "Tenure"
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
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
    "Virginia Housing Units by Structure Type"
  })
  
  cbsa_title <- reactive({
    paste("Housing Units by Structure Type in", input$cbsa)
  })
  
  locality_title <- reactive({
    paste("Housing Units by Structure Type in", input$locality)
  })
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_structure_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_structure_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_structure_plot(filtered_locality(), locality_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
