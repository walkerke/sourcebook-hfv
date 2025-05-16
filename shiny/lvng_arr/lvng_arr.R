library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For formatting
library(ggtext)      # For rich text in plots

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
      h4("Living Arrangements of Adults", style = "margin: 0; color: #011E41;")
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
                        choices = NULL, 
                        selected = NULL, 
                        width = "100%",
                        selectize = FALSE)
          ),
          
          # Age group select
          div(
            style = "margin-bottom: 0;",
            selectInput("age", "Select Age Group:", 
                        choices = NULL, 
                        selected = NULL, 
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
  # Load data
  lvng_arr <- reactive({
    readRDS(here("data", "rds", "lvng_arr.rds"))
  })
  
  # Create lists for filters
  year_list <- reactive({
    sort(unique(lvng_arr()$year), decreasing = TRUE)
  })
  
  age_list <- reactive({
    sort(unique(lvng_arr()$age))
  })
  
  cbsa_list <- reactive({
    sort(unique(lvng_arr()$cbsa_title))
  })
  
  locality_list <- reactive({
    sort(unique(lvng_arr()$name_long))
  })
  
  # Pre-aggregate data
  # Locality data
  locality_la <- reactive({
    lvng_arr() %>% 
      group_by(year, name_long, age, type) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>%
      group_by(year, name_long, age) %>% 
      mutate(percent = estimate/sum(estimate))
  })
  
  # CBSA data
  cbsa_la <- reactive({
    lvng_arr() %>% 
      group_by(year, cbsa_title, age, type) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, cbsa_title, age) %>% 
      mutate(percent = estimate/sum(estimate))
  })
  
  # State data
  state_la <- reactive({
    lvng_arr() %>% 
      group_by(year, age, type) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>% 
      group_by(year, age) %>% 
      mutate(percent = estimate/sum(estimate))
  })
  
  # Initialize dropdowns
  observe({
    updateSelectInput(session, "year", 
                      choices = year_list(),
                      selected = max(year_list()))
    
    updateSelectInput(session, "age", 
                      choices = age_list(),
                      selected = "All ages")
    
    # CBSAs
    updateSelectInput(session, "cbsa", 
                      choices = cbsa_list(),
                      selected = if("Richmond, VA" %in% cbsa_list()) "Richmond, VA" else cbsa_list()[1])
    
    # Localities
    updateSelectInput(session, "locality", 
                      choices = locality_list(),
                      selected = if("Richmond City" %in% locality_list()) "Richmond City" else locality_list()[1])
  })
  
  # Filter data for plots
  filtered_state <- reactive({
    req(input$year, input$age)
    
    state_la() %>%
      filter(year == input$year,
             age == input$age)
  })
  
  filtered_cbsa <- reactive({
    req(input$year, input$age, input$cbsa)
    
    cbsa_la() %>%
      filter(year == input$year,
             age == input$age,
             cbsa_title == input$cbsa)
  })
  
  filtered_locality <- reactive({
    req(input$year, input$age, input$locality)
    
    locality_la() %>%
      filter(year == input$year,
             age == input$age,
             name_long == input$locality)
  })
  
  # Create subtitle text
  state_subtitle <- reactive({
    paste("Virginia -", input$year, "-", input$age)
  })
  
  cbsa_subtitle <- reactive({
    paste(input$cbsa, "-", input$year, "-", input$age)
  })
  
  locality_subtitle <- reactive({
    paste(input$locality, "-", input$year, "-", input$age)
  })
  
  # Helper function for creating interactive plots
  create_plot <- function(data, subtitle) {
    req(nrow(data) > 0)
    
    # Add tooltips to the data
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Living Arrangement: ", type, "\n",
        "Percentage: ", scales::percent(percent, accuracy = 0.1), "\n",
        "Count: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = reorder(type, percent),
                    y = percent,
                    fill = type)) + 
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = type)
      ) +
      # Match text color to bar fill color
      geom_text(aes(label = scales::percent(percent, accuracy = 1),
                    color = type),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.5) +
      # Use the HFV colors for fill and text
      scale_fill_manual(values = c(
        "Alone" = hfv_colors$sky,
        "Spouse" = hfv_colors$grass,
        "Unmarried partner" = hfv_colors$lilac,
        "Other nonrelatives only" = hfv_colors$shadow,
        "Child of householder" = hfv_colors$berry,
        "Other relatives of householder" = hfv_colors$desert
      )) +
      scale_color_manual(values = c(
        "Alone" = hfv_colors$sky,
        "Spouse" = hfv_colors$grass,
        "Unmarried partner" = hfv_colors$lilac,
        "Other nonrelatives only" = hfv_colors$shadow,
        "Child of householder" = hfv_colors$berry,
        "Other relatives of householder" = hfv_colors$desert
      )) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Living Arrangements of Adults",
        subtitle = subtitle,
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage",
        x = NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(color = hfv_colors$shadow, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, lineheight = 0.8),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 5, 15, 5) # Extra bottom margin for logo
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
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
    create_interactive_plot(create_plot(filtered_state(), state_subtitle()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_cbsa(), cbsa_subtitle()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_plot(filtered_locality(), locality_subtitle()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)