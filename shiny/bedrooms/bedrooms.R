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

# Define the bedroom order
bedroom_order <- c("No bedroom", "1 bedroom", "2 bedrooms", "3 bedrooms", 
                   "4 bedrooms", "5 or more bedrooms")

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
      h4("Distribution of Housing by Bedroom Count", style = "margin: 0; color: #011E41;")
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
          
          # Year select with minimal padding
          div(
            style = "margin-bottom: 0;",
            selectInput("year", "Select Year:", 
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
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B25042.",
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
  # Read in the data
  b25042 <- reactive({
    read_rds(here("data", "rds", "b25042.rds"))
  })
  
  # Pre-process the data
  state_bed <- reactive({
    b25042() %>% 
      group_by(year, tenure, br) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>%
      mutate(br = factor(br, levels = bedroom_order))
  })
  
  cbsa_bed <- reactive({
    b25042() %>% 
      group_by(year, cbsa_title, tenure, br) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>%
      mutate(br = factor(br, levels = bedroom_order))
  })
  
  local_bed <- reactive({
    b25042() %>% 
      group_by(year, name_long, tenure, br) %>% 
      summarise(estimate = sum(estimate), .groups = "drop") %>%
      mutate(br = factor(br, levels = bedroom_order))
  })
  
  # Update year filter choices
  observe({
    years <- unique(b25042()$year)
    updateSelectInput(session, "year", 
                      choices = years,
                      selected = max(years))
  })
  
  # Update CBSA filter choices
  observe({
    req(input$year)
    cbsas <- cbsa_bed() %>%
      filter(year == input$year) %>%
      pull(cbsa_title) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "cbsa", 
                      choices = cbsas,
                      selected = if("Richmond, VA" %in% cbsas) "Richmond, VA" else cbsas[1])
  })
  
  # Update locality filter choices
  observe({
    req(input$year)
    localities <- local_bed() %>%
      filter(year == input$year) %>%
      pull(name_long) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "locality", 
                      choices = localities,
                      selected = if("Richmond City" %in% localities) "Richmond City" else localities[1])
  })
  
  # Create title text
  title_text <- reactive({
    paste("Bedroom Distribution in", input$year)
  })
  
  # Create a plot function for bedroom distribution
  create_plot <- function(data) {
    # Add tooltip text to the data
    data <- data %>%
      mutate(tooltip = paste0(
        "Bedrooms: ", br, "\n",
        "Tenure: ", tenure, "\n",
        "Households: ", format(estimate, big.mark = ",")
      ))
    
    # Create a pure, base ggplot with no theme customizations that could cause conflicts
    p <- ggplot(data, 
                aes(x = br, 
                    y = estimate, 
                    fill = tenure)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(br, tenure)),
        position = "stack"
      ) +
      facet_wrap(~tenure, ncol = 1) +
      coord_flip() +  # Flip for horizontal bars
      # Use Housing Forward Virginia colors
      scale_fill_manual(values = c("Owner" = "#40C0C0", "Renter" = "#011E41")) +
      scale_y_continuous(labels = scales::number_format(big.mark = ","),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = title_text(),
        caption = " ", # Add empty caption to leave space for logo
        y = "Number of Households",
        x = NULL
      ) +
      # Use the most basic theme with minimal customization
      theme_bw() +
      theme(
        legend.position = "none",
        strip.text = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.text = element_text(size = 10),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        # Make the plot more compact overall with extra bottom margin for logo
        plot.margin = margin(5, 5, 15, 5)
      )
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    return(p_with_logo)
  }
  
  # Filter data based on inputs
  filtered_state <- reactive({
    state_bed() %>% 
      filter(year == input$year)
  })
  
  filtered_cbsa <- reactive({
    cbsa_bed() %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa)
  })
  
  filtered_local <- reactive({
    local_bed() %>% 
      filter(year == input$year,
             name_long == input$locality)
  })
  
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