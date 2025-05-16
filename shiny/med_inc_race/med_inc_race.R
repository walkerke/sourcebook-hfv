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
locality_inc_data <- read_rds(here("data", "rds", "b19013_locality.rds"))
cbsa_inc_data <- read_rds(here("data", "rds", "b19013_cbsa.rds"))
state_inc_data <- read_rds(here("data", "rds", "b19013_state.rds"))

# Create lists for filters
state_list <- sort(unique(state_inc_data$state))
cbsa_list <- sort(unique(cbsa_inc_data$CBSA))
locality_list <- sort(unique(locality_inc_data$locality))
state_year_list <- sort(unique(state_inc_data$year), decreasing = TRUE)
cbsa_year_list <- sort(unique(cbsa_inc_data$year), decreasing = TRUE)
locality_year_list <- sort(unique(locality_inc_data$year), decreasing = TRUE)

# Define all possible race categories
all_races <- unique(c(
  state_inc_data$race,
  cbsa_inc_data$race,
  locality_inc_data$race
))

# Create color vector for races
race_colors <- c(
  "White alone, not Hispanic" = hfv_colors$sky,
  "Black alone" = hfv_colors$shadow,
  "Asian alone" = hfv_colors$grass,
  "Hispanic (any race)" = hfv_colors$desert,
  "Two or more races" = hfv_colors$berry,
  "American Indian alone" = hfv_colors$lilac,
  "Pacific Islander alone" = "#FFC658",  # Additional color
  "Some other race alone" = "#FF7276"    # Additional color
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
      h4("Median Household Income by Race/Ethnicity", style = "margin: 0; color: #011E41;")
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
                        choices = state_year_list, 
                        selected = max(state_year_list), 
                        width = "100%",
                        selectize = FALSE)
          ),
          
          # Show inflation-adjusted option
          div(
            style = "margin-bottom: 0;",
            checkboxInput("adjusted", "Show Inflation-Adjusted", FALSE)
          ),
          
          # Geography selectors with minimal height
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
              selectInput("locality_select", "Locality:", 
                          choices = locality_list,
                          selected = "Richmond city",
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
              "Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B19013.",
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
  
  # Get filtered data based on selected tab and inputs
  filtered_state <- reactive({
    req(input$state_select, input$year)
    
    state_inc_data %>%
      filter(
        state == input$state_select,
        year == input$year
      )
  })
  
  filtered_cbsa <- reactive({
    req(input$cbsa_select, input$year)
    
    cbsa_inc_data %>%
      filter(
        CBSA == input$cbsa_select,
        year == input$year
      )
  })
  
  filtered_locality <- reactive({
    req(input$locality_select, input$year)
    
    locality_inc_data %>%
      filter(
        locality == input$locality_select,
        year == input$year
      )
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$state_select, "(", input$year, ")")
  })
  
  cbsa_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$cbsa_select, "(", input$year, ")")
  })
  
  locality_title <- reactive({
    paste("Median Household Income by Race/Ethnicity in", input$locality_select, "(", input$year, ")")
  })
  
  # Y-axis label based on inflation adjustment
  y_label <- reactive({
    if(input$adjusted) {
      "Median Household Income (Inflation-Adjusted)"
    } else {
      "Median Household Income"
    }
  })
  
  # Function to create interactive bar plots
  create_bar_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Select which value to plot based on checkbox
    value_col <- if(input$adjusted) "adjusted" else "estimate"
    
    # Filter out NA values
    plot_data <- data %>% 
      drop_na() %>%
      # Use the value column to order the races
      mutate(race = factor(race, levels = race[order(get(value_col))]))
    
    # Add tooltips
    plot_data <- plot_data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Income: ", scales::dollar(get(value_col))
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = race,
                  y = .data[[value_col]],
                  fill = race)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = race)
      ) +
      # Add the value labels at the end of each bar
      geom_text(aes(label = scales::dollar(.data[[value_col]], accuracy = 1)),
                hjust = -0.2,
                color = "#333333",
                size = 3) +
      # Set the fill colors
      scale_fill_manual(values = race_colors) +
      # Extend the plot area to make room for labels
      coord_flip(clip = "off") +
      # Format y-axis with dollar signs
      scale_y_continuous(
        labels = scales::dollar_format(),
        limits = function(x) c(0, max(x) * 1.2)  # Add 20% headroom for labels
      ) +
      labs(
        title = title_text,
        caption = " ",  # Empty caption for logo space
        x = NULL,
        y = y_label()
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20)),
        plot.margin = margin(5, 20, 15, 5)  # Extra right margin for labels, bottom for logo
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
  
  # Render plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_bar_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_bar_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_bar_plot(filtered_locality(), locality_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)