library(tidycensus)
library(tidyverse)
library(shiny)
library(bslib)
library(ggiraph)
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(shinyjs)     # For dynamic UI updates
library(cowplot)     # For adding logo to plots

# Load data
bps <- read_rds(here::here("data", "rds", "bps.rds"))

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

# Prepare aggregated datasets with recategorized building types
# First, create a function to recode building types
recode_type <- function(df) {
  df %>%
    # Assuming the original column is called 'type' or similar
    # Combine 2-unit and 3-4 unit into "2-4 unit"
    mutate(type = case_when(
      type == "1-unit" ~ "1-unit",
      type == "2-units" | type == "3-4 units" ~ "2-4 units",
      type == "5+ units" ~ "5+ units",
      TRUE ~ as.character(type)
    ))
}

# Apply the recoding and create aggregated datasets
state <- bps %>% 
  recode_type() %>%
  group_by(year, type) %>% 
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value)
  )

cbsa <- bps %>% 
  recode_type() %>%
  group_by(year, cbsa_title, type) %>% 
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value)
  )

locality <- bps %>% 
  recode_type() %>%
  group_by(year, name_long, type) %>% 
  summarise(
    bldgs = sum(bldgs),
    units = sum(units),
    value = sum(value)
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
      h4("Building Permit Trends", style = "margin: 0; color: #011E41;")
    ),
    
    # Main content area with reduced margins
    div(
      style = "height: 435px; overflow: hidden;",
      
      # Use layout_columns for a compact layout
      layout_columns(
        col_widths = c(3, 9),
        gap = "10px",
        
        # Sidebar Panel
        card(
          height = "435px",
          padding = "8px",  # Reduced padding for compactness
          margin = 0,
          full_screen = FALSE,
          style = "background-color: #E8EDF2;",  # Light shade derived from shadow color
          
          # Metric select
          div(
            style = "margin-bottom: 8px;",
            selectInput("metric", "Select Metric:", 
                        choices = list(
                          "Units" = "units",
                          "Buildings" = "bldgs", 
                          "Value ($)" = "value"
                        ), 
                        selected = "units", 
                        width = "100%",
                        selectize = FALSE)
          ),
          
          # Building type filter
          div(
            style = "margin-bottom: 8px;",
            checkboxGroupInput("types", "Building Types:",
                               choices = list(
                                 "1-unit" = "1-unit",
                                 "2-4 units" = "2-4 units",
                                 "5+ units" = "5+ units"
                               ),
                               selected = c("1-unit", "2-4 units", "5+ units"),
                               width = "100%")
          ),
          
          # Geography selectors
          div(
            style = "margin-bottom: 8px;",
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
          hr(style = "margin: 10px 0;"),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 8px;",
            p(
              "Source: U.S. Census Bureau Building Permits Survey.",
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

# Define server logic
server <- function(input, output, session) {
  
  # Update metro area choices
  observe({
    cbsa_choices <- sort(unique(cbsa$cbsa_title))
    updateSelectInput(session, "cbsa", choices = cbsa_choices, selected = "Richmond, VA")
  })
  
  # Update locality choices
  observe({
    locality_choices <- sort(unique(locality$name_long))
    updateSelectInput(session, "locality", choices = locality_choices, selected = "Richmond City")
  })
  
  # Filter data based on selected building types
  filter_data <- function(data) {
    # Filter by selected building types
    data %>% filter(type %in% input$types)
  }
  
  # Generate title based on current tab and selections
  get_title <- reactive({
    if(input$tabs == "state") {
      "Virginia Building Permits"
    } else if(input$tabs == "cbsa") {
      paste(input$cbsa, "Building Permits")
    } else {
      paste(input$locality, "Building Permits")
    }
  })
  
  # Get subtitle based on metric selection
  get_subtitle <- reactive({
    metric_label <- case_when(
      input$metric == "units" ~ "Housing Units",
      input$metric == "bldgs" ~ "Buildings",
      input$metric == "value" ~ "Value ($ million)",
      TRUE ~ input$metric
    )
    
    metric_label
  })
  
  # Create plots with interactive tooltips on stacked bars
  create_plot <- function(data, metric_col, title, subtitle) {
    # Map colors to building types
    color_mapping <- c(
      "1-unit" = hfv_colors$sky,
      "2-4 units" = hfv_colors$shadow,
      "5+ units" = hfv_colors$lilac
    )
    
    # Format tooltip text
    tooltip_format <- function(year, type, value) {
      if(metric_col == "value") {
        paste0(
          type, " (", year, "): ", 
          scales::dollar(value, accuracy = 0.1, prefix = "$", suffix = "M")
        )
      } else {
        paste0(
          type, " (", year, "): ", 
          scales::comma(value, accuracy = 1)
        )
      }
    }
    
    # For value, convert to millions
    if(metric_col == "value") {
      data <- data %>%
        mutate(value = value / 1000000)
    }
    
    # Create the base plot
    p <- ggplot(data,
                aes(
                  x = year,
                  y = !!sym(metric_col),
                  fill = type,
                  tooltip = tooltip_format(year, type, !!sym(metric_col)),
                  data_id = paste(type, year)
                )) +
      geom_col_interactive(position = "stack") +
      scale_fill_manual(values = color_mapping) +
      scale_x_continuous(breaks = unique(data$year)) +
      # Format y-axis based on the metric
      {if(metric_col == "value") 
        scale_y_continuous(labels = scales::dollar_format(scale = 1, prefix = "$", suffix = "M"))
        else 
          scale_y_continuous(labels = scales::number_format(big.mark = ","))
      } +
      labs(
        title = title,
        subtitle = subtitle,
        caption = " ",  # Empty caption to leave space for logo
        fill = "Building Type"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12),
        axis.title = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 30, l = 5)  # Increased bottom margin for logo
      )
    
    # Add logo to the plot
    # Find the logo file path
    logo_path <- file.path(getwd(), "www", "hfv_logo.png")
    
    # Add logo to the plot
    logo_plot <- ggdraw(p) +
      draw_image(
        logo_path,
        x = 0.85,    # Horizontal position (right side)
        y = 0.05,    # Vertical position (bottom)
        width = 0.15, 
        height = 0.15
      )
    
    # Convert to interactive girafe plot
    girafe(
      ggobj = logo_plot,
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
  
  # Create the state plot
  output$state_plot <- renderGirafe({
    req(input$metric, input$types)
    
    # Filter and prepare the data
    plot_data <- filter_data(state)
    metric_col <- input$metric
    
    # Create the plot
    create_plot(
      plot_data, 
      metric_col, 
      get_title(), 
      get_subtitle()
    )
  })
  
  # Create the CBSA plot
  output$cbsa_plot <- renderGirafe({
    req(input$metric, input$cbsa, input$types)
    
    # Filter and prepare data
    plot_data <- filter_data(cbsa) %>%
      filter(cbsa_title == input$cbsa)
    
    metric_col <- input$metric
    
    # Create the plot
    create_plot(
      plot_data, 
      metric_col, 
      get_title(), 
      get_subtitle()
    )
  })
  
  # Create the locality plot
  output$local_plot <- renderGirafe({
    req(input$metric, input$locality, input$types)
    
    # Filter and prepare data
    plot_data <- filter_data(locality) %>%
      filter(name_long == input$locality)
    
    metric_col <- input$metric
    
    # Create the plot
    create_plot(
      plot_data, 
      metric_col, 
      get_title(), 
      get_subtitle()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)