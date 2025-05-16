library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For number_format
library(ggtext)      # For formatted text in plots

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
pop_change <- read_rds(here("data", "rds", "pop_change.rds"))

# Create lists for filters
cbsa_list <- sort(unique(pop_change$cbsa_title))
locality_list <- sort(unique(pop_change$name_long))

# Pre-process data
cbsa_pop <- pop_change %>% 
  group_by(year, cbsa_title, component) %>% 
  summarise(value = sum(value), .groups = "drop")

state_pop <- pop_change %>% 
  group_by(year, component) %>% 
  summarise(value = sum(value), .groups = "drop")

# Create color-coded subtitle
subtitle_text <- "Net <span style='color:#011E41'><b>domestic migration</b></span>, <span style='color:#40C0C0'><b>international migration</b></span>, and <span style='color:#8B85CA'><b>natural increase (or decrease)</b></span>"

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
      h4("Components of Population Change", style = "margin: 0; color: #011E41;")
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
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa_select", "Metro Area:", 
                          choices = cbsa_list,
                          selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local'",
              selectInput("locality_select", "Locality:", 
                          choices = locality_list,
                          selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1],
                          width = "100%", 
                          selectize = FALSE)
            )
          ),
          
          # Component legend
          div(
            style = "font-size: 10px; margin-top: 15px;",
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 10px; height: 10px; background-color: #011E41; margin-right: 5px;'></span>Domestic Migration</div>"),
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 10px; height: 10px; background-color: #40C0C0; margin-right: 5px;'></span>International Migration</div>"),
            HTML("<div style='margin-bottom: 5px;'><span style='display: inline-block; width: 10px; height: 10px; background-color: #8B85CA; margin-right: 5px;'></span>Natural Increase/Decrease</div>")
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
  
  # Create filtered datasets
  filtered_cbsa <- reactive({
    req(input$cbsa_select)
    
    cbsa_pop %>%
      filter(cbsa_title == input$cbsa_select)
  })
  
  filtered_locality <- reactive({
    req(input$locality_select)
    
    pop_change %>%
      filter(name_long == input$locality_select)
  })
  
  # Plot titles
  state_title <- reactive({
    "Virginia Components of Population Change"
  })
  
  cbsa_title <- reactive({
    paste("Components of Population Change in", input$cbsa_select)
  })
  
  locality_title <- reactive({
    paste("Components of Population Change in", input$locality_select)
  })
  
  # Function to create stacked bar plots for population components
  create_pop_change_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Year: ", year, "\n",
        "Component: ", component, "\n",
        "Change: ", format(value, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = value,
                    fill = component)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = paste(year, component)),
        position = "stack"
      ) +
      scale_fill_manual(values = c(
        "domestic" = hfv_colors$shadow,
        "international" = hfv_colors$sky,
        "natural" = hfv_colors$lilac
      )) +
      scale_y_continuous(labels = number_format(big.mark = ",")) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Population Change",
        x = NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
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
  
  # Render the plots
  output$state_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(state_pop, state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_pop_change_plot(filtered_locality(), locality_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)