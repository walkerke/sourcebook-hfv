library(shiny)
library(tidyverse)
library(ggiraph)     # For interactive ggplots
library(systemfonts) # For font_google
library(here)        # For here() function in file paths
library(grid)        # For grobs
library(png)         # For reading PNG files
library(bslib)       # For modern UI components
library(cowplot)     # For adding logo to plots
library(scales)      # For percent_format

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
race_data <- read_rds(here("data", "rds", "race-ethnicity.rds"))

# Create lists for filters
cbsa_list <- sort(unique(race_data$cbsa_title))
locality_list <- sort(unique(race_data$name_long))
year_list <- sort(as.character(unique(race_data$year)))

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
      h4("Population by Race and Ethnicity", style = "margin: 0; color: #011E41;")
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
                        choices = year_list, 
                        selected = max(year_list), 
                        width = "100%",
                        selectize = FALSE)
          ),
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa'",
              selectInput("cbsa", "Metro Area:", 
                          choices = cbsa_list,
                          selected = if("Richmond, VA" %in% cbsa_list) "Richmond, VA" else cbsa_list[1],
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local'",
              selectInput("locality", "Locality:", 
                          choices = locality_list,
                          selected = if("Richmond City" %in% locality_list) "Richmond City" else locality_list[1],
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
  filtered_state <- reactive({
    req(input$year)
    
    race_data %>% 
      group_by(year, label) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      filter(year == input$year) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>% 
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  filtered_cbsa <- reactive({
    req(input$year, input$cbsa)
    
    race_data %>% 
      group_by(year, cbsa_title, label) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      filter(year == input$year,
             cbsa_title == input$cbsa) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>% 
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  filtered_locality <- reactive({
    req(input$year, input$locality)
    
    race_data %>% 
      filter(year == input$year,
             name_long == input$locality) %>% 
      group_by(year) %>% 
      mutate(percent = value/sum(value),
             value_label = number_format(big.mark = ",")(value),
             percent_label = percent_format(accuracy = 0.1)(percent)) %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", label, "\n",
        "Count: ", value_label, "\n",
        "Percentage: ", percent_label
      ))
  })
  
  # Plot titles
  state_title <- reactive({
    paste("Virginia Population by Race and Ethnicity in", input$year)
  })
  
  cbsa_title <- reactive({
    paste("Population by Race and Ethnicity in", input$cbsa, "(", input$year, ")")
  })
  
  locality_title <- reactive({
    paste("Population by Race and Ethnicity in", input$locality, "(", input$year, ")")
  })
  
  # Function to create bar charts for race/ethnicity distribution
  create_race_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Create base plot
    p <- ggplot(data,
                aes(x = reorder(label, -percent),
                    y = percent,
                    fill = label)) +
      geom_col_interactive(
        aes(tooltip = tooltip, data_id = label),
        position = "dodge"
      ) +
      scale_fill_manual(values = c(
        "White alone, not Hispanic" = hfv_colors$sky,
        "Black alone" = hfv_colors$shadow,
        "Asian alone" = hfv_colors$grass,
        "Hispanic (any race)" = hfv_colors$desert,
        "Two or more races" = hfv_colors$berry,
        "American Indian alone" = hfv_colors$lilac,
        "Pacific Islander alone" = "#FFC658",  # Additional color
        "Some other race alone" = "#FF7276"    # Additional color
      )) +
      scale_y_continuous(
        labels = percent_format(), 
        limits = c(0, 1)
      ) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Percentage of Population",
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
    create_interactive_plot(create_race_plot(filtered_state(), state_title()))
  })
  
  output$cbsa_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_cbsa(), cbsa_title()))
  })
  
  output$local_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_locality(), locality_title()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)