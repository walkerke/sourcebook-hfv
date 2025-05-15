library(shiny)
library(tidyverse)
library(scales)
library(ggiraph)  # For interactive ggplots
library(systemfonts)
library(hdatools)  # For scale_fill_hfv(), theme_hfv() and scale_color_hfv()
library(here)
library(ggplot2)
library(svglite) # For PDF export
library(grid)    # For grobs
library(png)     # For reading PNG files

# Define UI
ui <- fluidPage(
  # Set a fixed width and height for the entire app with a border in HFV Shadow color
  style = "width: 800px; height: 500px; margin: 0 auto; overflow: hidden; border: 2px solid #011E41; border-radius: 5px; font-family: 'Open Sans', sans-serif;",
  
  # Add CSS for HousingForward branding
  tags$head(
    # Import Google Fonts for Poppins and Open Sans
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@600&family=Open+Sans&display=swap"),
    
    tags$style(HTML("
      /* Set brand colors */
      :root {
        --sky: #40C0C0;
        --grass: #259591;
        --lilac: #8B85CA;
        --shadow: #011E41;
        --berry: #B1005F;
        --desert: #E0592A;
      }
      
      /* Apply brand fonts */
      h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
      }
      
      body {
        font-family: 'Open Sans', sans-serif;
      }
      
      /* Style header */
      .shiny-title {
        color: var(--shadow);
        margin-top: 5px;
        margin-bottom: 5px;
        border-bottom: 2px solid var(--sky);
        padding-bottom: 5px;
      }
      
      /* Style sidebar */
      .well {
        background-color: #f8f8f8;
        border: 1px solid var(--sky);
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 10px;
      }
      
      /* Style tabs */
      .nav-tabs > li > a {
        color: var(--shadow);
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        color: white;
        background-color: var(--sky);
        border-color: var(--sky);
      }
      
      /* Style select inputs */
      .selectize-input {
        border-color: var(--sky);
      }
      
      /* Make girafe container fit fixed size */
      .girafe-container {
        width: 100%;
        height: 380px;
      }
      
      /* Force SVG to take specified dimensions */
      .girafe {
        width: 100% !important;
        height: 100% !important;
      }
      
      /* Compact sidebar */
      .well {
        padding: 10px;
        margin-bottom: 10px;
      }
      
      /* Compact form controls */
      .form-group {
        margin-bottom: 8px;
      }
      
      /* Make tab panel fit in fixed height */
      .tab-content {
        padding: 0;
        height: 380px;
      }
      
      /* Reduce tab header space */
      .nav-tabs {
        margin-bottom: 5px;
      }
      
      /* Adjust spacing in main panel */
      .col-sm-9 {
        padding-left: 5px;
      }
      
      /* Style download buttons */
      .btn-default {
        border: none;
        font-family: 'Poppins', sans-serif;
        font-weight: 600;
        transition: background-color 0.3s ease;
      }
      
      .btn-default:hover {
        opacity: 0.85;
      }
    "))
  ),
  
  titlePanel(
    title = div(
      style = "display: flex; align-items: center;",
      img(src = "https://housingforwardva.org/wp-content/uploads/2025/05/HousingForward-VA-Logo-Files-Icon-One-Color-RGB.png", height = "30px", style = "margin-right: 10px;"),
      "Housing Type Distribution"
    ),
    windowTitle = "Housing Type Distribution"
  ),
  
  # Use standard 4/8 column layout with reduced vertical margins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Year filter for all tabs
      selectInput("year", "Select Year:",
                  choices = 2010:2023,
                  selected = 2023),
      
      # Conditional UI based on selected tab
      conditionalPanel(
        condition = "input.tabs == 'cbsa'",
        selectInput("cbsa", "Metro Area:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'local'",
        selectInput("locality", "Locality:", choices = NULL)
      ),
      
      # Add horizontal line separator
      tags$hr(style = "border-color: var(--sky); margin-top: 15px; margin-bottom: 15px;"),
      
      # Data source information
      tags$div(
        style = "font-size: 12px; color: #666; margin-bottom: 15px;",
        tags$p("Source: U.S. Census Bureau. American Community Survey 5-Year Estimates. Table B25032.")
      )
      
      # Removed download buttons
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("State", girafeOutput("state_plot", height = "380px"), value = "state"),
        tabPanel("Metro Area", girafeOutput("cbsa_plot", height = "380px"), value = "cbsa"),
        tabPanel("Locality", girafeOutput("local_plot", height = "380px"), value = "local")
      )
    )
  )
)

# Define server
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
                size = 4) +
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
        axis.text = element_text(size = 12),
        axis.text.x = element_text(hjust = 0.1),
        panel.spacing.x = unit(15, "pt"),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 20))
      )
    
    # Get logo and add it to the plot
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.2, height = 0.2)
    
    return(p_with_logo)
  }
  
  
  # Convert to interactive girafe for each plot
  create_interactive_plot <- function(plot_obj) {
    girafe(
      ggobj = plot_obj,
      width_svg = 8,      # Width in inches
      height_svg = 5,     # Height in inches
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