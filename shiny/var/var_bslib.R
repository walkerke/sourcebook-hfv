library(shiny)
library(shinydashboard)
library(bslib)        # For theme customization
library(tidyverse)
library(here)
library(scales)
library(shinyjs)      # For UI interactions
library(plotly)       # For interactive plots

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

# Create a bslib theme
hfv_theme <- bs_theme(
  version = 4,                       # Use Bootstrap 4 for better compatibility with shinydashboard
  bg = "#ffffff",                    # Background color
  fg = "#333333",                    # Text color
  primary = hfv_colors$sky,          # Primary color
  secondary = hfv_colors$shadow,     # Secondary color
  success = hfv_colors$grass,        # Success color
  info = hfv_colors$lilac,           # Info color
  warning = hfv_colors$desert,       # Warning color
  danger = hfv_colors$berry,         # Danger color
  base_font = font_google("Open Sans"),
  heading_font = font_google("Poppins")
)

# UI with shinydashboard
ui <- dashboardPage(
  dashboardHeader(
    title = "Virginia Housing",
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      # Logo box - removed white background
      tags$div(
        class = "sidebar-logo-container",
        tags$img(src = "var_white_logo.png", width = "80%", class = "logo-img mb-2"),
        tags$img(src = "hfv_white_logo.png", width = "80%", class = "logo-img")
      ),
      
      # Filter inputs
      tags$div(
        class = "sidebar-filters",
        tags$h4("Select Geography", class = "filter-label"),
        selectInput(
          inputId = "geo_type",
          label = NULL,
          choices = c("State", "MSA", "Locality"),
          selected = "State"
        ),
        
        tags$h4(textOutput("geo_name_label"), class = "filter-label"),
        selectInput(
          inputId = "geo_name",
          label = NULL,
          choices = NULL
        )
      )
    )
  ),
  
  # Main body
  dashboardBody(
    # Apply bslib theme to the dashboard body
    theme = hfv_theme,
    
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Control dashboard height */
        html, body {
          height: 100%;
          overflow-y: auto;
        }
        
        .wrapper {
          min-height: 800px;
          height: 100vh;
        }
        
        /* Main styling */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        /* Fix sidebar colors - lighter blue */
        .skin-blue .main-sidebar, .skin-blue .left-side {
          background-color: #102C54 !important;
        }
        
        .skin-blue .sidebar-menu>li.active>a, 
        .skin-blue .sidebar-menu>li:hover>a {
          background-color: #011E41 !important;
        }
        
        /* Logo container styling */
        .sidebar-logo-container {
          padding: 15px;
          text-align: center;
          margin-bottom: 20px;
        }
        
        .logo-img {
          display: block;
          margin: 0 auto;
          max-width: 100%;
        }
        
        /* Filter labels */
        .filter-label {
          color: white;
          font-size: 16px;
          margin-bottom: 5px;
          font-weight: 500;
        }
        
        .sidebar-filters {
          padding: 0 15px 15px 15px;
        }
        
        /* Custom info box styling */
        .custom-info-box {
          display: flex;
          min-height: 100px;
          color: white;
          border-radius: 6px;
          margin-bottom: 15px;
          overflow: hidden;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .custom-info-box .info-box-icon {
          display: flex;
          align-items: center;
          justify-content: center;
          width: 90px;
          font-size: 40px;
          background-color: rgba(0,0,0,0.2);
        }
        
        .custom-info-box .info-box-content {
          padding: 15px;
          flex: 1;
        }
        
        .custom-info-box .info-box-text {
          font-size: 14px;
          font-weight: 500;
          text-transform: uppercase;
          margin-bottom: 5px;
        }
        
        .custom-info-box .info-box-number {
          font-size: 24px;
          font-weight: 700;
        }
        
        /* Responsive styles for info boxes */
        @media (max-width: 992px) {
          .info-box-container {
            width: 50% !important;
          }
          
          .custom-info-box .info-box-number {
            font-size: 20px;
          }
        }
        
        @media (max-width: 576px) {
          .info-box-container {
            width: 100% !important;
          }
        }
        
        /* Responsive text in value boxes */
        .vb-subtitle {
          margin: 0;
          font-size: min(1vw, 14px);
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        
        .vb-value {
          margin: 0;
          font-size: min(1.8vw, 24px);
          font-weight: 600;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        
        /* Ensure all text is readable */
        @media (max-width: 1200px) {
          .vb-value {
            font-size: min(2.2vw, 20px);
          }
          .vb-subtitle {
            font-size: min(1.5vw, 12px);
          }
        }
        
        @media (max-width: 992px) {
          .vb-value {
            font-size: min(2.5vw, 18px);
          }
        }
        
        @media (max-width: 768px) {
          .vb-value {
            font-size: 16px;
          }
          .vb-subtitle {
            font-size: 11px;
          }
        }
        
        @media (max-width: 576px) {
          .vb-value {
            font-size: 14px;
          }
          .vb-subtitle {
            font-size: 10px;
          }
        }
        
        /* Chart box styling */
        .box {
          border-radius: 6px !important;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1) !important;
          border-top: 3px solid #3c8dbc;
        }
        
        .box.box-primary {
          border-top-color: #40C0C0 !important;
        }
        
        .box.box-success {
          border-top-color: #259591 !important;
        }
        
        .box.box-info {
          border-top-color: #8B85CA !important;
        }
        
        /* Ensure plotly charts scale properly */
        .plotly {
          width: 100% !important;
          height: 100% !important;
        }
        
        .js-plotly-plot {
          width: 100% !important;
          height: 100% !important;
        }
        
        /* Fix SVG scaling */
        svg {
          width: 100% !important;
          height: 100% !important;
        }
        
        /* Responsive styling */
        @media (max-width: 767px) {
          .info-box {
            margin-bottom: 10px !important;
          }
          
          .box {
            margin-bottom: 10px !important;
          }
          
          .main-header .logo {
            width: 100% !important;
          }
        }
      "))
    ),
    
    # Main content
    tags$div(
      class = "info-box-row",
      style = "display: flex; flex-wrap: wrap; margin: 0 -7.5px;",
      
      # Latest Quarter box
      tags$div(
        class = "info-box-container",
        style = "width: 25%; padding: 0 7.5px; box-sizing: border-box;",
        tags$div(
          class = "custom-info-box",
          style = paste0("background-color: ", hfv_colors$berry, ";"),
          tags$div(class = "info-box-icon", icon("calendar")),
          tags$div(
            class = "info-box-content",
            tags$div(class = "info-box-text", "Latest Quarter"),
            tags$div(class = "info-box-number", textOutput("latest_quarter", inline = TRUE))
          )
        )
      ),
      
      # Units Sold box
      tags$div(
        class = "info-box-container",
        style = "width: 25%; padding: 0 7.5px; box-sizing: border-box;",
        tags$div(
          class = "custom-info-box",
          style = paste0("background-color: ", hfv_colors$lilac, ";"),
          tags$div(class = "info-box-icon", icon("home")),
          tags$div(
            class = "info-box-content",
            tags$div(class = "info-box-text", "Units Sold"),
            tags$div(class = "info-box-number", textOutput("units_sold", inline = TRUE))
          )
        )
      ),
      
      # Median Price box
      tags$div(
        class = "info-box-container",
        style = "width: 25%; padding: 0 7.5px; box-sizing: border-box;",
        tags$div(
          class = "custom-info-box",
          style = paste0("background-color: ", hfv_colors$grass, ";"),
          tags$div(class = "info-box-icon", icon("dollar-sign")),
          tags$div(
            class = "info-box-content",
            tags$div(class = "info-box-text", "Median Price"),
            tags$div(class = "info-box-number", textOutput("median_price", inline = TRUE))
          )
        )
      ),
      
      # Median DOM box
      tags$div(
        class = "info-box-container",
        style = "width: 25%; padding: 0 7.5px; box-sizing: border-box;",
        tags$div(
          class = "custom-info-box",
          style = paste0("background-color: ", hfv_colors$shadow, ";"),
          tags$div(class = "info-box-icon", icon("clock")),
          tags$div(
            class = "info-box-content",
            tags$div(class = "info-box-text", "Median DOM"),
            tags$div(class = "info-box-number", textOutput("median_dom", inline = TRUE))
          )
        )
      )
    ),
    
    # Chart rows
    fluidRow(
      # Top price chart
      box(
        title = "Median Sales Price by Quarter",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput("price", height = "280px")
      )
    ),
    
    fluidRow(
      # Sales chart
      box(
        title = "Homes Sold by Quarter",
        status = "info",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 6,
        plotlyOutput("sales", height = "280px")
      ),
      
      # Days on Market chart
      box(
        title = "Median Days on Market by Quarter",
        status = "success", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 6,
        plotlyOutput("days", height = "280px")
      )
    )
  ),
  
  # Dashboard skin and overall size control
  skin = "blue"
)

server <- function(input, output, session) {
  # Copy the logo to the app directory for direct access
  observe({
    # Define absolute path to the logo for server-side operations
    source_logos <- c(
      file.path(getwd(), "..", "www", "var_white_logo.png"),
      file.path(getwd(), "..", "www", "hfv_white_logo.png")
    )
    
    dest_dir <- file.path(getwd(), "www")
    
    # Create www directory in the app folder if it doesn't exist
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }
    
    # Copy the logo files
    for (logo in source_logos) {
      dest_file <- file.path(dest_dir, basename(logo))
      if (!file.exists(dest_file) && file.exists(logo)) {
        file.copy(logo, dest_file)
      }
    }
  })
  
  # Load data
  var_data <- read_rds(here("data", "rds", "home-sales.rds"))
  
  # Define latest quarter
  latest_quarter <- max(var_data$quarter)
  
  # Create a data frame for the name list - Now sorted alphabetically
  name_list <- var_data %>%
    select(geography, name) %>%
    distinct() %>%
    arrange(geography, name)
  
  # Observer that updates the name dropdown based on geography selection
  observe({
    # Get geography type from input
    selected_geo <- input$geo_type
    
    # Filter name_list to only show names matching the selected geography type
    filtered_names <- name_list %>%
      filter(geography == selected_geo) %>%
      arrange(name) %>%
      pull(name)
    
    # Update the name dropdown
    updateSelectInput(
      session = session,
      inputId = "geo_name",
      label = paste(selected_geo, "Name"),
      choices = filtered_names,
      selected = if(length(filtered_names) > 0) {filtered_names[1]} else {NULL}
    )
  })
  
  # Create reactive filtered data
  dashboard_data <- reactive({
    req(input$geo_type, input$geo_name)
    var_data %>% 
      filter(geography == input$geo_type,
             name == input$geo_name)
  })
  
  # Create reactive values for the info boxes
  latest_data <- reactive({
    req(dashboard_data())
    dashboard_data() %>% 
      filter(quarter == latest_quarter)
  })
  
  # Simple text outputs for info boxes
  output$latest_quarter <- renderText({
    as.character(latest_quarter)
  })
  
  output$units_sold <- renderText({
    req(latest_data())
    if(nrow(latest_data()) > 0) {
      format(latest_data()$units[1], big.mark = ",", trim = TRUE)
    } else {"N/A"}
  })
  
  output$median_price <- renderText({
    req(latest_data())
    if(nrow(latest_data()) > 0) {
      dollar_format()(latest_data()$med_price[1])
    } else {"N/A"}
  })
  
  output$median_dom <- renderText({
    req(latest_data())
    if(nrow(latest_data()) > 0) {latest_data()$med_dom[1]} else {"N/A"}
  })
  
  # Plotly outputs - sales chart with HFV colors
  output$sales <- renderPlotly({
    req(dashboard_data())
    
    # Create the ggplot object
    gg <- ggplot(dashboard_data(), aes(
      x = quarter,
      y = units,
      fill = units,
      text = paste0(
        "Quarter: ", quarter, "<br>",
        "Units Sold: ", scales::comma(units)
      )
    )) +
      geom_col(alpha = 0.9) +
      scale_fill_gradient(low = "#d0cee9", high = hfv_colors$lilac) +
      scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 4)]}) +
      scale_y_continuous(labels = comma_format()) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Convert to plotly and add custom tooltip
    ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>% 
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 20, b = 40, t = 10, pad = 2),
        font = list(size = 10)
      )
  })
  
  # Plotly outputs - price chart with HFV colors
  output$price <- renderPlotly({
    req(dashboard_data())
    
    # Create the ggplot object
    gg <- ggplot(dashboard_data(), aes(
      x = quarter,
      y = med_price,
      fill = med_price,
      text = paste0(
        "Quarter: ", quarter, "<br>",
        "Median Sales Price: $", scales::comma(med_price)
      )
    )) +
      geom_col(alpha = 0.8) +
      scale_fill_gradient(low = "#a7d4d3", high = hfv_colors$grass) +
      scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 4)]}) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Convert to plotly and add custom tooltip
    ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 20, b = 40, t = 10, pad = 2),
        font = list(size = 10)
      )
  })
  
  # Plotly outputs - days chart with HFV colors  
  output$days <- renderPlotly({
    req(dashboard_data())
    
    # Create the ggplot object
    gg <- ggplot(dashboard_data(), aes(
      x = quarter,
      y = med_dom,
      fill = med_dom,
      text = paste0(
        "Quarter: ", quarter, "<br>",
        "Median Days on Market: ", med_dom
      )
    )) +
      geom_col(alpha = 0.8) +
      scale_fill_gradient(low = "#99a5b3", high = hfv_colors$shadow) +
      scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 4)]}) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Convert to plotly and add custom tooltip
    ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 20, b = 40, t = 10, pad = 2),
        font = list(size = 10)
      )
  })
}

shinyApp(ui, server)