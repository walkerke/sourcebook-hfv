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

# UI with shinydashboard - optimized for embedding
ui <- dashboardPage(
  # Set the header to minimal for embedding
  dashboardHeader(disable = TRUE),
  
  # Make sidebar narrower for embedding
  dashboardSidebar(
    width = 200,
    # Using CSS instead of the collapsible parameter
    sidebarMenu(
      tags$div(
        class = "sidebar-logo-container",
        tags$div(
          class = "logo-wrapper",
          tags$img(src = "var_white_logo.png", class = "logo-img")
        ),
        tags$div(
          class = "logo-wrapper",
          tags$img(src = "hfv_white_logo.png", class = "logo-img")
        )
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
    
    # CSS optimized for embedding
    tags$style(HTML("
    /* Control dashboard height - optimized for embedding */
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow-y: auto;
    }
    
    .wrapper {
      min-height: 600px;
      height: auto;
      max-height: 100vh;
      position: relative; /* Positioning context for sidebar */
    }
    
    /* Make sure sidebar always extends full height */
    .main-sidebar, .left-side {
      position: absolute;
      top: 0;
      bottom: 0;
      min-height: 100% !important;
      height: auto !important;
    }
    
    /* Ensure entire background of the app is white rather than default gray */
    body, html, .content-wrapper, .right-side {
      background-color: white !important;
    }
    
    /* Main styling */
    .content-wrapper, .right-side {
      background-color: white !important;
    }
    
    /* Fix sidebar colors - lighter blue */
    .skin-blue .main-sidebar, .skin-blue .left-side {
      background-color: #102C54 !important;
      min-height: 100% !important;
      height: 100% !important;
      position: fixed !important; /* Keep sidebar fixed while content scrolls */
      overflow-y: auto; /* Allow sidebar to scroll if content is tall */
    }
    
    /* Content wrapper needs to accommodate fixed sidebar */
    .content-wrapper {
      margin-left: 200px; /* Width of sidebar */
      min-height: 100% !important;
    }
    
    /* Force sidebar to remain visible - prevent collapse */
    .sidebar-collapse .main-sidebar {
      -webkit-transform: none !important;
      -ms-transform: none !important;
      -o-transform: none !important;
      transform: none !important;
      width: 200px !important;
    }
    
    /* Hide sidebar toggle button */
    .sidebar-toggle {
      display: none !important;
    }
    
    /* Logo container styling - more compact for embedding */
    .sidebar-logo-container {
      padding: 10px;
      text-align: center;
      margin-bottom: 15px;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 10px; /* Space between logos */
    }
    
    /* Individual logo styling - more compact */
    .logo-img {
      display: block;
      max-width: 80%; /* Reduced to ensure it fits */
      height: auto;
      margin: 0 auto;
    }
    
    /* Filter labels */
    .filter-label {
      color: white;
      font-size: 14px;
      margin-bottom: 5px;
      font-weight: 500;
    }
    
    .sidebar-filters {
      padding: 0 12px 12px 12px;
    }
    
    /* Custom valueBox colors to match HFV palette */
    .small-box.bg-purple { background-color: #B1005F !important; } /* berry */
    .small-box.bg-blue { background-color: #8B85CA !important; } /* lilac */
    .small-box.bg-aqua { background-color: #259591 !important; } /* grass */
    .small-box.bg-navy { background-color: #011E41 !important; } /* shadow */
    
    /* Make sure the valueBoxes contain their contents */
    .small-box {
      position: relative;
      overflow: hidden;
      margin-bottom: 10px; /* Reduced margin for embedding */
    }
    
    /* Make value boxes more responsive for embedding */
    .small-box h3 {
      font-size: clamp(14px, 3vw, 22px);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: normal; /* Allow text to wrap */
      word-wrap: break-word; /* Break words if needed */
      line-height: 1.2; /* Tighter line height for wrapped text */
      max-height: 2.4em; /* Limit to 2 lines */
    }
    
    .small-box p {
      font-size: clamp(10px, 1.5vw, 14px);
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: normal; /* Allow text to wrap */
      word-wrap: break-word; /* Break words if needed */
      line-height: 1.2; /* Tighter line height */
      max-height: 2.4em; /* Limit to 2 lines */
    }
    
    /* Fixed icon positioning for valueBoxes with improved padding */
    .small-box .icon-large {
      font-size: clamp(30px, 5vw, 50px);
      transition: all 0.3s linear;
      position: absolute;
      top: auto; 
      right: 15px;
      bottom: 15px;
      z-index: 0;
      max-height: 60%;
      opacity: 0.7;
    }
    
    /* Additional padding for inner content */
    .small-box .inner {
      padding: 12px;
      padding-right: 60px;
      padding-bottom: 15px;
    }
    
    /* Chart box styling - more compact for embedding */
    .box {
      border-radius: 6px !important;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1) !important;
      border-top: 3px solid #3c8dbc;
      margin-bottom: 10px; /* Reduced for embedding */
    }
    
    .box.box-primary {
      border-top-color: #40C0C0 !important;
    }
    
    .box.box-success {
      border-top-color: #011E41 !important;
    }
    
    .box.box-info {
      border-top-color: #8B85CA !important;
    }
    
    /* Ensure plotly charts scale properly in embedded view */
    .plotly {
      width: 100% !important;
      height: 100% !important;
    }
    
    .js-plotly-plot {
      width: 100% !important;
      height: 100% !important;
    }
    
    /* Fix SVG scaling for embedding */
    svg {
      width: 100% !important;
    }
    
    /* Responsive styling for embedding */
    @media (max-width: 767px) {
      .info-box {
        margin-bottom: 8px !important;
      }
      
      .box {
        margin-bottom: 8px !important;
      }
      
      /* Prevent sidebar from disappearing on small screens */
      .sidebar-collapse .main-sidebar {
        -webkit-transform: translate(-200px, 0);
        -ms-transform: translate(-200px, 0);
        -o-transform: translate(-200px, 0);
        transform: translate(-200px, 0);
      }
      
      /* Override default shinydashboard behavior that hides sidebar */
      .main-sidebar {
        -webkit-transform: translate(0, 0) !important;
        -ms-transform: translate(0, 0) !important;
        -o-transform: translate(0, 0) !important;
        transform: translate(0, 0) !important;
        width: 200px !important;
        position: fixed !important;
        height: 100% !important;
      }
      
      /* Adjust main content area to make room for sidebar */
      .content-wrapper {
        margin-left: 200px !important;
      }
      
      /* Make value boxes stack vertically on mobile */
      .col-sm-3 {
        width: 100%;
      }
      
      /* Reduce sidebar width on very narrow screens */
      @media (max-width: 480px) {
        .main-sidebar {
          width: 160px !important;
        }
        
        .content-wrapper {
          margin-left: 160px !important;
        }
      }
    }
    
    /* Embedded mode specific adjustments */
    .embedded-mode .main-header {
      display: none !important;
    }
    
    .embedded-mode .content-wrapper {
      margin-left: 200px; /* Width of sidebar */
    }
    
    .embedded-mode.sidebar-collapse .content-wrapper {
      margin-left: 50px; /* Width of collapsed sidebar */
    }
    
    /* Add proper scrolling behavior for narrow screens */
    @media (max-width: 767px) {
      /* Make content area scrollable when needed */
      .content-wrapper {
        overflow-x: auto !important;
        min-width: 320px; /* Ensure minimum content width */
      }
      
      /* Allow horizontal scrolling for the page on very narrow screens */
      html, body {
        overflow-x: auto !important;
        min-width: 480px; /* Minimum width ensures critical elements are visible */
      }
      
      /* Ensure charts don't get squeezed too small */
      .box .box-body {
        min-width: 280px;
      }
      
      /* Make sure sidebar background extends full height on mobile too */
      .main-sidebar {
        bottom: 0;
        min-height: 100vh !important;
      }
    }
    
    /* Sidebar toggle button styling for embedded mode */
    .sidebar-toggle {
      color: white;
      background-color: #102C54;
      position: absolute;
      top: 0;
      left: 200px;
      z-index: 1000;
      padding: 5px 10px;
      border-radius: 0 0 4px 0;
    }
    
    /* Add class to body for embedded mode */
    body.embedded-mode {
      background-color: #f8f9fa;
    }
    ")),
    
    # Add a custom sidebar toggle for embedded mode
    tags$script(HTML("
      $(document).ready(function() {
        $('body').addClass('embedded-mode');
        
        // Remove any stray text from DOM
        $('.main-header').find('span:contains(\"FALSE\")').remove();
        $('.main-sidebar').find('span:contains(\"FALSE\")').remove();
        
        // Extra cleanup of any text nodes that might appear
        $('.main-header, .main-sidebar').contents().each(function() {
          if(this.nodeType === 3 && $.trim(this.nodeValue) !== '') {
            $(this).remove();
          }
        });
        
        // Fix for sidebar height
        function fixSidebarHeight() {
          var contentHeight = $('.content-wrapper').height();
          var windowHeight = $(window).height();
          var sidebarHeight = Math.max(contentHeight, windowHeight);
          $('.main-sidebar').css('min-height', sidebarHeight + 'px');
        }
        
        // Run on page load and whenever the window resizes
        fixSidebarHeight();
        $(window).resize(fixSidebarHeight);
        
        // Also run when content might have changed
        var observer = new MutationObserver(fixSidebarHeight);
        observer.observe($('.content-wrapper')[0], {
          childList: true,
          subtree: true
        });
      });
    ")),
    
    # Value boxes - optimized for embedding with reduced height
    fluidRow(
      # Latest Quarter box
      valueBox(
        value = textOutput("latest_quarter", inline = TRUE),
        subtitle = "Latest Quarter",
        icon = icon("calendar"),
        color = "purple",
        width = 3
      ),
      
      # Units Sold box
      valueBox(
        value = textOutput("units_sold", inline = TRUE),
        subtitle = "Units Sold", 
        icon = icon("home"),
        color = "blue",
        width = 3
      ),
      
      # Median Price box
      valueBox(
        value = textOutput("median_price", inline = TRUE),
        subtitle = "Median Price",
        icon = icon("dollar-sign"),
        color = "aqua",
        width = 3
      ),
      
      # Median DOM box
      valueBox(
        value = textOutput("median_dom", inline = TRUE),
        subtitle = "Median Days on Market",
        icon = icon("clock"),
        color = "navy",
        width = 3
      )
    ),
    
    # Chart rows - reduced heights for embedding
    fluidRow(
      # Top price chart
      box(
        title = "Median Sales Price by Quarter",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput("price", height = "200px")  # Reduced height
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
        plotlyOutput("sales", height = "200px")  # Reduced height
      ),
      
      # Days on Market chart
      box(
        title = "Median Days on Market by Quarter",
        status = "success", 
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 6,
        plotlyOutput("days", height = "200px")  # Reduced height
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
  var_data <- read_rds("home-sales.rds")
  
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
      label = NULL,
      choices = filtered_names,
      selected = if(length(filtered_names) > 0) filtered_names[1] else NULL
    )
  })
  
  # Dynamic label for the geo name
  output$geo_name_label <- renderText({
    paste(input$geo_type, "Name")
  })
  
  # Create reactive filtered data with better error handling
  dashboard_data <- reactive({
    req(input$geo_type, input$geo_name)
    
    # Filter data
    filtered <- var_data %>% 
      filter(geography == input$geo_type,
             name == input$geo_name)
    
    # Validate that we have data after filtering
    validate(
      need(nrow(filtered) > 0, 
           "Loading...")
    )
    
    return(filtered)
  })
  
  # Create reactive values for the info boxes with better error handling
  latest_data <- reactive({
    req(dashboard_data())
    
    # Get latest quarter data with validation
    latest <- dashboard_data() %>% 
      filter(quarter == latest_quarter)
    
    validate(
      need(nrow(latest) > 0, "No data for the latest quarter")
    )
    
    return(latest)
  })
  
  # Simple text outputs for info boxes with error handling
  output$latest_quarter <- renderText({
    as.character(latest_quarter)
  })
  
  output$units_sold <- renderText({
    tryCatch({
      req(latest_data())
      format(latest_data()$units[1], big.mark = ",", trim = TRUE)
    },
    error = function(e) {
      return("--")
    })
  })
  
  output$median_price <- renderText({
    tryCatch({
      req(latest_data())
      dollar_format()(latest_data()$med_price[1])
    },
    error = function(e) {
      return("--")
    })
  })
  
  output$median_dom <- renderText({
    tryCatch({
      req(latest_data())
      latest_data()$med_dom[1]
    },
    error = function(e) {
      return("--")
    })
  })
  
  # Plotly outputs - sales chart with HFV colors
  output$sales <- renderPlotly({
    req(dashboard_data())
    
    # Create the ggplot object - simplified for embedding
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
      # Show fewer x-axis labels for embedded view
      scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 8)]}) +
      scale_y_continuous(labels = comma_format()) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2)
      )
    
    # Convert to plotly with more compact layout for embedding
    ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>% 
      layout(
        autosize = TRUE,
        margin = list(l = 40, r = 10, b = 30, t = 10, pad = 2),
        font = list(size = 9)
      )
  })
  
  # Plotly outputs - price chart with HFV colors
  output$price <- renderPlotly({
    req(dashboard_data())
    
    # Create the ggplot object - simplified for embedding
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
      # Show fewer x-axis labels for embedded view
      scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 8)]}) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2)
      )
    
    # Convert to plotly with more compact layout for embedding
    ggplotly(gg, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 40, r = 10, b = 30, t = 10, pad = 2),
        font = list(size = 9)
      )
  })
  
  # Plotly outputs - days chart with HFV colors and error handling  
  output$days <- renderPlotly({
    # Validate that we have data
    tryCatch({
      req(dashboard_data())
      validate(need(nrow(dashboard_data()) > 0, "No data available"))
      
      # Create the ggplot object - simplified for embedding
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
        # Show fewer x-axis labels for embedded view
        scale_x_discrete(breaks = function(x) {x[seq(1, length(x), by = 8)]}) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          panel.grid.major.x = element_blank(),
          legend.position = "none",
          plot.margin = margin(2, 2, 2, 2)
        )
      
      # Convert to plotly with more compact layout for embedding
      ggplotly(gg, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        layout(
          autosize = TRUE,
          margin = list(l = 40, r = 10, b = 30, t = 10, pad = 2),
          font = list(size = 9)
        )
    },
    error = function(e) {
      # Return an empty plot with a loading message
      plot_ly() %>%
        add_annotations(
          text = "Loading data...",
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

# Create a function to run the app with embedding parameters
run_embedded_app <- function(width = "100%", height = "600px") {
  shinyApp(
    ui = ui,
    server = server,
    options = list(
      width = width,
      height = height,
      launch.browser = FALSE
    )
  )
}

# Run the app (uncomment to run directly)
# run_embedded_app()

# Standard shinyApp to run without embedding options
shinyApp(ui, server)