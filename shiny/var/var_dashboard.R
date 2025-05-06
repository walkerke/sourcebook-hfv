library(shiny)
library(tidyverse)
library(here)
library(scales)
library(ggiraph)     # For interactive plots
library(shinyjs)     # For UI interactions

# Using dashboardPage instead of fluidPage for better sidebar functionality
ui <- navbarPage(
  title = NULL,
  id = "navbar",
  theme = "css/custom.css",  # Optional custom CSS file
  
  # Main page with sidebar layout
  tabPanel(
    "Home",
    # Use sidebarLayout for proper sidebar functionality
    sidebarLayout(
      # SIDEBAR PANEL
      sidebarPanel(
        width = 3,
        # Add logos to the top of the sidebar
        div(class = "logo-container",
            img(src = "var_logo_new.png", height = "60px", class = "main-logo"),
            img(src = "hfv_rgb_logo.png", height = "60px", class = "secondary-logo")
        ),
        # Title now in sidebar
        h2("Homes Sales in Virginia", class = "sidebar-title"),
        
        # Filter inputs
        selectInput(
          inputId = "geo_type",
          label = "Select Geography",
          choices = c("State", "MSA", "Locality"),
          selected = "State"
        ),
        selectInput(
          inputId = "geo_name",
          label = "Name",
          choices = NULL
        )
      ),
      
      # MAIN PANEL
      mainPanel(
        width = 9,
        # Info boxes
        fluidRow(
          column(width = 3, style = "padding: 0 7px;",
                 uiOutput("dateBox")),
          column(width = 3, style = "padding: 0 7px;",
                 uiOutput("salesBox")),
          column(width = 3, style = "padding: 0 7px;",
                 uiOutput("priceBox")),
          column(width = 3, style = "padding: 0 7px;",
                 uiOutput("daysBox"))
        ),
        
        # Top row with price chart
        div(class = "top-row-container",
            fluidRow(
              div(
                class = "custom-box",
                div(
                  class = "box-header",
                  h3(class = "box-title", "Median Sales Price by Quarter"),
                  div(style = "position: absolute; top: 10px; right: 10px;",
                      downloadButton("downloadSalesPDF", "PDF", class = "btn-xs download-btn"),
                      downloadButton("downloadSalesPNG", "PNG", class = "btn-xs download-btn")
                  )
                ),
                div(
                  class = "box-body chart-container",  # Added chart-container class
                  girafeOutput("price")  # Removed fixed height
                )
              )
            )
        ),
        
        # Bottom row with two charts
        div(class = "bottom-row-container",
            fluidRow(
              column(
                width = 6,
                div(
                  class = "custom-box",
                  div(
                    class = "box-header",
                    h3(class = "box-title", "Homes Sold by Quarter"),
                    div(style = "position: absolute; top: 10px; right: 10px;",
                        downloadButton("downloadPricePDF", "PDF", class = "btn-xs download-btn"),
                        downloadButton("downloadPricePNG", "PNG", class = "btn-xs download-btn")
                    )
                  ),
                  div(
                    class = "box-body chart-container",  # Added chart-container class 
                    girafeOutput("sales")  # Removed fixed height
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "custom-box",
                  div(
                    class = "box-header",
                    h3(class = "box-title", "Median Days on Market by Quarter"),
                    div(style = "position: absolute; top: 10px; right: 10px;",
                        downloadButton("downloadDaysPDF", "PDF", class = "btn-xs download-btn"),
                        downloadButton("downloadDaysPNG", "PNG", class = "btn-xs download-btn")
                    )
                  ),
                  div(
                    class = "box-body chart-container",  # Added chart-container class
                    girafeOutput("days")  # Removed fixed height
                  )
                )
              )
            )
        )
      )
    )
  )
)

# Add custom CSS to handle responsive charts and sidebar styling
ui <- tagList(
  tags$head(
    tags$style(HTML("
      /* Variables for consistent colors */
      :root {
        --primary: #3c6382;
        --secondary: #60a3bc;
        --accent: #0a3d62;
        --light-bg: #f5f8fa;
        --dark-text: #2c3e50;
        --light-text: #ecf0f1;
      }
      
      /* Overall styling */
      body {
        background-color: var(--light-bg);
        color: var(--dark-text);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', sans-serif;
      }
      
      /* Title in sidebar */
      .sidebar-title {
        color: white;
        margin-bottom: 20px;
        border-bottom: 1px solid rgba(255,255,255,0.2);
        padding-bottom: 10px;
      }
      
      /* Sidebar styling - improved */
      .well {
        background-color: #2c3e50;
        padding: 20px;
        border-radius: 8px;
        color: var(--light-text);
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        border: none;
      }
      
      /* Logo styling */
      .logo-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 1px solid rgba(255,255,255,0.1);
      }
      .main-logo, .secondary-logo {
        max-width: 45%;
        height: auto;
      }
      
      /* Input styling */
      .form-control {
        border-radius: 4px;
        border: 1px solid #ddd;
        padding: 10px;
        font-size: 14px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .selectize-dropdown {
        box-shadow: 0 2px 6px rgba(0,0,0,0.2);
        border-radius: 4px;
      }
      .control-label {
        font-weight: 600;
        color: var(--light-text);
        margin-bottom: 8px;
        margin-top: 12px;
      }
      
      /* Chart container - NEW */
      .chart-container {
        position: relative;
        width: 100%;
        min-height: 300px;
        overflow: hidden;
      }
      
      /* Info box styling - FIXED HEIGHT */
      .info-box {
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        border-radius: 8px;
        height: 100px; /* Fixed height instead of min-height */
        transition: all 0.2s ease;
        margin-bottom: 15px;
        position: relative;
        overflow: hidden;
      }
      .info-box:hover {
        transform: translateY(-3px);
        box-shadow: 0 4px 15px rgba(0,0,0,0.15);
      }
      .info-box-icon {
        height: 100px;
        width: 90px;
        border-radius: 8px 0 0 8px;
        text-align: center;
        line-height: 100px;
        position: absolute;
        top: 0;
        left: 0;
        color: white;
        font-size: 24px;
      }
      .info-box-content {
        margin-left: 90px;
        padding: 10px 10px;
        position: relative;
        height: 100px; /* Fixed height to match parent */
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      .info-box-text {
        display: block;
        font-size: 14px;
        white-space: normal;
        overflow: visible;
        text-overflow: unset;
        text-transform: uppercase;
        font-weight: 600;
        color: var(--dark-text);
      }
      .info-box-number {
        display: block;
        font-weight: 500;
        font-size: 20px;
        white-space: normal;
        word-wrap: break-word;
        margin-top: 5px;
      }
      
      /* Box styling */
      .custom-box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        border-top: 3px solid var(--secondary);
        background-color: white;
      }
      .box-header {
        padding: 15px;
        border-bottom: 1px solid #f4f4f4;
        position: relative;
      }
      .box-title {
        font-size: 16px;
        font-weight: 600;
        color: var(--dark-text);
      }
      .box-body {
        padding: 10px;
        position: relative;
      }
      
      /* Download button styling */
      .download-btn {
        margin-right: 5px;
        margin-top: 5px;
        font-size: 12px;
      }
      
      /* Custom colors */
      .bg-purple {background-color: #8B85CA !important;}
      .bg-red {background-color: #B1005F !important;}
      .bg-teal {background-color: #259591 !important;}
      .bg-navy {background-color: #011E41 !important;}
      
      /* Plot styling - IMPROVED */
      .girafe {
        width: 100% !important;
      }
      .girafe svg {
        width: 100% !important;
        height: auto !important; /* Let height adjust automatically */
        min-height: 300px; /* Minimum height for better visibility */
      }
      
      /* Ensure charts scale with container */
      .box-body .girafe {
        overflow: visible;
        display: block;
      }
      
      /* Spacing */
      .top-row-container {
        margin-bottom: 20px;
      }
      .bottom-row-container {
        margin-top: 20px;
      }
      
      /* Responsive adjustments - IMPROVED */
      @media (max-width: 1200px) {
        .col-sm-3 {
          width: 50%;
          float: left;
          margin-bottom: 10px;
        }
      }
      @media (max-width: 768px) {
        .col-sm-3 {
          width: 100%;
          float: none;
        }
        .sidebar {
          margin-bottom: 20px;
        }
        .chart-container {
          min-height: 250px;
        }
        .girafe svg {
          min-height: 250px;
        }
      }
    "))
  ),
  ui
)

server <- function(input, output, session) {
  # Define absolute path to the logo for server-side operations
  logo_path <- normalizePath(file.path(getwd(), "..", "www", "hfv_rgb_logo.png"))
  
  # Copy the logo to the app directory for direct access
  source_logo <- file.path(getwd(), "..", "www", "hfv_rgb_logo.png")
  dest_logo <- file.path(getwd(), "www")
  
  # Create www directory in the app folder if it doesn't exist
  if (!dir.exists(dest_logo)) {
    dir.create(dest_logo, recursive = TRUE)
  }
  
  # Copy the logo file
  dest_logo <- file.path(dest_logo, "hfv_rgb_logo.png")
  if (!file.exists(dest_logo)) {
    file.copy(source_logo, dest_logo)
  }
  
  # Now logo can be referenced simply as "hfv_rgb_logo.png" in the UI
  
  # Function to add logos to ggplot objects
  # Now just returns the original plot without adding logos
  add_logos_to_plot <- function(plot) {
    # Simply return the original plot without modification
    return(plot)
  }
  # Load data
  var_data <- read_rds(here("data", "rds", "home-sales.rds"))
  
  # Define latest quarter
  latest_quarter <- max(var_data$quarter)
  
  # Create a data frame for the name list - Now sorted alphabetically
  name_list <- var_data %>%
    select(geography, name) %>%
    distinct() %>%
    arrange(geography, name)  # Sort alphabetically by geography and then by name
  
  # Observer that updates the name dropdown based on geography selection
  observe({
    # Get geography type from input
    selected_geo <- input$geo_type
    
    # Filter name_list to only show names matching the selected geography type
    # Now sorted alphabetically
    filtered_names <- name_list %>%
      filter(geography == selected_geo) %>%
      arrange(name) %>%  # Sort alphabetically
      pull(name)
    
    # Update the name dropdown
    updateSelectInput(
      session = session,
      inputId = "geo_name",
      label = paste(selected_geo, "Name"),
      choices = filtered_names,
      selected = if(length(filtered_names) > 0) filtered_names[1] else NULL
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
  
  # Create custom info box function
  createInfoBox <- function(title, value, icon_name, color) {
    div(
      class = "info-box",
      div(class = paste0("info-box-icon ", color), 
          icon(icon_name)),
      div(
        class = "info-box-content",
        div(class = "info-box-text", title),
        div(class = "info-box-number", value)
      )
    )
  }
  
  # Render info boxes
  output$dateBox <- renderUI({
    createInfoBox(
      "Latest Quarter", 
      as.character(latest_quarter),
      "calendar",
      "bg-red"
    )
  })
  
  output$salesBox <- renderUI({
    req(latest_data())
    units_val <- if(nrow(latest_data()) > 0) {
      # Format units with commas
      format(latest_data()$units[1], big.mark = ",", trim = TRUE)
    } else "N/A"
    
    createInfoBox(
      "Units Sold in Quarter", 
      units_val,
      "home",
      "bg-purple"
    )
  })
  
  output$priceBox <- renderUI({
    req(latest_data())
    price_val <- if(nrow(latest_data()) > 0) {
      # Format price as currency
      dollar_format()(latest_data()$med_price[1])
    } else "N/A"
    
    createInfoBox(
      "Median Sales Price in Quarter", 
      price_val,
      "dollar-sign",
      "bg-teal"
    )
  })
  
  output$daysBox <- renderUI({
    req(latest_data())
    dom_val <- if(nrow(latest_data()) > 0) latest_data()$med_dom[1] else "N/A"
    
    createInfoBox(
      "Median DOM in Quarter", 
      dom_val,
      "clock",
      "bg-navy"
    )
  })
  
  # Modified girafe outputs for improved responsive sizing
  output$sales <- renderGirafe({
    req(dashboard_data())
    
    # Create a data frame with tooltips for each bar
    plot_data <- dashboard_data() %>%
      mutate(
        tooltip = paste0(
          "Quarter: ", quarter, 
          "<br>Units Sold: ", format(units, big.mark = ",")))
    
    # Create the ggplot object
    gg <- ggplot(plot_data, aes(x = quarter, y = units)) +
      geom_col_interactive(
        aes(tooltip = tooltip, 
            data_id = quarter,
            fill = units), 
        alpha = 0.9) + 
      scale_fill_gradient(low = "#d0cee9", high = "#8B85CA") +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) + 
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(10, 10, 20, 10), # Standard margins (no extra for logos)
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      ) 
    
    # Add logos to the plot
    gg <- add_logos_to_plot(gg)
    
    # Improved girafe settings with better responsiveness
    girafe(
      ggobj = gg,
      width_svg = 8,  # Adjust SVG dimensions for better scaling
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE), # Let it resize fully with container
        opts_toolbar(position = "topright", saveaspng = FALSE),
        opts_hover(css = "fill:orange;"),
        opts_selection(type = "multiple", css = "fill:red;stroke:gray;")
      )
    )
  })
  
  output$price <- renderGirafe({
    req(dashboard_data())
    
    # Create a data frame with tooltips
    plot_data <- dashboard_data() %>%
      mutate(tooltip = paste0("Quarter: ", quarter, 
                              "<br>Median Price: ", 
                              dollar_format()(med_price)))
    
    # Create the ggplot object
    gg <- ggplot(plot_data, 
                 aes(
                   x = quarter, 
                   y = med_price)) +
      geom_col_interactive(
        aes(tooltip = tooltip, 
            data_id = quarter,
            fill = med_price), alpha = 0.8) +
      scale_fill_gradient(low = "#a7d4d3", high = "#259591") +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(10, 10, 20, 10), # Standard margins
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      ) +
      scale_y_continuous(labels = dollar_format())
    
    # Add logos to the plot
    gg <- add_logos_to_plot(gg)
    
    # Improved girafe settings with better responsiveness
    girafe(
      ggobj = gg,
      width_svg = 8,  # Adjust SVG dimensions for better scaling
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE), # Let it resize fully with container
        opts_toolbar(position = "topright", saveaspng = FALSE),
        opts_hover(css = "fill:orange;"),
        opts_selection(type = "multiple", css = "fill:red;stroke:gray;")
      )
    )
  })
  
  output$days <- renderGirafe({
    req(dashboard_data())
    
    # Create a data frame with tooltips
    plot_data <- dashboard_data() %>%
      mutate(tooltip = paste0("Quarter: ", quarter, 
                              "<br>Median Days on Market: ", med_dom))
    
    # Create the ggplot object
    gg <- ggplot(plot_data, 
                 aes(
                   x = quarter, 
                   y = med_dom)) +
      geom_col_interactive(
        aes(tooltip = tooltip,
            data_id = quarter, 
            fill = med_dom), 
        alpha = 0.8) +
      scale_fill_gradient(low = "#99a5b3", high = "#011E41") +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(10, 10, 20, 10), # Standard margins
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      ) 
    
    # Add logos to the plot
    gg <- add_logos_to_plot(gg)
    
    # Improved girafe settings with better responsiveness
    girafe(
      ggobj = gg,
      width_svg = 8,  # Adjust SVG dimensions for better scaling
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE), # Let it resize fully with container
        opts_toolbar(position = "topright", saveaspng = FALSE),
        opts_hover(css = "fill:orange;"),
        opts_selection(type = "multiple", css = "fill:red;stroke:gray;")
      )
    )
  })
  
  # Generate filename prefix for downloads based on current selection
  get_filename_prefix <- reactive({
    paste0(input$geo_type, "-", gsub(" ", "_", input$geo_name), "-")
  })
  
  # Non-interactive plots for downloads - updated to include logos
  get_sales_plot_for_download <- function() {
    req(dashboard_data())
    gg <- ggplot(dashboard_data(), aes(x = quarter, y = units)) +
      geom_col(fill = "#3c6382", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Units Sold by Quarter -", input$geo_name),
           x = "Quarter",
           y = "Units",
           caption = paste("Data for", input$geo_type, "-", input$geo_name)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(30, 20, 20, 10) # More margin for logos
      )
    
    # Add logos to the plot
    add_logos_to_plot(gg)
  }
  
  get_price_plot_for_download <- function() {
    req(dashboard_data())
    gg <- ggplot(dashboard_data(), aes(x = quarter, y = med_price)) +
      geom_col(fill = "#3c6382", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Median Sales Price by Quarter -", input$geo_name),
           x = "Quarter", 
           y = "Median Price ($)",
           caption = paste("Data for", input$geo_type, "-", input$geo_name)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(30, 20, 20, 10) # More margin for logos
      ) +
      scale_y_continuous(labels = dollar_format())
    
    # Add logos to the plot
    add_logos_to_plot(gg)
  }
  
  get_days_plot_for_download <- function() {
    req(dashboard_data())
    gg <- ggplot(dashboard_data(), aes(x = quarter, y = med_dom)) +
      geom_col(fill = "#3c6382", alpha = 0.8) +
      geom_point(size = 3, color = "#e74c3c") +
      theme_minimal() +
      labs(title = paste("Median Days on Market by Quarter -", input$geo_name),
           x = "Quarter",
           y = "Days",
           caption = paste("Data for", input$geo_type, "-", input$geo_name)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(30, 20, 20, 10) # More margin for logos
      )
    
    # Add logos to the plot
    add_logos_to_plot(gg)
  }
  
  # Download handlers - no changes
  output$downloadSalesPDF <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "units_sold.pdf")
    },
    content = function(file) {
      ggsave(file, plot = get_sales_plot_for_download(), device = "pdf", width = 8, height = 5)
    }
  )
  
  output$downloadSalesPNG <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "units_sold.png")
    },
    content = function(file) {
      ggsave(file, plot = get_sales_plot_for_download(), device = "png", width = 8, height = 5, dpi = 300)
    }
  )
  
  output$downloadPricePDF <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "median_price.pdf")
    },
    content = function(file) {
      ggsave(file, plot = get_price_plot_for_download(), device = "pdf", width = 8, height = 5)
    }
  )
  
  output$downloadPricePNG <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "median_price.png")
    },
    content = function(file) {
      ggsave(file, plot = get_price_plot_for_download(), device = "png", width = 8, height = 5, dpi = 300)
    }
  )
  
  output$downloadDaysPDF <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "days_on_market.pdf")
    },
    content = function(file) {
      ggsave(file, plot = get_days_plot_for_download(), device = "pdf", width = 8, height = 5)
    }
  )
  
  output$downloadDaysPNG <- downloadHandler(
    filename = function() {
      paste0(get_filename_prefix(), "days_on_market.png")
    },
    content = function(file) {
      ggsave(file, plot = get_days_plot_for_download(), device = "png", width = 8, height = 5, dpi = 300)
    }
  )
}

shinyApp(ui, server)