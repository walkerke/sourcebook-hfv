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
poverty_race <- read_rds(here("data", "rds", "poverty_race.rds"))
poverty_age <- read_rds(here("data", "rds", "poverty_age.rds"))

# Create lists for filters
cbsa_list_race <- sort(unique(poverty_race$cbsa_title))
locality_list_race <- sort(unique(poverty_race$locality))
cbsa_list_age <- sort(unique(poverty_age$cbsa_title))
locality_list_age <- sort(unique(poverty_age$locality))

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
      h4("Poverty Rate Analysis", style = "margin: 0; color: #011E41;")
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
          
          # Analysis type selector
          div(
            style = "margin-bottom: 0;",
            radioButtons("analysis_type", "Analysis Type:",
                         choices = list("By Race/Ethnicity" = "race", 
                                        "By Age Group" = "age"),
                         selected = "race",
                         inline = FALSE)
          ),
          
          # Geography selectors with minimal height
          div(
            style = "margin-bottom: 0;",
            conditionalPanel(
              condition = "input.tabs == 'cbsa' && input.analysis_type == 'race'",
              selectInput("race_cbsa_select", "Metro Area:", 
                          choices = cbsa_list_race,
                          selected = if("Richmond, VA" %in% cbsa_list_race) "Richmond, VA" else cbsa_list_race[1],
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local' && input.analysis_type == 'race'",
              selectInput("race_locality_select", "Locality:", 
                          choices = locality_list_race,
                          selected = if("Richmond city" %in% locality_list_race) "Richmond city" else locality_list_race[1],
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'cbsa' && input.analysis_type == 'age'",
              selectInput("age_cbsa_select", "Metro Area:", 
                          choices = cbsa_list_age,
                          selected = if("Richmond, VA" %in% cbsa_list_age) "Richmond, VA" else cbsa_list_age[1],
                          width = "100%", 
                          selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.tabs == 'local' && input.analysis_type == 'age'",
              selectInput("age_locality_select", "Locality:", 
                          choices = locality_list_age,
                          selected = if("Richmond city" %in% locality_list_age) "Richmond city" else locality_list_age[1],
                          width = "100%", 
                          selectize = FALSE)
            )
          ),
          
          # Horizontal line
          hr(style = "margin: 3px 0;"),
          
          # Tooltip info
          div(
            style = "margin-top: 5px; margin-bottom: 5px; font-size: 10px;",
            p("Hover over points to see details", style = "margin-bottom: 5px;"),
            verbatimTextOutput("hover_info", placeholder = TRUE)
          ),
          
          # Source information
          div(
            style = "font-size: 10px; color: #666; margin-top: 2px;",
            p(
              "Source: U.S. Census Bureau, American Community Survey 5-Year Estimates.",
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
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_state_plot", height = "390px")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_state_plot", height = "390px")
              )
            ),
            nav_panel(
              title = "Metro Area", 
              value = "cbsa",
              padding = 5,
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_cbsa_plot", height = "390px")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_cbsa_plot", height = "390px")
              )
            ),
            nav_panel(
              title = "Locality", 
              value = "local",
              padding = 5,
              conditionalPanel(
                condition = "input.analysis_type == 'race'",
                girafeOutput("race_locality_plot", height = "390px")
              ),
              conditionalPanel(
                condition = "input.analysis_type == 'age'",
                girafeOutput("age_locality_plot", height = "390px")
              )
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Define race colors
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
  
  # Define age colors
  age_colors <- c(
    "17 years and under" = "#FFC658",    # Desert variant
    "18 to 24 years" = hfv_colors$desert, # Desert
    "25 to 34 years" = hfv_colors$grass,  # Grass
    "35 to 44 years" = hfv_colors$sky,    # Sky
    "45 to 54 years" = hfv_colors$lilac,  # Lilac
    "55 to 64 years" = hfv_colors$berry,  # Berry
    "65 years and over" = hfv_colors$shadow # Shadow
  )
  
  #----- RACE DATA PROCESSING -----#
  
  # Process race state data
  race_state_data <- reactive({
    # Process race state data
    pov_race_state <- poverty_race %>% 
      group_by(year, race) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalrace)
    
    # Calculate the mean rate for each race to help determine order of facets
    state_summary <- pov_race_state %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    pov_race_state %>%
      mutate(race_ordered = factor(race, levels = state_summary$race))
  })
  
  # Process race CBSA data
  filtered_race_cbsa_data <- reactive({
    req(input$race_cbsa_select)
    
    cbsa <- poverty_race %>% 
      group_by(year, race, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalrace = sum(totalrace),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalrace) %>% 
      filter(cbsa_title == input$race_cbsa_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    cbsa_summary <- cbsa %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    cbsa %>%
      mutate(race_ordered = factor(race, levels = cbsa_summary$race))
  })
  
  # Process race locality data
  filtered_race_locality_data <- reactive({
    req(input$race_locality_select)
    
    local <- poverty_race %>% 
      filter(locality == input$race_locality_select)
    
    # Calculate the mean rate for each race to help determine order of facets
    local_summary <- local %>%
      group_by(race) %>%
      summarize(mean_rate = mean(rate, na.rm = TRUE)) %>%
      arrange(desc(mean_rate))
    
    # Add ordered factor
    local %>%
      mutate(race_ordered = factor(race, levels = local_summary$race))
  })
  
  #----- AGE DATA PROCESSING -----#
  
  # Process age state data
  age_state_data <- reactive({
    poverty_age %>% 
      group_by(year, age, age_group) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage)
  })
  
  # Process age CBSA data
  filtered_age_cbsa_data <- reactive({
    req(input$age_cbsa_select)
    
    poverty_age %>% 
      group_by(year, age, age_group, cbsa_title) %>% 
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>% 
      mutate(rate = estimate/totalage) %>% 
      filter(cbsa_title == input$age_cbsa_select)
  })
  
  # Process age locality data
  filtered_age_locality_data <- reactive({
    req(input$age_locality_select)
    
    poverty_age %>% 
      filter(locality == input$age_locality_select) %>%
      group_by(year, age, age_group) %>%
      summarise(estimate = sum(estimate),
                totalage = sum(totalage),
                .groups = "drop") %>%
      mutate(rate = estimate/totalage)
  })
  
  # Function to create race poverty rate plots
  create_race_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest year data for labels
    latest_year <- max(data$year)
    latest_data <- data %>% filter(year == latest_year)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    latest_data <- latest_data %>%
      mutate(tooltip = paste0(
        "Race/Ethnicity: ", race, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(x = year,
                    y = rate,
                    color = race_ordered,
                    group = race_ordered)) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = paste(year, race)),
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, race)),
        size = 2
      ) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~race_ordered, nrow = 1) +
      scale_color_manual(values = race_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format(), 
                         limits = c(0, NA)) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Poverty Rate",
        x = "Year"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(5, 15, 15, 5), # Extra right margin for labels and bottom for logo
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Add logo to the plot
    logo_path <- "www/hfv_logo.png"
    p_with_logo <- cowplot::ggdraw(p) +
      cowplot::draw_image(logo_path, 
                          x = 0.8, y = -0.05, 
                          width = 0.15, height = 0.15)
    
    return(p_with_logo)
  }
  
  # Function to create age poverty rate plots
  create_age_plot <- function(data, title_text) {
    req(nrow(data) > 0)
    
    # Get latest year data for labels
    latest_year <- max(data$year)
    latest_data <- data %>% filter(year == latest_year)
    
    # Create tooltips
    plot_data <- data %>%
      mutate(tooltip = paste0(
        "Age Group: ", age, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    latest_data <- latest_data %>%
      mutate(tooltip = paste0(
        "Age Group: ", age, "\n",
        "Year: ", year, "\n",
        "Poverty Rate: ", scales::percent(rate, accuracy = 0.1), "\n",
        "Number in Poverty: ", format(estimate, big.mark = ",")
      ))
    
    # Create base plot
    p <- ggplot(plot_data,
                aes(
                  x = year,
                  y = rate,
                  color = age,
                  group = age
                )) +
      geom_line_interactive(
        aes(tooltip = tooltip, data_id = paste(year, age)),
        linewidth = 1
      ) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = paste(year, age)),
        size = 2
      ) +
      # Add labels for latest values
      geom_text(data = latest_data, 
                aes(label = scales::percent(rate, accuracy = 0.1)),
                hjust = -0.3, vjust = 0.5) +
      facet_wrap(~age_group, nrow = 1) +
      scale_color_manual(values = age_colors) +
      # Format y-axis as percentage
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = title_text,
        caption = " ", # Add empty caption to leave space for logo
        y = "Poverty Rate",
        x = "Year"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(5, 15, 15, 5), # Extra right margin for labels and bottom for logo
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)
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
  
  # Plot titles
  race_state_title <- reactive({
    "Poverty Rate by Race/Ethnicity in Virginia"
  })
  
  race_cbsa_title <- reactive({
    paste("Poverty Rate by Race/Ethnicity in", input$race_cbsa_select)
  })
  
  race_locality_title <- reactive({
    paste("Poverty Rate by Race/Ethnicity in", input$race_locality_select)
  })
  
  age_state_title <- reactive({
    "Poverty Rate by Age Group in Virginia"
  })
  
  age_cbsa_title <- reactive({
    paste("Poverty Rate by Age Group in", input$age_cbsa_select)
  })
  
  age_locality_title <- reactive({
    paste("Poverty Rate by Age Group in", input$age_locality_select)
  })
  
  # Render race plots
  output$race_state_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(race_state_data(), race_state_title()))
  })
  
  output$race_cbsa_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_race_cbsa_data(), race_cbsa_title()))
  })
  
  output$race_locality_plot <- renderGirafe({
    create_interactive_plot(create_race_plot(filtered_race_locality_data(), race_locality_title()))
  })
  
  # Render age plots
  output$age_state_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(age_state_data(), age_state_title()))
  })
  
  output$age_cbsa_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(filtered_age_cbsa_data(), age_cbsa_title()))
  })
  
  output$age_locality_plot <- renderGirafe({
    create_interactive_plot(create_age_plot(filtered_age_locality_data(), age_locality_title()))
  })
  
  # Handle hover info for all plots
  get_hover_data <- reactive({
    if (input$analysis_type == "race") {
      if (input$tabs == "state") {
        data <- race_state_data()
        geo_name <- "Virginia"
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      } else if (input$tabs == "cbsa") {
        data <- filtered_race_cbsa_data()
        geo_name <- input$race_cbsa_select
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      } else { # locality
        data <- filtered_race_locality_data()
        geo_name <- input$race_locality_select
        demographic_type <- "Race/Ethnicity"
        demographic_field <- "race"
      }
    } else { # age
      if (input$tabs == "state") {
        data <- age_state_data()
        geo_name <- "Virginia"
        demographic_type <- "Age Group"
        demographic_field <- "age"
      } else if (input$tabs == "cbsa") {
        data <- filtered_age_cbsa_data()
        geo_name <- input$age_cbsa_select
        demographic_type <- "Age Group"
        demographic_field <- "age"
      } else { # locality
        data <- filtered_age_locality_data()
        geo_name <- input$age_locality_select
        demographic_type <- "Age Group"
        demographic_field <- "age"
      }
    }
    
    list(
      data = data,
      geo_name = geo_name,
      demographic_type = demographic_type,
      demographic_field = demographic_field
    )
  })
  
  # Display hover information
  output$hover_info <- renderText({
    hover_data <- get_hover_data()
    data <- hover_data$data
    
    # If there's no hover data, show a placeholder message
    if (is.null(data) || nrow(data) == 0) {
      return("Hover over a point for details")
    }
    
    geo_name <- hover_data$geo_name
    demo_type <- hover_data$demographic_type
    demo_field <- hover_data$demographic_field
    
    # Format some example hover data for display
    if (nrow(data) > 0) {
      # Take one row as an example
      example <- data[1,]
      demo_value <- example[[demo_field]]
      
      paste0(
        geo_name, "\n",
        demo_type, ": ", demo_value, "\n",
        "Hover for more details"
      )
    } else {
      "Hover over a point for details"
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)