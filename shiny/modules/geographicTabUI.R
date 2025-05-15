# geoYearModuleUI.R
# 
# 

# UI function for the module
geoYearUI <- function(id, 
                          title = "Housing Type Distribution",
                          year_range = 2010:2023,
                          selected_year = 2023) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel(title),
    
    sidebarLayout(
      sidebarPanel(
        # Year filter for all tabs - now with modular parameters
        selectInput(ns("year"), "Select Year:",
                    choices = year_range,
                    selected = selected_year),
        
        # Conditional UI based on selected tab
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'cbsa'"),
          selectInput(ns("cbsa"), "Select Metro Area:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'local'"),
          selectInput(ns("locality"), "Select Locality:", choices = NULL)
        )
      ),
      
      mainPanel(
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("State", plotlyOutput(ns("state_plot")), value = "state"),
          tabPanel("Metro Area", plotlyOutput(ns("cbsa_plot")), value = "cbsa"),
          tabPanel("Locality", plotlyOutput(ns("local_plot")), value = "local")
        )
      )
    )
  )
}

# Server function for the module
housingTypeServer <- function(id, 
                              data, 
                              cbsa_list = NULL, 
                              locality_list = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Update the CBSA choices when the module initializes
    observe({
      if (!is.null(cbsa_list)) {
        updateSelectInput(session, "cbsa", choices = cbsa_list)
      }
    })
    
    # Update the locality choices when the module initializes
    observe({
      if (!is.null(locality_list)) {
        updateSelectInput(session, "locality", choices = locality_list)
      }
    })
    
    # Filtered data based on year
    filtered_data <- reactive({
      data %>%
        filter(year == input$year)
    })
    
    # State plot
    output$state_plot <- renderPlotly({
      req(filtered_data())
      
      # Your plotting code here for state-level data
      p <- plot_ly(filtered_data(), type = "bar") %>%
        # Add your specific plot data here
        layout(title = "State Housing Types",
               xaxis = list(title = "Housing Type"),
               yaxis = list(title = "Count"))
      
      return(p)
    })
    
    # CBSA plot
    output$cbsa_plot <- renderPlotly({
      req(filtered_data(), input$cbsa)
      
      cbsa_data <- filtered_data() %>%
        filter(cbsa == input$cbsa)
      
      # Your plotting code here for CBSA-level data
      p <- plot_ly(cbsa_data, type = "bar") %>%
        # Add your specific plot data here
        layout(title = paste("Housing Types in", input$cbsa),
               xaxis = list(title = "Housing Type"),
               yaxis = list(title = "Count"))
      
      return(p)
    })
    
    # Locality plot
    output$local_plot <- renderPlotly({
      req(filtered_data(), input$locality)
      
      local_data <- filtered_data() %>%
        filter(locality == input$locality)
      
      # Your plotting code here for locality-level data
      p <- plot_ly(local_data, type = "bar") %>%
        # Add your specific plot data here
        layout(title = paste("Housing Types in", input$locality),
               xaxis = list(title = "Housing Type"),
               yaxis = list(title = "Count"))
      
      return(p)
    })
  })
}