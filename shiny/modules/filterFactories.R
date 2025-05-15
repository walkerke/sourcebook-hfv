# inputFactories.R

# Create a select input specification
create_select_input <- function(id, label, choices = NULL, selected = NULL) {
  list(
    type = "selectInput",
    inputId = id,
    args = list(
      label = label,
      choices = choices,
      selected = selected
    ),
    update_function = "updateSelectInput"
  )
}

# Create a year range input (two select inputs in a row)
create_year_range_input <- function(id_prefix, year_list = NULL, 
                                  start_selected = NULL, end_selected = NULL) {
  list(
    start = create_select_input(
      paste0(id_prefix, "_start"), 
      "Start Year:", 
      choices = year_list,
      selected = start_selected
    ),
    end = create_select_input(
      paste0(id_prefix, "_end"), 
      "End Year:", 
      choices = year_list,
      selected = end_selected
    ),
    # Add dependency logic
    dependency = list(
      inputId = paste0(id_prefix, "_end"),
      depends_on = c(paste0(id_prefix, "_start")),
      dependency_handler = function(start_value) {
        # Ensure end year is not earlier than start year
        if (as.numeric(start_value) > as.numeric(end_selected)) {
          return(start_value)
        }
        return(end_selected)
      },
      update_function = "updateSelectInput"
    )
  )
}

# Create a tenure input
create_tenure_input <- function(id = "tenure", 
                               choices = c("All", "Homeowner", "Renter"),
                               selected = "All") {
  create_select_input(id, "Tenure:", choices, selected)
}

# Create a geography input
create_geography_input <- function(id, label, choices = NULL, selected = NULL) {
  create_select_input(id, label, choices, selected)
}

# Create a wrapped year range in a fluidRow with columns
create_year_range_row <- function(id_prefix, year_list = NULL, 
                                start_selected = NULL, end_selected = NULL) {
  year_inputs <- create_year_range_input(id_prefix, year_list, start_selected, end_selected)
  
  return(list(
    type = "fluidRow",
    inputId = paste0(id_prefix, "_row"),
    args = list(
      column(6, do.call("selectInput", c(list(inputId = year_inputs$start$inputId), 
                                        year_inputs$start$args))),
      column(6, do.call("selectInput", c(list(inputId = year_inputs$end$inputId), 
                                        year_inputs$end$args)))
    ),
    update_function = NULL,
    dependency = year_inputs$dependency
  ))
}