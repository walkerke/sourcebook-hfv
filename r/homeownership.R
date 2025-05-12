library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)

# Add census API key if needed
# census_api_key("YOUR_KEY_HERE")

# Helper function to get homeownership data for a range of years
get_homeownership_data <- function(yr, geography) {
  # Create an empty list to store results
  results_list <- list()
  
  # Loop through each year and get the data
  for (year in yr) {
    message("Getting data for year ", year)
    
    # Handle different data sources based on year
    if (year < 2010) {
      # Pre-2010 data may be in Decennial Census
      # Add logic as needed
      next
    }
    
    # Get ACS data
    data <- tryCatch({
      get_acs(
        geography = geography,
        table = "B25003",
        state = "VA",
        year = year,
        survey = "acs5",
        geometry = FALSE
      )
    }, error = function(e) {
      message("Error getting data for year ", year, ": ", e$message)
      return(NULL)
    })
    
    # If successful, add year column and append to list
    if (!is.null(data)) {
      data$year <- year
      results_list[[length(results_list) + 1]] <- data
    }
  }
  
  # Combine all years
  combined <- bind_rows(results_list)
  return(combined)
}

# Get tract geometry once (most recent year) for the map
va_tracts_geo <- get_acs(
  geography = "tract",
  table = "B25003",
  state = "VA",
  year = current_acs, 
  geometry = TRUE,
  resolution = "5m"
) 

# Get statewide, county, tract homeownership data for all available years
state_homeownership <- get_homeownership_data(years, geography = "state")
county_homeownership <- get_homeownership_data(years, geography = "county")
tract_homeownership <- get_homeownership_data(years, geography = "tract")

# Pull the table variables for processing
b25003_defns <- load_variables(current_acs, "acs5") %>%
  filter(str_detect(name, "B25003_")) |> 
  separate(label, into = c("est", "total", "tenure"), fill = "right") |> 
  mutate(
    tenure = case_when(
      tenure == "Owner" ~ "Homeowner",
      tenure == "Renter" ~ "Renter",
      TRUE ~ "All"),
    variable = name)

# Process state data
state_ho_data <- state_homeownership %>%
  left_join(b25003_defns, by = "variable") %>%
  select(GEOID, NAME, year, estimate, tenure) %>%
  pivot_wider(
    names_from = tenure,
    values_from = estimate
  ) %>%
  mutate(ho_rate = ((Homeowner/All)*100)) %>%
  rename(state = NAME) %>% 
  mutate(geography = "State") %>% 
  mutate(tract = "Not tract level",,
         jurisdiction = "Not at jurisdiction level") %>% 
  select(-Renter, -state)

# Process county data
county_ho_data <- county_homeownership %>%
  left_join(b25003_defns, by = "variable") %>%
  select(GEOID, NAME, year, estimate, tenure) %>%
  pivot_wider(
    names_from = tenure,
    values_from = estimate
  ) %>%
  mutate(ho_rate = ((Homeowner/All)*100)) %>%
  rename(jurisdiction = NAME) %>%
  # Clean jurisdiction names (remove "County, Virginia")
  mutate(jurisdiction = str_replace(jurisdiction, " County, Virginia", "")) %>% 
  mutate(jurisdiction = str_replace(jurisdiction, ", Virginia", "")) %>% 
  mutate(geography = "Jurisdiction") %>% 
  mutate(tract = "Not tract-level") %>% 
  select(-Renter)

# Process tract data
tract_ho_data <- tract_homeownership %>%
  left_join(b25003_defns, by = "variable") %>%
  select(GEOID, NAME, year, estimate, tenure) %>%
  pivot_wider(
    names_from = tenure,
    values_from = estimate
  ) %>%
  mutate(ho_rate = ((Homeowner/All)*100)) %>%
  mutate(ho_rate = ifelse(ho_rate == 0, NA, ho_rate)) %>%
  # Extract tract, county, and state from NAME using both comma and semicolon as delimiters
  separate(NAME, into = c("tract", "jurisdiction", "state"), sep = "[,;]") %>%
  # Clean county names
  mutate(jurisdiction = str_replace(jurisdiction, " County", "")) %>% 
  mutate(geography = "Tract") %>% 
  select(-Renter, -state)

# Match tracts to counties for easier joining
tract_map_data <- va_tracts_geo %>%
  left_join(
    tract_ho_data %>% 
      select(GEOID, tract, jurisdiction, ho_rate, All, Homeowner),
    by = "GEOID"
  ) %>%
  # Add county-level homeownership rates
  group_by(jurisdiction) %>%
  mutate(
    jurisdiction_total = sum(All, na.rm = TRUE),
    jurisdiction_homeowners = sum(Homeowner, na.rm = TRUE),
    jurisdiction_ho_rate = (jurisdiction_homeowners/jurisdiction_total*100)
  ) %>%
  ungroup() %>%
  # Create custom tooltip
  mutate(custom_tooltip = case_when(
    is.na(ho_rate) ~ paste0(
      tract,
      "<br><b>Jurisdiction: </b>", jurisdiction,
      "<br><b>Homeownership Rate:</b> No data",
      "<br><b>County Homeownership Rate: </b>", round(jurisdiction_ho_rate, 1), "%"
    ),
    TRUE ~ paste0(
      tract,
      "<br><b>Jurisdiction: </b>", jurisdiction,
      "<br><b>Homeownership Rate: </b>", round(ho_rate, 1), "%",
      "<br><b>County Homeownership Rate: </b>", round(jurisdiction_ho_rate, 1), "%"
    )
  ))

# Get simplified county boundaries
va_counties <- counties(state = "VA", year = 2021) %>%
  select(GEOID, NAME) %>%
  # Clean jurisdiction names to match our data
  mutate(jurisdiction = str_replace(NAME, " County", ""))

  

write_rds(state_ho_data, "shiny/ho_rate/state_ho_data.rds")
write_rds(county_ho_data, "shiny/ho_rate/county_ho_data.rds")
write_rds(tract_ho_data, "shiny/ho_rate/tract_ho_data.rds")
write_rds(tract_map_data, "shiny/ho_rate/tract_map_data.rds")
write_rds(va_counties, "data/va_co_shape.rds")



trend_data <- rbind(county_ho_data, tract_ho_data, state_ho_data)

write_rds(trend_data, "shiny/ho_rate/trend_data.rds")


library(sf)
tract_map_data <- readRDS(here("shiny", "ho_rate", "tract_map_data.rds"))

tract_data_simplified <- tract_map_data |> 
  st_simplify(dTolerance = 0.001)

write_rds(tract_data_simplified, "shiny/ho_rate/_tract_data_simplified.rds")

