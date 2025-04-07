library(tidyverse)
library(tidycensus)
library(fredr)
library(lubridate)

# Use the fredR package to get Consumer Price Index for All Urban Consumers from
# FRED. The CPI will be used to adjust median household income for inflation.

cpi <- fredr(
  series_id = "CPIAUCSL" # ID for CPI for All Urban Consumers
) |> 
  select(date, value) |> # Select date and CPI
  mutate(date = as.Date(date), # Convert date to date data type.
         value = as.numeric(value), # Convert CPI to a numeric value.
         year = year(date)) |> # Create a field for the year and extract year from date.
  group_by(year) |> # Group by year. 
  summarise(index = mean(value)) # Calculate annual average CPI. 

# Create a function to convert median household income from ACS to most recent 
# inflation-adjusted dollar value.
adjustment <- function(x) {
  transform(x, adjusted = ((270.97142/index)*estimate))
}


# Create object for years needed. 
years <- 2010:2022

#  B25119: Median Household Income by Tenure

# Load variable names for B25119 and clean at the same time.

b25119_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) == "B25119")|> 
  separate(label, into = c("est", "medincome", "total", "tenure"), sep = "!!") |> 
  select(variable = name, tenure) |>
  mutate(tenure = case_when(
    tenure == "Owner occupied (dollars)" ~ "Homeowner",
    tenure == "Renter occupied (dollars)" ~ "Renter",
    TRUE ~ "All households"
  ))

# Median household income cannot be aggregated so you have to 
# pull data for each geographic level.

b25119_state <- map_dfr(years, function(yr){
  acs_pull <- get_acs(
    geography = "state",
    table = "B25119",
    survey = "acs5",
    year = yr
  ) |> 
    mutate(year = yr) |> 
    left_join(b25119_vars, by = "variable") |> 
    select(variable, year, state = NAME, fips = GEOID, tenure, estimate, moe)
})

b25119_cbsa <- map_dfr(years, function(yr){
  acs5_pull <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    table = "B25119",
    survey = "acs5",
    year = yr
  ) |> 
    mutate(year = yr) |> 
    left_join(b25119_vars, by = "variable") |> 
    select(variable, year, cbsa = NAME, GEOID, tenure,
           estimate, moe) |> 
    filter(str_detect(cbsa, "VA"))
})

b25119_local <- map_dfr(years, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25119",
    survey = "acs5",
    year = yr
  ) |> 
    mutate(year = yr) |> 
    left_join(b25119_vars, by = "variable") |> 
    select(variable, year, locality = NAME, GEOID, tenure,
           estimate, moe) |> 
    mutate(across(.fns = ~str_remove_all(.x, ", Virginia")),
           year = as.numeric(year),
           estimate = as.numeric(estimate))
})


med_inc_state <- b25119_state |> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(state, year, tenure, estimate, moe, adjusted) 

med_inc_cbsa <- b25119_cbsa |> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(GEOID, cbsa, year, tenure, estimate, moe, adjusted)

med_inc_local <- b25119_local |> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(locality, year, tenure, estimate, moe, adjusted) 



write_rds(med_inc_state, "shiny/med_inc_tenure/b25119_state.rds")
write_rds(med_inc_cbsa, "shiny/med_inc_tenure/b25119_cbsa.rds")
write_rds(med_inc_local, "shiny/med_inc_tenure/b25119_local.rds")