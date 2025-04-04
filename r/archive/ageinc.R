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
  transform(x, adjusted = ((292.61250/index)*estimate)) #2022 value is 292.61250
}

# Create object for years.

years <- 2010:2022

# Get variables for Table B19049 and clean the variables.

b19049_vars <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) %in% "B19049")

b19049_vars_cleaned <- b19049_vars %>%
  separate(label, c("estimate", "medhhincome", "total", "age"), sep = "!!") %>%
  select(variable = name, medhhincome, age) %>%
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")),
         across(.fns =~ str_remove_all(.x, "in the past 12 months  \\(\\in 2021 inflation-adjusted dollars\\)\\ --")),
         across(.fns = ~str_remove_all(.x, "Householder ")),
         age = case_when(
           age == "under 25 years" ~ "24 years and under",
           TRUE ~ age))

# Table B19049 - Median Household Income by Age of Householder

output_b19049_state <- map_dfr(years, function(yr) {
  acs_pull <- get_acs(
    geography = "state",
    table = "B19049",
    year = yr
  ) %>%
    left_join(b19049_vars_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, state = NAME, fips = GEOID, medhhincome, age,
           estimate, moe)
  
  acs_rearranged
})

output_b19049_cbsa <- map_dfr(years, function(yr) {
  acs_pull <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    table = "B19049",
    year = yr
  ) %>%
    left_join(b19049_vars_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, cbsa = NAME, fips = GEOID, medhhincome, age,
           estimate, moe) %>%
    filter(str_detect(cbsa, "VA"))
  
  acs_rearranged 
})

output_b19049_locality <- map_dfr(years, function(yr) {
  acs_pull <- get_acs(
    geography = "county",
    state = "VA", 
    table = "B19049",
    year = yr
  ) %>%
    left_join(b19049_vars_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, locality = NAME, fips = GEOID, medhhincome, age,
           estimate, moe)
  
  acs_rearranged
})

state_adj <- output_b19049_state |> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(state, year, age, moe, estimate, adjusted) |> 
  filter(age != "All")


cbsa_adj <- output_b19049_cbsa |> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(cbsa, year, age, moe, estimate, adjusted) |> 
  filter(age != "All")

locality_adj <- output_b19049_locality|> 
  left_join(cpi, by = "year") |> 
  adjustment() |> 
  select(locality, year, age, moe, estimate, adjusted) |> 
  mutate(across(.fns = ~str_remove_all(.x, ", Virginia"))) |> 
  filter(age != "All")

write_rds(state_adj, "shiny/med_inc_age/b19049_state.rds")
write_rds(cbsa_adj, "shiny/med_inc_age/b19049_cbsa.rds")
write_rds(locality_adj, "shiny/med_inc_age/b19049_locality.rds")