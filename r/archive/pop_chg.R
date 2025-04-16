library(tidyverse)
library(tidycensus)

# Get Components of Population Change data -------------------------------------

change_2010s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = c("NATURALINC", "DOMESTICMIG", "INTERNATIONALMIG"),
  year = 2019,
  time_series = TRUE
) |> 
  mutate(year = # Translate date codes into years
           case_when(
             PERIOD == 1 ~ "2010",
             PERIOD == 2 ~ "2011",
             PERIOD == 3 ~ "2012",
             PERIOD == 4 ~ "2013",
             PERIOD == 5 ~ "2014",
             PERIOD == 6 ~ "2015",
             PERIOD == 7 ~ "2016",
             PERIOD == 8 ~ "2017",
             PERIOD == 9 ~ "2018",
             PERIOD == 10 ~ "2019")) |> 
  select(GEOID, variable, year, value)


change_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = c("NATURALCHG", "DOMESTICMIG", "INTERNATIONALMIG"),
  year = 2022,
  time_series = TRUE
) |> 
  select(GEOID, variable, year, value)

# Combine data from the 2010s and 2020s ----------------------------------------

change_data <- rbind(change_2010s, change_2020s) |> 
  mutate(component = # Rename components of change
           case_when(
             variable == "NATURALINC" ~ "Natural increase",
             variable == "NATURALCHG" ~ "Natural increase", 
             variable == "DOMESTICMIG" ~ "Domestic migration",
             variable == "INTERNATIONALMIG" ~ "International migration"))

# Join data to the Virginia locality crosswalk csv -----------------------------

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

change_data_join <- change_data |> 
  left_join(lookup, by = 'GEOID')

# Write data to rds format in data folder and note the date below --------------

# Data up-to-date as of: 5-22-23

# write_rds(change_data_join, "data/pop_change.rds")
write_rds(change_data_join, "data/pop_change.rds")
