library(tidyverse)
library(tidycensus)

# Download total population estimates from PEP for each Virginia locality from 2010 to 2019

pep_2010s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = "POP",
  year = 2019,
  time_series = TRUE
) |> 
  filter(!DATE %in% c(2, 3)) |> # Remove non-Decennial 2010 estimates
  mutate(year = # Translate date codes into years
           case_when(
             DATE == 1 ~ "2010",
             DATE == 4 ~ "2011",
             DATE == 5 ~ "2012",
             DATE == 6 ~ "2013",
             DATE == 7 ~ "2014",
             DATE == 8 ~ "2015",
             DATE == 9 ~ "2016",
             DATE == 10 ~ "2017",
             DATE == 11 ~ "2018",
             DATE == 12 ~ "2019")) |> 
  mutate(counttype = # Add descriptions to count types
           case_when(
             DATE == 1 ~ "Census population",
             TRUE ~ "Population estimate")) |>  
  select(GEOID, counttype,year, value) # Simplify data

# Download total population estimates from PEP for each Virginia locality from 2021 to 2022.

pep_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = "POPESTIMATE",
  year = 2022,
  time_series = TRUE
) |> 
  filter(year != 2020) |> # Remove non-Decennial 2020 count
  mutate(counttype = case_when( # Add descriptions to count types
    variable == "POPESTIMATE" ~ "Population estimate",
    TRUE ~ variable
  )) |> 
  select(GEOID,counttype, year, value) # Simplify data

# Download total population counts from 2020 Decennial Census.

census <- get_decennial(
  geography = "county",
  state = "VA",
  year = 2020,
  sumfile = "pl",
  variables = "P1_001N"
) |> 
  mutate(counttype = "Census population",
         year = 2020) |> 
  select(GEOID, counttype, year, value)

# Combine data frames into one and join to lookup table.

pop_data <- rbind(pep_2010s, pep_2020s, census) # Combine data frames

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

pop_data_join <- pop_data |> 
  left_join(lookup, by = 'GEOID')

# Write data to rds format in data folder and note the date below.

# Data up-to-date as of: 5-22-2023

write_rds(pop_data_join, "data/total_pop.rds")
write_rds(pop_data_join, "shiny/total_pop/total_pop.rds")