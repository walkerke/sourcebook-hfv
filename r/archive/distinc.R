library(tidyverse)
library(tidycensus)
library(lubridate)

years <- 2010:2022

b25118_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25118")

b25118_raw <- map_dfr(years, function(yr){
  b25118_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25118",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25118_vars_cleaned <- b25118_vars |> 
  separate(label, into = c("est", "total", "tenure", "income"), sep = "!!") |>  
  select(variable = name, tenure, income) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  )) |> 
  drop_na()

b25118_data <- b25118_raw |> 
  right_join(b25118_vars_cleaned, by = "variable") |> 
  select(GEOID, NAME, year, tenure, income, estimate, moe) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>
  mutate(income = case_when(
    income == "Less than $5,000" ~ "Less than $15,000",
    income == "$5,000 to $9,999" ~ "Less than $15,000",
    income == "$10,000 to $14,999" ~ "Less than $15,000",
    income == "$15,000 to $19,999" ~ "$15,000 to $24,999",
    income == "$20,000 to $24,999" ~ "$15,000 to $24,999",
    income == "$25,000 to $34,999" ~ "$25,000 to $49,999",
    income == "$35,000 to $49,999" ~ "$25,000 to $49,999",
    income == "$50,000 to $74,999" ~ "$50,000 to $74,999",
    TRUE ~ income
  )) |> 
  group_by(GEOID, NAME, year, tenure, income) |> 
  summarise(estimate = sum(estimate))  

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

b25118_data <- b25118_data |> 
  left_join(lookup, by = "GEOID")

write_rds(b25118_data, "shiny/inc_dist_tenure/b25118_data.rds")