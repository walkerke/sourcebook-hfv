# Load libraries
library(tidyverse)
library(tidycensus)

# Download population counts by age group from PEP for each Virginia locality from 2010 to 2019

age_10s <- get_estimates(
  geography = "county",
  state = "VA",
  product = "characteristics",
  breakdown = "AGEGROUP",
  breakdown_labels = TRUE,
  year = 2019,
  time_series = TRUE
)

# Download population counts by age group from PEP for each Virginia locality from 2020 to 2022

age_20s <- get_estimates(
  geography = "county",
  state = "VA",
  product = "characteristics",
  breakdown = "AGEGROUP",
  breakdown_labels = TRUE,
  year = 2022,
  time_series = TRUE
)

# Create list of age variables to extract from data

age_vars <- c(
  "Age 0 to 4 years",
  "Age 5 to 9 years",
  "Age 10 to 14 years",
  "14 to 17 years",
  "18 to 24 years",
  "Age 25 to 29 years",
  "Age 30 to 34 years",
  "Age 35 to 39 years",
  "Age 40 to 44 years",
  "Age 45 to 49 years",
  "Age 50 to 54 years",
  "Age 55 to 59 years",
  "Age 60 to 64 years",
  "Age 65 to 69 years",
  "Age 70 to 74 years",
  "Age 75 to 79 years",
  "Age 80 to 84 years",
  "Age 85 years and older"
)

# Prep total population counts from PEP

age_10s_clean <- age_10s |>
  filter(!DATE %in% c(2, 3), # Remove non-decennial 2010 counts
         AGEGROUP %in% age_vars) |> # Filter only selected age group variables
  mutate(counttype = # Add descriptions to count types
           case_when(
             DATE == 1 ~ "Census population",
             TRUE ~ "Population estimate")) |>
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
  mutate(agegroup = # Recode detailed age groups to fewer ranges
           case_when(
             AGEGROUP == "Age 0 to 4 years" ~ "Under 10",
             AGEGROUP == "Age 5 to 9 years" ~ "Under 10",
             AGEGROUP == "Age 10 to 14 years" ~ "10 to 17",
             AGEGROUP == "14 to 17 years" ~ "10 to 17",
             AGEGROUP == "18 to 24 years" ~ "18 to 24",
             AGEGROUP == "Age 25 to 29 years" ~ "25 to 29",
             AGEGROUP == "Age 30 to 34 years" ~ "30 to 34",
             AGEGROUP == "Age 35 to 39 years" ~ "35 to 44",
             AGEGROUP == "Age 40 to 44 years" ~ "35 to 44",
             AGEGROUP == "Age 45 to 49 years" ~ "45 to 54",
             AGEGROUP == "Age 50 to 54 years" ~ "45 to 54",
             AGEGROUP == "Age 55 to 59 years" ~ "55 to 64",
             AGEGROUP == "Age 60 to 64 years" ~ "55 to 64",
             AGEGROUP == "Age 65 to 69 years" ~ "65 to 74",
             AGEGROUP == "Age 70 to 74 years" ~ "65 to 74",
             AGEGROUP == "Age 75 to 79 years" ~ "75 and over",
             AGEGROUP == "Age 80 to 84 years" ~ "75 and over",
             AGEGROUP == "Age 85 years and older" ~ "75 and over"
           )) |> 
  select( # Simplify columns
    GEOID,
    year,
    counttype,
    agegroup,
    value
  )

age_20s_clean <- age_20s |>
         filter(AGEGROUP %in% age_vars) |> 
  mutate(agegroup = # Recode detailed age groups to fewer ranges
           case_when(
             AGEGROUP == "Age 0 to 4 years" ~ "Under 10",
             AGEGROUP == "Age 5 to 9 years" ~ "Under 10",
             AGEGROUP == "Age 10 to 14 years" ~ "10 to 17",
             AGEGROUP == "14 to 17 years" ~ "10 to 17",
             AGEGROUP == "18 to 24 years" ~ "18 to 24",
             AGEGROUP == "Age 25 to 29 years" ~ "25 to 29",
             AGEGROUP == "Age 30 to 34 years" ~ "30 to 34",
             AGEGROUP == "Age 35 to 39 years" ~ "35 to 44",
             AGEGROUP == "Age 40 to 44 years" ~ "35 to 44",
             AGEGROUP == "Age 45 to 49 years" ~ "45 to 54",
             AGEGROUP == "Age 50 to 54 years" ~ "45 to 54",
             AGEGROUP == "Age 55 to 59 years" ~ "55 to 64",
             AGEGROUP == "Age 60 to 64 years" ~ "55 to 64",
             AGEGROUP == "Age 65 to 69 years" ~ "65 to 74",
             AGEGROUP == "Age 70 to 74 years" ~ "65 to 74",
             AGEGROUP == "Age 75 to 79 years" ~ "75 and over",
             AGEGROUP == "Age 80 to 84 years" ~ "75 and over",
             AGEGROUP == "Age 85 years and older" ~ "75 and over"
           ),
         counttype = "Population estimate") |> 
  select( # Simplify columns
    GEOID,
    year,
    counttype,
    agegroup,
    value
  )

age_data <- rbind(age_10s_clean, age_20s_clean) 

age <- age_data |> 
  group_by(GEOID, year, counttype, agegroup) |>  # Collapse and sum recoded age groups
  summarise(value = sum(value)) 

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

age_join <- age |> 
  left_join(lookup, by = 'GEOID')
  
write_rds(age_join, "shiny/age/pop_age.rds")
