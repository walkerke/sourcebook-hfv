library(tidycensus)
library(tidyverse)
library(knitr)
library(janitor)
library(tigris)
library(air)

source("config.R")

va <- counties(state = "VA") %>% 
  sf::st_drop_geometry()


## ---- Population Estimate ---- 

# Get data from the 2010s first.

pep_2010s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = "POP",
  year = 2019,
  time_series = TRUE
)

# Clean the data from the 2010s.

pep_2010s_clean <- pep_2010s %>%
  filter(!DATE %in% c(2, 3)) %>% # Remove non-decennial 2010 counts
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
             DATE == 12 ~ "2019")) %>% 
  mutate(counttype = # Add descriptions to count types
           case_when(
             DATE == 1 ~ "Census population",
             TRUE ~ "Population estimate")) %>% 
  select( # Simplify columns
    GEOID,
    year,
    counttype,
    value
  ) 

# Get data from the 2020s to-date. 

pep_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = "POPESTIMATE",
  vintage = latest_pep,
  time_series = TRUE
)

pep_2020s_clean <- pep_2020s |> 
  filter(year != 2020) |> 
  mutate(counttype = "Population estimate") |> 
  select( # Simplify columns
    GEOID,
    year,
    counttype,
    value
  ) 


# Get 2020 Decennial Census data.

census_raw <- get_decennial(
  geography = "county",
  state = "VA",
  year = 2020,
  sumfile = "pl",
  variables = "P1_001N"
)

census_clean <- census_raw %>% 
  mutate(year = "2020", # Add year and count type columns
         counttype = "Census population") %>%
  select( # Simplify columns
    GEOID,
    year,
    counttype,
    value
  ) 


#  Combine total population data.

pop_data <- rbind(pep_2010s_clean, pep_2020s_clean, census_clean) %>% 
  left_join(va, by = "GEOID")


write_rds(pop_data, "data/pop_data.rds")

## ---- Components of Population Change -----

change_2010s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = c("NATURALINC", "DOMESTICMIG", "INTERNATIONALMIG"),
  year = 2019,
  time_series = TRUE
)

change_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  variables = c("NATURALCHG", "DOMESTICMIG", "INTERNATIONALMIG"),
  vintage = latest_pep,
  time_series = TRUE
)

change_2010s_clean <- change_2010s %>% 
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
             PERIOD == 10 ~ "2019")) %>%
  mutate(component = # Rename components of change
           case_when(
             variable == "NATURALINC" ~ "Natural increase",
             variable == "DOMESTICMIG" ~ "Domestic migration",
             variable == "INTERNATIONALMIG" ~ "International migration")) %>% 
  select( # Simplify columns
    GEOID,
    year,
    component,
    value
  )

change_2020s_clean <- change_2020s %>%
  mutate(component = # Rename components of change
           case_when(
             variable == "NATURALCHG" ~ "Natural increase",
             variable == "DOMESTICMIG" ~ "Domestic migration",
             variable == "INTERNATIONALMIG" ~ "International migration")) %>% 
  select( # Simplify columns
    GEOID,
    year,
    component,
    value
  ) 

pep_change <- rbind(change_2010s_clean, change_2020s_clean) %>% 
  left_join(va, by = "GEOID")

write_rds(pep_change, "data/pop_change.rds")

## ---- Race and Ethnicity Estimates ----

# Download population counts by race and ethnicity from PEP for each Virginia locality from 2010 to 2019

race_2010s <- get_estimates(
  geography = "county",
  state = "VA",
  product = "characteristics",
  breakdown = c("RACE", "HISP"),
  breakdown_labels = TRUE,
  year = 2019,
  time_series = TRUE
) %>%
  filter(!DATE %in% c(2, 3)) %>% # Remove non-decennial 2010 counts
  filter(!str_detect(RACE, "combination")) %>% # Remove counts for "in combination" race categories for simplicity
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
             DATE == 12 ~ "2019")) %>% 
  mutate(counttype = # Add descriptions to count types
           case_when(
             DATE == 1 ~ "Census population",
             TRUE ~ "Population estimate")) %>% 
  select( # Simplify columns
    GEOID,
    NAME,
    year,
    counttype,
    race = RACE,
    hisp = HISP,
    value
  )

race_2020s <- get_estimates(
  geography = "county",
  state = "VA",
  product = "characteristics",
  breakdown = c("RACE", "HISP"),
  breakdown_labels = TRUE,
  vintage = lag_pep,
  time_series = TRUE
) %>%
  filter(!str_detect(RACE, "combination")) %>% # Remove counts for "in combination" race categories for simplicity
  mutate(counttype = "Population estimate") |> 
  select( # Simplify columns
    GEOID,
    NAME,
    year,
    counttype,
    race = RACE,
    hisp = HISP,
    value
  )

# Get race and ethnicity variables from the 2020 Census summary file

census_vars <- load_variables(2020, "pl") |> 
  mutate(variable = name)

census_vars_race <- c(
  "P1_001N", # All races
  "P1_003N", # White alone
  "P1_004N", # Black or African American alone
  "P1_005N", # American Indian and Alaska Native alone
  "P1_006N", # Asian alone
  "P1_007N", # Native Hawaiian and Other Pacific Islander alone
  "P1_008N", # Some Other Race alone
  "P1_009N", # Two or more races
  "P2_002N", # Total Hispanic or Latino
  "P2_005N", # Hispanic or Latino, White alone
  "P2_006N", # Hispanic or Latino, Black or African American alone
  "P2_007N", # Hispanic or Latino, American Indian and Alaska Native alone
  "P2_008N", # Hispanic or Latino, Asian alone
  "P2_009N", # Hispanic or Latino, Native Hawaiian and Other Pacific Islander alone
  "P2_010N", # Hispanic or Latino, Some Other Race alone
  "P2_011N", # Hispanic or Latino, Two or more races
  "P2_003N"  # Total Not Hispanic or Latino
)

# Download population counts by race and ethnicity from 2020 Census summary file for each Virginia locality

census_raw <- get_decennial(
  geography = "county",
  state = "VA",
  year = 2020,
  sumfile = "pl",
  variables = census_vars_race
) %>% 
  filter(variable %in% census_vars_race) |> 
  left_join(census_vars, by = "variable")%>%
  separate(label, into = c("col1", "col2", "col3", "col4", "col5"), sep = "!!") %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":")),
         across(.fns = ~replace_na(.x, "All"))) %>% 
  mutate(
    col4 = case_when(
      col5 == "White alone" ~ col5,
      col5 == "Black or African American alone" ~ col5,
      col5 == "American Indian and Alaska Native alone" ~ col5,
      col5 == "Asian alone" ~ col5,
      col5 == "Native Hawaiian and Other Pacific Islander alone" ~ col5,
      col5 == "Some Other Race alone" ~ col5,
      col3 == "Population of two or more races" ~ "Population of two or more races",
      col4 == "All" ~ "All races",
      TRUE ~ col4),
    col3 = case_when(
      col3 == "Population of one race" ~ "Both Hispanic Origins",
      col3 == "Population of two or more races" ~ "Both Hispanic Origins",
      col3 == "All" ~ "Both Hispanic Origins",
      col3 == "Hispanic or Latino" ~ "Hispanic",
      col3 == "Not Hispanic or Latino" ~ "Non-Hispanic",
      TRUE ~ col3),
    across(.fns = ~str_replace_all(.x, "Population of two or more races", "Two or more races")),
    across(.fns = ~str_replace_all(.x, "Black or African American alone", "Black alone"))
  ) %>% 
  select(
    GEOID,
    NAME,
    variable = name,
    race = col4,
    hisp = col3, 
    value
  )

race_ethnicity_raw <- rbind(race_2010s, race_2020s) |> 
  filter(year != 2020)


# Data prep


# Prep total population counts from PEP

# race_ethnicity_clean <- race_ethnicity_raw %>%
#   filter(!DATE %in% c(2, 3)) %>% # Remove non-decennial 2010 counts
#   filter(!str_detect(RACE, "combination")) %>% # Remove counts for "in combination" race categories for simplicity
#   mutate(year = # Translate date codes into years
#     case_when(
#       DATE == 1 ~ "2010",
#       DATE == 4 ~ "2011",
#       DATE == 5 ~ "2012",
#       DATE == 6 ~ "2013",
#       DATE == 7 ~ "2014",
#       DATE == 8 ~ "2015",
#       DATE == 9 ~ "2016",
#       DATE == 10 ~ "2017",
#       DATE == 11 ~ "2018",
#       DATE == 12 ~ "2019")) %>% 
#   mutate(counttype = # Add descriptions to count types
#       case_when(
#         DATE == 1 ~ "Census population",
#         TRUE ~ "Population estimate")) %>% 
#   select( # Simplify columns
#     GEOID,
#     year,
#     counttype,
#     race = RACE,
#     hisp = HISP,
#     value
#   )

# Create and add labels to race and ethnicity variables from 2020 Census summary file

# census_labels_race <- census_vars %>% 
#   filter(name %in% census_vars_race) %>%
#   separate(label, into = c("col1", "col2", "col3", "col4", "col5"), sep = "!!") %>% 
#   mutate(across(.fns = ~str_remove_all(.x, ":")),
#          across(.fns = ~replace_na(.x, "All"))) %>% 
#   mutate(
#     col4 = case_when(
#       col5 == "White alone" ~ col5,
#       col5 == "Black or African American alone" ~ col5,
#       col5 == "American Indian and Alaska Native alone" ~ col5,
#       col5 == "Asian alone" ~ col5,
#       col5 == "Native Hawaiian and Other Pacific Islander alone" ~ col5,
#       col5 == "Some Other Race alone" ~ col5,
#       col3 == "Population of two or more races" ~ "Population of two or more races",
#       col4 == "All" ~ "All races",
#       TRUE ~ col4),
#     col3 = case_when(
#       col3 == "Population of one race" ~ "Both Hispanic Origins",
#       col3 == "Population of two or more races" ~ "Both Hispanic Origins",
#       col3 == "All" ~ "Both Hispanic Origins",
#       col3 == "Hispanic or Latino" ~ "Hispanic",
#       col3 == "Not Hispanic or Latino" ~ "Non-Hispanic",
#       TRUE ~ col3),
#     across(.fns = ~str_replace_all(.x, "Population of two or more races", "Two or more races")),
#     across(.fns = ~str_replace_all(.x, "Black or African American alone", "Black alone"))
# )

# Prep total population counts from 2020 Census summary file

census_clean <- census_raw %>%
  select(-c(variable)) %>% # Remove unnecessary columns
  mutate(value = as.numeric(value)) |> 
  pivot_wider( # Pivot out values by Hispanic status
    names_from = "hisp",
    values_from = "value"
  ) %>% 
  mutate( # Calculate Hispanic values for all races (not included in Census data)
    `Hispanic` = `Both Hispanic Origins` - `Non-Hispanic`
  ) %>% 
  pivot_longer( # Transform data back into tidy format
    cols = contains("Hispanic"),
    names_to = "hisp",
    values_to = "value"
  ) %>%
  mutate(year = "2020", # Add year and count type columns to match PEP data
         counttype = "Census population") %>%
  select( # Simplify columns
    GEOID,
    NAME,
    year,
    counttype,
    race,
    hisp,
    value
  )

# Append 2020 Census data to PEP time series, remove unnecessary subtotals, and relabel variables

race_ethnicity_data <- race_ethnicity_raw %>% 
  bind_rows(census_clean) %>% 
  mutate(race = str_remove_all(race, " alone")) %>% # Remove "alone" from race column values
  filter( # Remove unnecessary subtotals to avoid double-counted data
    !hisp == "Both Hispanic Origins",
    hisp == "Non-Hispanic" | race == "All races") %>%
  mutate(label = # Create "label" column that collapses less prevalent race categories
           case_when(
             race == "All races" & hisp == "Hispanic" ~ "Hispanic or Latino",
             race == "White" ~ "White, non-Hispanic",
             race == "American Indian and Alaska Native" ~ "Another race",
             race == "Native Hawaiian and Other Pacific Islander" ~ "Another race",
             race == "Two or more races" ~ "Multiracial",
             race == "Some Other Race" ~ "Another race",
             TRUE ~ race
           )) %>% 
  filter(!label == "All races") %>% # Remove unnecessary "All races, Non-Hispanic" subtotal
  select( # Reorder columns
    GEOID,
    year,
    counttype,
    race,
    hisp,
    label,
    value
  )

write_rds(race_ethnicity_data, "data/race-ethnicity.rds")

## ---- Population by Age ----

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
  year = lag_pep,
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

write_rds(age_join, "data/pop_age.rds")




