library(tidyverse)
library(tidycensus)
library(janitor)

# Download population counts by race and ethnicity from PEP for each Virginia locality from 2010 to 2019

race_ethnicity_raw <- get_estimates(
  geography = "county",
  state = "VA",
  product = "characteristics",
  breakdown = c("RACE", "HISP"),
  breakdown_labels = TRUE,
  year = 2019,
  time_series = TRUE
)


# Get race and ethnicity variables from the 2020 Census summary file

census_vars <- load_variables(2020, "pl")

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
)

# Download total population estimates from PEP for each Virginia locality from 2010 to 2019

race_ethnicity_clean <- race_ethnicity_raw %>%
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
    year,
    counttype,
    race = RACE,
    hisp = HISP,
    value
  )

# Create and add labels to race and ethnicity variables from 2020 Census summary file

census_labels_race <- census_vars %>% 
  filter(name %in% census_vars_race) %>%
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
    variable = name,
    race = col4,
    hisp = col3
  )

# Prep total population counts from 2020 Census summary file

census_clean <- census_raw %>%
  left_join(census_labels_race, # Add race and ethnicity labels
            by = "variable"
  ) %>%
  select(-c(variable, NAME)) %>% # Remove unnecessary columns
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
    year,
    counttype,
    race,
    hisp,
    value
  )

# Append 2020 Census data to PEP time series, remove unnecessary subtotals, and relabel variables

race_ethnicity_data <- race_ethnicity_clean %>% 
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
  ) |> 
  group_by(GEOID,
           year,
           counttype,
           race,
           hisp,
           label) |> 
  summarise(value = sum(value))

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

race_data <- race_ethnicity_data |> 
  left_join(lookup, by = 'GEOID')

# Write data to rds format in data folder and note the date below.

write_rds(race_data, "shiny/pop_race/race_data.rds")


# race <- read_rds("shiny/pop_race/race_data.rds")
# 
# cbsa <- race |> 
#   group_by(year, cbsa_title, label) |> 
#   summarise(value = sum(value)) 
# 
# cbsa <- cbsa |> filter(cbsa_title == "Big Stone Gap, VA") |> 
#       filter(year == "2020") |> 
#       mutate(percent = value/sum(value))


