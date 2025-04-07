# Load libraries
library(tidyverse)
library(tidycensus)

# Table B09021: Living Arrangements of Adults 18 Years and Over by Age

# Get variables for Table B09021

b09021_vars <- load_variables(2022, "acs5") %>% 
  filter(str_detect(name, "B09021"))

# Get B09021 data for every locality in Virginia

years <-2015:2022

b09021_raw <- map_dfr(years, function(yr){
  b09021_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B09021",
    year = yr,
    survey = "acs5"
  ) |> 
    mutate(year = yr)
})

# Clean B09021 variable names

b09021_vars_clean <- b09021_vars %>% 
  separate(label, into = c("est", "tot", "age", "type"),
           sep = "!!") %>% 
  select(variable = name, age, type) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":")),
         age = str_remove_all(age, " years")) %>%
  mutate(type = case_when(
    variable %in% c(paste0("B09021_00", 2:7)) ~ age,
    TRUE ~ type)) %>%
  mutate(age = case_when(
    variable %in% c(paste0("B09021_00", 2:7)) ~ "All ages",
    TRUE ~ age)) %>%
  filter(across(c(age, type), ~ !is.na(.x))) %>% 
  mutate(type = case_when(
    str_detect(type, "Householder living") ~ "Lives with married or unmarried partner",
    type == "Child of householder" ~ "Lives with parent(s)",
    type == "Other relatives" ~ "Lives with other relative(s)",
    type == "Other nonrelatives" ~ "Lives with other nonrelative(s)",
    TRUE ~ type)
  )

b09021_data <- b09021_raw %>% 
  right_join(b09021_vars_clean, by = "variable") %>% # Use right_join to leave out unnecessary subtotals from data
  select(GEOID, year, age, type, estimate, moe) %>% # Simplify columns
  group_by(GEOID, year, age, type) %>% # Collapse and sum subtypes
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate) # Calculate new group-wise margins of error 
  ) %>% 
  ungroup() %>% 
  mutate(cv = ((moe/1.645)/estimate)*100) %>% # Calculate coefficient of variation
  mutate(reliability = case_when(
    cv < 15 ~ "High reliability",
    cv >= 15 & cv <= 30 ~ "Medium reliability",
    cv > 30 ~ "Low reliability")
  )

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

lvng_join <- b09021_data |> 
  left_join(lookup, by = 'GEOID')

write_rds(lvng_join, "data/lvng_arr.rds")
