# Load libraries
library(tidyverse)
library(tidycensus)

# ACS Table B11012: Households by Type

# Get variables for Table B11012

b11012_vars <- load_variables(2022, "acs5") %>% 
  filter(str_detect(name, "B11012"))

# Get B11012 data for every locality in Virginia

years <-c(2010,2022)

b11012_raw <- map_dfr(years, function(yr){
  b11012_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B11012",
  year = yr,
  survey = "acs5"
) |> 
    mutate(year = yr)
})

# Clean B11012 variable names

b11012_vars_clean <- b11012_vars %>% 
  separate(label, into = c("est", "tot", "type", "subtype"),
           sep = "!!") %>% 
  select(variable = name, type, subtype) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(type, subtype), ~ !is.na(.x))) %>% 
  mutate(type = case_when(
    str_detect(type, "couple") ~ "Married or cohabitating couple",
    str_detect(type, "present") ~ "Householder with no partner",
    TRUE ~ type)
  ) %>% 
  mutate(subtype = case_when(
    str_detect(subtype, "With own children") ~ "With own children",
    str_detect(subtype, "With no own") ~ "Without own children",
    subtype == "With relatives, no own children under 18 years" ~ "With relatives, no own children",
    subtype == "With only non relatives present" ~ "With only nonrelatives",
    TRUE ~ subtype)
  )

b11012_data <- b11012_raw %>% 
  right_join(b11012_vars_clean, by = "variable") %>% # Use right_join to leave out unnecessary subtotals from data
  select(GEOID, year, type, subtype, estimate, moe) %>% # Simplify columns
  group_by(GEOID, year, type, subtype) %>% # Collapse and sum subtypes
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

hhtype_join <- b11012_data |> 
  left_join(lookup, by = 'GEOID')

write_rds(hhtype_join, "data/hh_type.rds")
