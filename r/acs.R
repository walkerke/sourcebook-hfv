library(tidycensus)
library(tidyverse)
library(fredr)
library(lubridate)
source("config.R")


# ---- Table B25009: Tenure by Household Size ----

# Get B25009 data for every locality in Virginia

b25009_raw <- map_dfr(years, function(yr) {
  b25009_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr) |> 
    mutate(year = yr)
})

# Clean B25009 variable names

b25009_vars_clean <- b25009_vars %>% 
  separate(label, into = c("est", "tot", "tenure", "hhsize"),
           sep = "!!") %>% 
  select(variable = name, tenure, hhsize) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":")),
         tenure = str_remove_all(tenure, " occupied"),
         hhsize = str_remove_all(hhsize, " household")) %>%
  filter(across(c(tenure, hhsize), ~ !is.na(.x))) %>% 
  mutate(hhsize = case_when(
    str_detect(hhsize, "[4-7]") ~ "4-or-more person",
    TRUE ~ hhsize)
  )

# Join B25009 variables to data, calculate new sums, and add data reliability info

b25009_data <- b25009_raw %>%
  mutate(GEOID = str_replace_all(GEOID, "51515", "51019")) %>% # Merge 'Bedford city' values from 2010 to 'Bedford county'
  right_join(b25009_vars_clean, by = "variable") %>% # Use right_join to leave out unnecessary subtotals from data
  select(GEOID, year, tenure, hhsize, estimate, moe) %>% # Simplify columns
  group_by(GEOID, year, tenure, hhsize) %>% # Collapse and sum hhsizes
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate) # Calculate new group-wise margins of error 
  ) %>% 
  ungroup() %>% 
  mutate(cv = ((moe/1.645)/estimate)*100) %>% # Calculate coefficient of variation
  mutate(reliability = case_when(
    cv < 15 ~ "High",
    cv >= 15 & cv <= 30 ~ "Medium",
    cv > 30 ~ "Low")
  )

lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

hhsize_join <- b25009_data |> 
  left_join(lookup, by = 'GEOID')

write_rds(hhsize_join, "data/hh_size.rds")


# ---- Table B25010: Average Household Size of Occupied Housing Units by Tenure ----

# Average household size data from B25010 cannot be aggregated to CBSA or state level
# Must pull B25010 for each geographic level separately
# Create list of CBSA codes for all that are in or partly in Virginia

cbsa <- c("13720", "13980", "14140", "16820", "19260", "25500", "28700", "31340", "32300", "40060", "40220", "44420", "47260", "47900", "49020")

# Get variables for Table B25010

b25010_vars <- load_variables(current_acs, "acs5") %>% 
  filter(str_detect(name, "B25010"))

# Get B25010 data and add 'geography' column

b25010_raw <- map_dfr(years, function(yr) {
  
  # Pull data for state
  
  b25010_state <- get_acs(
    geography = "state",
    state = "VA",
    table = "B25010",
    year = yr) %>% 
    mutate(year = yr,
           geography = "state")
  
  # Pull data for CBSAs
  
  b25010_cbsa <- get_acs(
    geography = "cbsa",
    table = "B25010",
    year = yr) %>% 
    mutate(year = yr,
           geography = "cbsa") %>% 
    filter(GEOID %in% cbsa)
  
  # Pull data for localities
  
  b25010_locality <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25010",
    year = yr) %>% 
    mutate(year = yr,
           geography = "locality")
  
  # Bind dataframes together
  
  b25010_all <- bind_rows(b25010_state, b25010_cbsa, b25010_locality)
  
})

# Clean B25010 variable names

b25010_vars_clean <- b25010_vars %>% 
  separate(label, into = c("est", "avg", "tot", "tenure"),
           sep = "!!") %>% 
  select(variable = name, tenure) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":")),
         across(.fns = ~replace_na(.x, "All")),
         tenure = str_remove_all(tenure, " occupied"))

# Join B25010 variables to data, calculate new sums, and add data reliability info

b25010_data <- b25010_raw %>%
  filter(!GEOID == "51515") %>% # Filter out 2010 'Bedford city' values since 2019 values do not exist
  right_join(b25010_vars_clean, by = "variable") %>% # Join variables to data
  select(GEOID, geography, year, tenure, estimate, moe) %>% # Simplify columns
  mutate(cv = ((moe/1.645)/estimate)*100) %>% # Calculate coefficient of variation
  mutate(reliability = case_when(
    cv < 15 ~ "High",
    cv >= 15 & cv <= 30 ~ "Medium",
    cv > 30 ~ "Low")
  )

write_rds(b25010_data, "data/avg_hh_size.rds")

# Load libraries
library(tidyverse)
library(tidycensus)

# ---- Table B11012: Households by Type -----

# Get variables for Table B11012

b11012_vars <- load_variables(current_acs, "acs5") %>% 
  filter(str_detect(name, "B11012"))

# Get B11012 data for every locality in Virginia

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


# ---- Table B09021: Living Arrangements of Adults 18 Years and Over by Age ----

# Get variables for Table B09021

b09021_vars <- load_variables(current_acs, "acs5") %>% 
  filter(str_detect(name, "B09021"))

# Get B09021 data for every locality in Virginia



b09021_raw <- map_dfr(years_abrev, function(yr){
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

## ---- Consumer Price Index for income inflation ---- 

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

current_index <- cpi |> 
  filter(year == current_acs) |> 
  pull(index)

# Create a function to convert median household income from ACS to most recent 
# inflation-adjusted dollar value.

adjustment <- function(x) {
  transform(x, adjusted = ((current_index/index)*estimate))
}

## ---- Table B19049: Median Household Income by Age of Householder ----

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

write_rds(state_adj, "data/b19049_state.rds")
write_rds(cbsa_adj, "data/b19049_cbsa.rds")
write_rds(locality_adj, "data/b19049_locality.rds")

## ---- Table B25118 - Household Income Distribution by Tenure ---- 


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

write_rds(b25118_data, "data/b25118_data.rds")

## ---- Table B25119: Median Household Income by Tenure ---- 


b25119_vars <- load_variables(current_acs, "acs5") |> 
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

## ---- Table B17001: Poverty Status by Race and Age ----

# Create an object for all tables needed.

b17001 <- paste0("B17001", LETTERS[2:9]) # Need to append letters B-I

# Create a function to create a a race and ethnicity category.

concept_to_race <- function(x) {
  out <- x %>%
    str_remove_all("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(|\\)") %>%
    str_to_title()
}

# Get variables for Table B17001.

b17001_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 7) %in% b17001) %>%
  filter(str_detect(name, "PR") == FALSE)

b17001_cleaned <- b17001_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, c ("estimate", "total", "poverty", "sex", "age"), sep = "!!") %>%
  select(variable = name, race, poverty, sex, age) %>%
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

output_b17001 <- map_dfr(b17001, function(tb){
  yearly_data <- map_dfr(years, function(yr){
    
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) %>%
      left_join(b17001_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, race, sex, poverty, age,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})

output_b17001_clean <- output_b17001 %>%
  mutate(across(.fns = ~str_remove_all(.x, ", Virginia"))) |> 
  mutate(race = case_when(
    race == "Black Or African American Alone" ~ "Black",
    race == "American Indian And Alaska Native Alone" ~ "Some Other Race",
    race == "Asian Alone" ~ "Asian",
    race == "Native Hawaiian And Other Pacific Islander Alone" ~ "Some Other Race",
    race == "Some Other Race Alone" ~ "Some Other Race",
    race == "Two Or More Races" ~ "Multiracial",
    race == "White Alone, Not Hispanic Or Latino" ~ "White, Non-Hispanic",
    race == "Hispanic Or Latino" ~ "Hispanic or Latino",
    TRUE ~ race)) |> 
  mutate(age = case_when(
    age == "Under 5 years" ~ "17 years and under", 
    age == "5 years" ~ "17 years and under",
    age == "6 to 11 years" ~ "17 years and under", 
    age == "12 to 14 years" ~ "17 years and under",
    age == "15 years" ~ "17 years and under",
    age == "16 and 17 years" ~ "17 years and under",
    age == "18 to 24 years" ~ "18 to 24 years",
    age == "65 to 74 years" ~ "65 years and over",
    age == "75 years and over" ~ "65 years and over",
    TRUE ~ age
  )) |> 
  mutate(estimate = as.numeric(estimate)) |> 
  filter(poverty != "All") |> 
  filter(sex != "All") |> 
  filter(age != "All") |> 
  group_by(year, locality, fips, race) |> 
  mutate(totalrace = sum(estimate)) |> 
  group_by(year, locality, fips, age) |> 
  mutate(totalage = sum(estimate)) 


rate_race <- output_b17001_clean |> 
  filter(poverty == "Income in the past 12 months below poverty level") |> 
  group_by(year, locality, fips, race, totalrace) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(rate = estimate/totalrace)


rate_age <- output_b17001_clean  |> 
  filter(poverty == "Income in the past 12 months below poverty level") |> 
  group_by(year, locality, fips, age, totalage) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(rate = estimate/totalage)


# Write all data to rds.
write_rds(rate_race, "data/poverty_race.rds")
write_rds(rate_age, "data/poverty_age.rds")


# ---- Table B25003: Households by Tenure ---- 

# Create an object for all tables needed. This includes a table that aggregates 
# to total housing units.
b25003 <- c("B25003", paste0("B25003", LETTERS[2:9]))

b25007 <- "B25007"

# Create a function to convert variable to race or ethnicity variable.
concept_to_race <- function(x) {
  out <- x %>%
    str_remove_all("HOUSEHOLDER") %>%
    str_remove_all("TENURE \\(|\\)") %>%
    str_to_title() %>%
    str_replace_all("And", "and") %>%
    str_replace_all("Or", "or") %>%
    str_remove_all(" Alone")
  
  out
}

# Pull the table variables, excluding Puerto Rico.
b25003_defns <- load_variables(current_acs, "acs5") %>%
  filter(str_sub(name, end = 6) %in% b25003) %>% 
  filter(str_detect(name, "PR") == FALSE)

b25007_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) %in% b25007)%>%
  filter(str_detect(name, "PR")== FALSE)

# Clean the variables provided by Census API and separate needed variables into
# appropriate columns.
b25003_cleaned <- b25003_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, into = c("est", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, race, tenure) %>% 
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")),
         across(.fns = ~str_remove_all(.x, " --")),
         across(.fns = ~str_replace_all(.x, "total", "All")),
         across(.fns = ~str_replace_all(.x, "Tenure", "All")))

b25007_cleaned <- b25007_defns %>%
  separate(label, into = c("est", "tot", "tenure", "age"), sep = "!!") %>%
  select(variable = name, tenure, age)%>%
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

# Download the data for all counties in Virginia. Functions interate across all
# tables and all years. If desiring to change to state then input "state" instead
# of "county" and remove 'state = "VA" from function.
output_b25003 <- map_dfr(b25003, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) %>%
      left_join(b25003_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, race, tenure,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})

output_b25003_state <- map_dfr(b25003, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "state",
      table = tb,
      year = yr
    ) %>%
      left_join(b25003_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, race, tenure,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})

output_b25007 <- map_dfr(b25007, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) %>%
      left_join(b25007_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, tenure, age,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})


# Data Prep
# The previous data output makes it difficult to easily calculate dynamic 
# homeownership rates if wanting to combine multiple localities or races. In
# order to create these calculations more easily, unpivot the columns to rows
# utilizing the pivot_wider function.

output_b25003_wide <- output_b25003 %>% 
  select( -c(variable)) %>% 
  pivot_wider(names_from = tenure,
              values_from = c(estimate, moe)) %>% 
  select(year:race, est_all = estimate_All, est_owner = "estimate_Owner occupied",
         est_renter = "estimate_Renter occupied", moe_all = moe_All, 
         moe_owner = "moe_Owner occupied",
         moe_renter = "moe_Renter occupied")

output_b25003_wide_state <- output_b25003_state %>% 
  select( -c(variable)) %>% 
  pivot_wider(names_from = tenure,
              values_from = c(estimate, moe)) %>% 
  select(year:race, est_all = estimate_All, est_owner = "estimate_Owner occupied",
         est_renter = "estimate_Renter occupied", moe_all = moe_All, 
         moe_owner = "moe_Owner occupied",
         moe_renter = "moe_Renter occupied")

# Clean the locality column to remove comma and Virginia from data, as well as
# converting Bedford city (51515) to Bedford County (51019).


lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(fips = fips_full, name_long, cbsa_title) # Simplify data

output_b25003_wide_clean <- output_b25003_wide %>%
  mutate(across(.fns = ~str_remove_all(.x, ", Virginia")),
         across(.fns = ~str_replace_all(.x, "Bedford city", "Bedford County")),
         across(.fns = ~str_replace_all(.x, "51515", "51019"))) |> 
  left_join(lookup, by = "fips") |> 
  mutate(est_all = as.numeric(est_all)) |> 
  mutate(est_owner = as.numeric(est_owner)) |> 
  select(year, locality, cbsa = cbsa_title, race, est_owner, est_all)

output_b25007_clean <- output_b25007 %>%
  mutate(across(.fns = ~str_remove_all(.x, ", Virginia")),
         across(.fns = ~str_replace_all(.x, "Bedford city", "Bedford County")),
         across(.fns = ~str_replace_all(.x, "51515", "51019"))) |> 
  filter(tenure != "All") |> 
  filter(age != "All") |> 
  mutate(estimate = as.numeric(estimate)) |> 
  mutate(age = case_when(
    age == "Householder 15 to 24 years" ~ "24 years and younger",
    age == "Householder 25 to 34 years" ~ "25 to 34 years",
    age == "Householder 35 to 44 years" ~ "35 to 44 years",
    age == "Householder 45 to 54 years" ~ "45 to 54 years",
    age == "Householder 55 to 59 years" ~ "55 to 64 years",
    age == "Householder 60 to 64 years" ~ "55 to 64 years",
    TRUE ~ "65 years and over")) |> 
  group_by(year, locality, fips, tenure, age) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  left_join(lookup, by = "fips")

output_b25007_wide <- output_b25007 %>%
  select( -c(variable)) %>% 
  pivot_wider(names_from = tenure,
              values_from = c(estimate, moe)) %>% 
  select(year:age, est_all = estimate_All, est_owner = "estimate_Owner occupied",
         est_renter = "estimate_Renter occupied", moe_all = moe_All, 
         moe_owner = "moe_Owner occupied",
         moe_renter = "moe_Renter occupied") |> 
  select(year, locality, )


# Data export

# Write to rds.
write_rds(output_b25003_wide, "data/b25003_wide.rds")
write_rds(output_b25003_wide_state, "data/b25003_state.rds")

# ---- Table B25063: Gross Rent and Table B25064: Median Gross Rent ----

# Create object for tables.
b25063 <- "B25063" # Gross Rent
b25064 <- "B25064" # Median Gross Rent

# Get variables from tables.

b25063_defns_2021 <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) == b25063) %>%
  filter(str_detect(name, "PR") == FALSE)

b25063_defns_2014 <- load_variables(2014, "acs5") %>%
  filter(str_sub(name, end = 6) == b25063) %>%
  filter(str_detect(name, "PR") == FALSE)

b25064_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) == b25064) %>%
  filter(str_detect(name, "PR") == FALSE)

# Clean variables and select only needed columns.

b25063_cleaned_2014 <- b25063_defns_2014 %>%
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") %>%
  select(variable = name, cash, rent) %>%
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

b25063_cleaned_2022 <- b25063_defns_2021 %>%
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") %>%
  select(variable = name, cash, rent) %>%
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

b25064_cleaned <- b25064_defns %>%
  separate(label, into = c("est", "medgrossrent"), sep = "!!") %>%
  select(variable = name, medgrossrent) %>%
  mutate(across(.fns = ~replace_na(.x, "All")))

# Pull down data from Census API for all localities in Virginia. Note that you 
# cannot aggregate medians and will need to pull down a state value and CBSA
# values. 

# Use "metropolitan statistical area/micropolitan statistical area" for geography
# value when needing CBSA and "state" when needing state value (when using state
# remove the state = "VA" line)

output_b25063_2014 <- map_dfr(years_partial_1, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    table = b25063,
    year = yr,
    state = "VA"
  ) %>%
    left_join(b25063_cleaned_2014, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, locality = NAME, fips = GEOID, cash, rent, 
           estimate, moe)
  
  acs_rearranged
})

output_b25063_2022 <- map_dfr(years_partial_2, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    table = b25063,
    year = yr,
    state = "VA"
  ) %>%
    left_join(b25063_cleaned_2022, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, locality = NAME, fips = GEOID, cash, rent, 
           estimate, moe)
  
  acs_rearranged
})

output_b25064_locality <- map_dfr(years, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    table = b25064,
    year = yr,
    state = "VA"
  ) %>%
    left_join(b25064_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, locality = NAME, fips = GEOID, medgrossrent, 
           estimate, moe)
  
  acs_rearranged
})

output_b25064_cbsa <- map_dfr(years_full, function(yr){
  acs_pull <- get_acs(
    geography = "cbsa",
    table = b25064,
    year = yr
  ) %>%
    left_join(b25064_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, cbsa = NAME, fips = GEOID, medgrossrent, 
           estimate, moe) %>%
    filter(str_detect(cbsa, "VA"))
  
  acs_rearranged
})

output_b25064_state <- map_dfr(years_full, function(yr){
  acs_pull <- get_acs(
    geography = "state",
    table = b25064,
    year = yr
  ) %>%
    left_join(b25064_cleaned, by = "variable")
  
  acs_rearranged <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, state = NAME, fips = GEOID, medgrossrent, 
           estimate, moe)
  
  acs_rearranged
})

output_b25063 <- rbind(output_b25063_2014, output_b25063_2021)

output_b25063_clean <- output_b25063 |> 
  filter(cash != "All") |> 
  mutate(rent = case_when(
    cash == "No cash rent" ~ "No cash rent",
    rent == "Less than $100" ~ "Less than $500", 
    rent == "$100 to $149" ~ "Less than $500",  
    rent == "$150 to $199" ~ "Less than $500",  
    rent == "$200 to $249" ~ "Less than $500", 
    rent == "$250 to $299" ~ "Less than $500", 
    rent == "$300 to $349" ~ "Less than $500", 
    rent == "$350 to $399" ~ "Less than $500", 
    rent == "$400 to $449" ~ "Less than $500",  
    rent == "$450 to $499" ~ "Less than $500", 
    rent == "$500 to $549" ~ "$500 to $749", 
    rent == "$550 to $599" ~ "$500 to $749", 
    rent == "$600 to $649" ~ "$500 to $749", 
    rent == "$650 to $699" ~ "$500 to $749", 
    rent == "$700 to $749" ~ "$500 to $749", 
    rent == "$750 to $799" ~ "$750 to $999", 
    rent == "$800 to $849" ~ "$750 to $999",
    rent == "$850 to $899" ~ "$750 to $999", 
    rent == "$900 to $949" ~ "$750 to $999", 
    rent == "$950 to $999" ~ "$750 to $999",
    rent == "$1,000 to $1,249" ~ "$1,000 to $1,249", 
    rent == "$1,250 to $1,499" ~ "$1,250 to $1,499", 
    rent == "$1,500 to $1,999" ~ "$1,500 to $1,999",
    TRUE ~ "$2,000 or more"
  ))

# Data prep

#  Consumer Price Index for All Urban Consumers: Rent of Primary Residence 
# in U.S. City Average (CUSR0000SEHA)

cpi <- fredr(
  series_id = "CUSR0000SEHA" # ID for CPI for All Urban Consumers: Rent of Primary Residence
) |> 
  select(date, value) |> # Select date and CPI
  mutate(date = as.Date(date), # Convert date to date data type.
         value = as.numeric(value), # Convert CPI to a numeric value.
         year = year(date)) |> # Create a field for the year and extract year from date.
  group_by(year) |> # Group by year. 
  summarise(index = mean(value)) # Calculate annual average CPI. 

current_index <- cpi |> 
  filter(year == current_acs) |> 
  pull(index)

# Create a function to convert median household income from ACS to most recent 
# inflation-adjusted dollar value.

adjustment <- function(x) {
  transform(x, adjusted = ((current_index/index)*estimate))
}


# Load the raw csv data file of CPI values by month for CPI for all items less shelter.
# cpi <- read_csv("raw\\CUUR0000SA0L2.csv")

# Create a column based on the year of CPI value.
# cpi$year <- floor_date(cpi$DATE, "year")

# Rename columns and ensure that CPI value is a numeric value.
# cpi <- cpi %>%
#   rename(cpi_value = CUUR0000SA0L2) %>%
#   transform(cpi_value = as.numeric(cpi_value))

# Summarize CPI by year to get average annual CPI. 
# annual_cpi <- cpi %>%
#   group_by(year) %>%
#   summarize(mean = mean(cpi_value))%>%
#   mutate(year = as.integer(substring(year, 1,4)))

# Join B25064 tables to the Annual CPI table based on year and then create a 
# column for 2020 dollar value (dollars20) that utilizes the formula above.

# The average annual CPI for 2022 is 370.2016

b25064_locality <- output_b25064_locality %>%
  left_join(cpi_rent, by = 'year') %>%
  rename(cpi = index) %>%
  mutate(adjusted = adjustment(estimate))

b25064_cbsa <- output_b25064_cbsa %>%
  left_join(cpi_rent, by = 'year') %>%
  rename(cpi = index) %>%
  mutate(adjusted = adjustment(estimate))

b25064_state <- output_b25064_state %>%
  left_join(cpi_rent, by = 'year') %>%
  rename(cpi = index) %>%
  mutate(adjusted = adjustment(estimate))


# Data export

# Write to csv files.

write_rds(b25064_locality, "data/b25064_locality.rds")
write_rds(b25064_cbsa, "data/b25064_cbsa.rds")
write_rds(b25064_state, "data/b25064_state.rds")
write_rds(output_b25063, "data/b25063.rds")

# ---- Rental Vacancy -----

# Get variables for Table B25003

b25003_vars <- load_variables(current_acs, "acs5") %>%
  filter(str_detect(name, "B25003"))

# Get B25003 data for every locality in Virginia

b25003_raw <- map_dfr(years, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    table = "B25003",
    year = yr,
    state = "VA"
  )
  
  acs_cleaned <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, NAME, GEOID,
           estimate, moe)
  
  acs_cleaned
})

# Get variables for Table B25004

b25004_vars <- load_variables(current_acs, "acs5") %>%
  filter(str_detect(name, "B25004"))

# Get B25004 data for every locality in Virginia

b25004_raw <- map_dfr(years, function(yr){
  acs_pull <- get_acs(
    geography = "county",
    table = "B25004",
    year = yr,
    state = "VA"
  )
  
  acs_cleaned <- acs_pull %>%
    mutate(year = yr) %>%
    select(variable, year, NAME, GEOID,
           estimate, moe)
  
  acs_cleaned
})

# Clean B25003 variable names

b25003_vars_clean <- b25003_vars %>%
  separate(label, into = c("est", "tot", "tenure"),
           sep = "!!") %>%
  select(variable = name, tenure)%>%
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(tenure), ~!is.na(.x))) %>%
  filter(str_detect(tenure, "Owner occupied") == FALSE) %>% # Keep  only renter housing unit estimates
  mutate(tenure = case_when(
    str_detect(tenure, "Renter occupied") ~ "Renter",
    TRUE ~ tenure)
  )

# Join B25003 variables to data, calculate new sums, and add data reliability info

b25003_data <- b25003_raw %>%
  right_join(b25003_vars_clean, by = "variable") %>% # Use right_join to leave out unnecessary subtotals from data
  select(GEOID, year, tenure, estimate, moe) %>% # Simplify columns
  group_by(GEOID, year, tenure) %>% # Collapse and summarize
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate) # Calculate new group-wise margins of error
  ) %>%
  ungroup()%>%
  mutate(cv = ((moe/1.645)/estimate)*100) %>% # Calculate coefficient of variation
  mutate(reliability = case_when(
    cv < 15 ~ "High reliability",
    cv >= 15 & cv <= 30 ~ "Medium reliability",
    cv > 30 ~ "Low reliability")
  )

# Clean B25004 variable names

b25004_vars_clean <- b25004_vars %>%
  separate(label, into = c("est", "total", "status"),
           sep = "!!") %>%
  select(variable = name, status) %>%
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(status), ~!is.na(.x))) %>%
  filter(grepl("rent", status, ignore.case = TRUE))

# Join B25004 variables to data, calculate new sums, and add data reliability info

b25004_data <- b25004_raw %>%
  right_join(b25004_vars_clean, by = "variable") %>% # Use right_join to leave out unnecessary subtotals from data
  select(GEOID, year, status, estimate, moe) %>% # Simplify columns
  group_by(GEOID, year, status) %>% # Collapse and summarize
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate) # Calculate new group-wise margins of error
  ) %>%
  ungroup()%>%
  mutate(cv = ((moe/1.645)/estimate)*100) %>% # Calculate coefficient of variation
  mutate(reliability = case_when(
    cv < 15 ~ "High reliability",
    cv >= 15 & cv <= 30 ~ "Medium reliability",
    cv > 30 ~ "Low reliability")
  )

b25004_data_wide <- b25004_data %>%
  pivot_wider(
    names_from = status,
    values_from = c(estimate, moe, cv, reliability)
  )

rental_vacancy <- merge(b25003_data, b25004_data_wide, by = c("GEOID", "year"))

output_vacancy <- rental_vacancy %>%
  select(fips = GEOID, year, renter_occupied = estimate, for_rent_vacant = `estimate_For rent`, rented_vacant = `estimate_Rented, not occupied`) %>%
  mutate(total_units = select(., renter_occupied:rented_vacant) %>%
           rowSums(na.rm = TRUE))

lookup <- read_csv("data/local_lookup.csv") |>  
  select(fips = fips_full, name_long, cbsa_title) |> 
  mutate(fips = as.character(fips))

vacancy_rate <- output_vacancy |> 
  left_join(lookup, by = "fips")


# Data export


write_rds(vacancy_rate, "data/renter_vacancy.rds")

# ---- Table B25106: ACS Cost Burden

b25106_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25106")

b25106_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25106",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})


b25106_vars_cleaned <- b25106_vars |> 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") |> 
  select(variable = name, tenure, income, cb) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner-occupied housing units" ~ "Homeowner",
    tenure == "Renter-occupied housing units" ~ "Renter"),
    cb = case_when(
      cb == "30 percent or more" ~ "Cost-burdened",
      TRUE ~ "Not cost-burdened"))

b25106_raw <- b25106_raw |> 
  right_join(b25106_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, income, cb, estimate)

b25106_data <- b25106_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  select(NAME, GEOID, year, tenure, income, cb, estimate) |> 
  group_by(NAME, GEOID, year, tenure, income, cb) |> 
  summarise(estimate = sum(estimate))


write_rds(b25106_data, "data/b25106_data.rds")





