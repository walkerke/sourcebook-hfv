################################################################################
# Census Data Processing Script
# Purpose: Process multiple ACS tables for Virginia housing analysis
################################################################################

# Load required libraries
library(tidyverse)
library(tidycensus)
library(fredr)
library(lubridate)

# Helper functions
# Function to convert estimates to inflation-adjusted dollars
adjust_for_inflation <- function(data, cpi_data, base_col = "estimate") {
  data %>%
    left_join(cpi_data, by = "year") %>%
    mutate(adjusted = ((current_index/index) * !!sym(base_col)))
}

# Function to clean locality names
clean_locality_names <- function(data, name_col = "NAME") {
  data %>%
    mutate(
      {{name_col}} := str_remove_all(!!sym(name_col), ", Virginia"),
      {{name_col}} := if_else(!!sym(name_col) == "Bedford city", "Bedford County", !!sym(name_col)),
      fips_full = if_else(GEOID == "51515", "51019", GEOID)  # Standardize Bedford codes
    )
}

# Function to load lookup table
load_locality_lookup <- function() {
  read_csv("data/local_lookup.csv") %>%
    mutate(fips_full = as.character(fips_full)) %>%
    select(GEOID = fips_full, name_long, cbsa_title)
}

# Function to load variables for a specific table
load_table_vars <- function(table_name, year = current_acs, survey = "acs5") {
  load_variables(year, survey) %>%
    filter(str_detect(name, table_name))
}

# Function to get ACS data for a specific table
get_acs_data <- function(table_name, geography = "county", state = "VA", years_to_use = years) {
  map_dfr(years_to_use, function(yr) {
    get_acs(
      geography = geography,
      state = state,
      table = table_name,
      year = yr,
      survey = "acs5",
      cache_table = TRUE
    ) %>%
      mutate(year = yr)
  })
}

# Function to standardize reliability metrics
add_reliability_metrics <- function(data) {
  data %>%
    mutate(
      cv = ((moe/1.645)/estimate) * 100,
      reliability = case_when(
        cv < 15 ~ "High",
        cv >= 15 & cv <= 30 ~ "Medium",
        cv > 30 ~ "Low"
      )
    )
}

################################################################################
# SECTION 1: Consumer Price Index Data for Inflation Adjustment
################################################################################

# Get CPI for All Urban Consumers for income inflation adjustment
cpi <- fredr(
  series_id = "CPIAUCSL"
) %>%
  select(date, value) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value),
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(index = mean(value))

# Get CPI for rent inflation adjustment
cpi_rent <- fredr(
  series_id = "CUSR0000SEHA"
) %>%
  select(date, value) %>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value),
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(cpi = mean(value))

# Store current index values for reference
current_index <- cpi %>% filter(year == current_acs) %>% pull(index)
current_rent_index <- cpi_rent %>% filter(year == current_acs) %>% pull(cpi)

################################################################################
# SECTION 2: Table B25009 - Tenure by Household Size
################################################################################

# Get variables for Table B25009
b25009_vars <- load_table_vars("B25009")

# Get B25009 data
b25009_raw <- get_acs_data("B25009")

# Clean B25009 variable names
b25009_vars_clean <- b25009_vars %>% 
  separate(label, into = c("est", "tot", "tenure", "hhsize"), sep = "!!") %>% 
  select(variable = name, tenure, hhsize) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = str_remove_all(tenure, " occupied"),
    hhsize = str_remove_all(hhsize, " household")
  ) %>%
  filter(across(c(tenure, hhsize), ~ !is.na(.x))) %>% 
  mutate(hhsize = if_else(str_detect(hhsize, "[4-7]"), "4-or-more person", hhsize))

# Join B25009 variables to data and process
b25009_data <- b25009_raw %>%
  mutate(GEOID = str_replace_all(GEOID, "51515", "51019")) %>% # Merge Bedford city to county
  right_join(b25009_vars_clean, by = "variable") %>%
  select(GEOID, year, tenure, hhsize, estimate, moe) %>%
  group_by(GEOID, year, tenure, hhsize) %>%
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate), 
    .groups = "drop"
  ) %>%
  add_reliability_metrics()

# Join with locality lookup and save
hhsize_join <- b25009_data %>% 
  left_join(load_locality_lookup(), by = 'GEOID')

write_rds(hhsize_join, "data/hh_size.rds")

################################################################################
# SECTION 3: Table B25010 - Average Household Size by Tenure
################################################################################

# Define CBSA codes for Virginia
cbsa <- c("13720", "13980", "14140", "16820", "19260", "25500", "28700", 
          "31340", "32300", "40060", "40220", "44420", "47260", "47900", "49020")

# Get variables for Table B25010
b25010_vars <- load_table_vars("B25010")

# Load lookup tables
va_lookup <- read_csv("data/va-cbsa-locality-lookup.csv")
cbsa_lookup <- va_lookup %>% 
  select(cbsa_code, cbsa_title) %>% 
  distinct()

# Get B25010 data for multiple geographies
b25010_raw <- map_dfr(years, function(yr) {
  
  # State data
  b25010_state <- get_acs(
    geography = "state",
    state = "VA",
    table = "B25010",
    year = yr
  ) %>% 
    mutate(
      year = yr,
      geography = "state",
      name = "Virginia"
    )
  
  # CBSA data
  b25010_cbsa <- get_acs(
    geography = "cbsa",
    table = "B25010",
    year = yr
  ) %>% 
    mutate(
      year = yr,
      geography = "cbsa",
      cbsa_code = as.numeric(GEOID)
    ) %>% 
    filter(GEOID %in% cbsa) %>% 
    left_join(cbsa_lookup, by = "cbsa_code") %>% 
    mutate(name = cbsa_title)
  
  # Locality data
  b25010_locality <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25010",
    year = yr
  ) %>% 
    mutate(
      year = yr,
      geography = "locality",
      fips_full = as.numeric(GEOID)
    ) %>% 
    left_join(va_lookup, by = "fips_full") %>% 
    mutate(name = name_long)
  
  # Combine all geographies
  bind_rows(b25010_state, b25010_cbsa, b25010_locality)
})

# Clean B25010 variable names
b25010_vars_clean <- b25010_vars %>% 
  separate(label, into = c("est", "avg", "tot", "tenure"), sep = "!!") %>% 
  select(variable = name, tenure) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    across(.fns = ~replace_na(.x, "All")),
    tenure = str_remove_all(tenure, " occupied")
  )

# Process B25010 data
b25010_data <- b25010_raw %>%
  filter(GEOID != "51515") %>% # Filter out Bedford city
  right_join(b25010_vars_clean, by = "variable") %>%
  select(GEOID, name, geography, year, tenure, estimate, moe) %>%
  add_reliability_metrics()

write_rds(b25010_data, "data/avg_hh_size.rds")

################################################################################
# SECTION 4: Table B11012 - Households by Type
################################################################################

# Get variables for Table B11012
b11012_vars <- load_table_vars("B11012")

# Get B11012 data
b11012_raw <- get_acs_data("B11012", years_to_use = comp_years)

# Clean B11012 variable names
b11012_vars_clean <- b11012_vars %>% 
  separate(label, into = c("est", "tot", "type", "subtype"), sep = "!!") %>% 
  select(variable = name, type, subtype) %>% 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(type, subtype), ~ !is.na(.x))) %>% 
  mutate(
    type = case_when(
      str_detect(type, "couple") ~ "Married or cohabitating couple",
      str_detect(type, "present") ~ "Householder with no partner",
      TRUE ~ type
    ),
    subtype = case_when(
      str_detect(subtype, "With own children") ~ "With own children",
      str_detect(subtype, "With no own") ~ "Without own children",
      subtype == "With relatives, no own children under 18 years" ~ "With relatives, no own children",
      subtype == "With only non relatives present" ~ "With only nonrelatives",
      TRUE ~ subtype
    )
  )

# Process B11012 data
b11012_data <- b11012_raw %>% 
  right_join(b11012_vars_clean, by = "variable") %>%
  select(GEOID, year, type, subtype, estimate, moe) %>%
  group_by(GEOID, year, type, subtype) %>%
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  ) %>%
  mutate(
    cv = ((moe/1.645)/estimate)*100,
    reliability = case_when(
      cv < 15 ~ "High reliability",
      cv >= 15 & cv <= 30 ~ "Medium reliability",
      cv > 30 ~ "Low reliability"
    )
  )

# Join with locality lookup and save
hhtype_join <- b11012_data %>% 
  left_join(load_locality_lookup(), by = 'GEOID')

write_rds(hhtype_join, "data/hh_type.rds")

################################################################################
# SECTION 5: Table B09021 - Living Arrangements of Adults
################################################################################

# Get variables for Table B09021
b09021_vars <- load_table_vars("B09021")

# Get B09021 data for Virginia localities
b09021_raw <- get_acs_data("B09021", years_to_use = years_abbrev)

# Clean B09021 variable names
b09021_vars_clean <- b09021_vars %>% 
  separate(label, into = c("est", "tot", "age", "type"), sep = "!!") %>% 
  select(variable = name, age, type) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    age = str_remove_all(age, " years")
  ) %>%
  mutate(
    type = case_when(
      variable %in% paste0("B09021_00", 2:7) ~ age,
      TRUE ~ type
    ),
    age = case_when(
      variable %in% paste0("B09021_00", 2:7) ~ "All ages",
      TRUE ~ age
    )
  ) %>%
  filter(across(c(age, type), ~ !is.na(.x))) %>% 
  mutate(
    type = case_when(
      str_detect(type, "Householder living") ~ "Lives with married or unmarried partner",
      type == "Child of householder" ~ "Lives with parent(s)",
      type == "Other relatives" ~ "Lives with other relative(s)",
      type == "Other nonrelatives" ~ "Lives with other nonrelative(s)",
      TRUE ~ type
    )
  )

# Process B09021 data
b09021_data <- b09021_raw %>% 
  right_join(b09021_vars_clean, by = "variable") %>%
  select(GEOID, year, age, type, estimate, moe) %>%
  group_by(GEOID, year, age, type) %>%
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  ) %>%
  mutate(
    cv = ((moe/1.645)/estimate)*100,
    reliability = case_when(
      cv < 15 ~ "High reliability",
      cv >= 15 & cv <= 30 ~ "Medium reliability",
      cv > 30 ~ "Low reliability"
    )
  )

# Join with locality lookup and save
lvng_join <- b09021_data %>% 
  left_join(load_locality_lookup(), by = 'GEOID')

write_rds(lvng_join, "data/lvng_arr.rds")

################################################################################
# SECTION 6: Table B19049 - Median Household Income by Age
################################################################################

# Get variables for Table B19049
b19049_vars <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) %in% "B19049")

# Clean variables
b19049_vars_cleaned <- b19049_vars %>%
  separate(label, c("estimate", "medhhincome", "total", "age"), sep = "!!") %>%
  select(variable = name, medhhincome, age) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":")),
    across(.fns = ~str_remove_all(.x, "in the past 12 months  \\(\\in 2021 inflation-adjusted dollars\\)\\ --")),
    across(.fns = ~str_remove_all(.x, "Householder ")),
    age = if_else(age == "under 25 years", "24 years and under", age)
  )

# Function to get B19049 data for different geographies
get_b19049_data <- function(geography, state = NULL) {
  map_dfr(years, function(yr) {
    args <- list(
      geography = geography,
      table = "B19049",
      year = yr
    )
    
    if (!is.null(state)) {
      args$state <- state
    }
    
    acs_pull <- do.call(get_acs, args) %>%
      left_join(b19049_vars_cleaned, by = "variable") %>%
      mutate(year = yr)
    
    if (geography == "state") {
      acs_pull %>% select(variable, year, state = NAME, fips = GEOID, medhhincome, age, estimate, moe)
    } else if (geography == "metropolitan statistical area/micropolitan statistical area") {
      acs_pull %>% 
        select(variable, year, cbsa = NAME, fips = GEOID, medhhincome, age, estimate, moe) %>%
        filter(str_detect(cbsa, "VA"))
    } else {
      acs_pull %>% select(variable, year, locality = NAME, fips = GEOID, medhhincome, age, estimate, moe)
    }
  })
}

# Get data for different geographies
output_b19049_state <- get_b19049_data("state")
output_b19049_cbsa <- get_b19049_data("metropolitan statistical area/micropolitan statistical area")
output_b19049_locality <- get_b19049_data("county", "VA")

# Adjust for inflation and save
state_adj <- output_b19049_state %>% 
  adjust_for_inflation(cpi) %>%
  select(state, year, age, moe, estimate, adjusted) %>% 
  filter(age != "All")

cbsa_adj <- output_b19049_cbsa %>% 
  adjust_for_inflation(cpi) %>%
  select(cbsa, year, age, moe, estimate, adjusted) %>% 
  filter(age != "All")

locality_adj <- output_b19049_locality %>% 
  adjust_for_inflation(cpi) %>%
  select(locality, year, age, moe, estimate, adjusted) %>% 
  mutate(locality = str_remove_all(locality, ", Virginia")) %>% 
  filter(age != "All")

# Save data
write_rds(state_adj, "data/b19049_state.rds")
write_rds(cbsa_adj, "data/b19049_cbsa.rds")
write_rds(locality_adj, "data/b19049_locality.rds")

################################################################################
# SECTION 7: Table B25118 - Household Income Distribution by Tenure
################################################################################

# Get variables for Table B25118
b25118_vars <- load_table_vars("B25118")

# Get B25118 data
b25118_raw <- get_acs_data("B25118")

# Clean B25118 variable names
b25118_vars_cleaned <- b25118_vars %>% 
  separate(label, into = c("est", "total", "tenure", "income"), sep = "!!") %>%  
  select(variable = name, tenure, income) %>% 
  mutate(
    tenure = case_when(
      tenure == "Owner occupied:" ~ "Homeowner",
      tenure == "Renter occupied:" ~ "Renter"
    )
  ) %>% 
  drop_na()

# Process B25118 data
b25118_data <- b25118_raw %>% 
  right_join(b25118_vars_cleaned, by = "variable") %>% 
  select(GEOID, NAME, year, tenure, income, estimate, moe) %>% 
  mutate(
    NAME = str_remove_all(NAME, ", Virginia"),
    income = case_when(
      income %in% c("Less than $5,000", "$5,000 to $9,999", "$10,000 to $14,999") ~ "Less than $15,000",
      income %in% c("$15,000 to $19,999", "$20,000 to $24,999") ~ "$15,000 to $24,999",
      income %in% c("$25,000 to $34,999", "$35,000 to $49,999") ~ "$25,000 to $49,999",
      income == "$50,000 to $74,999" ~ "$50,000 to $74,999",
      TRUE ~ income
    )
  ) %>% 
  group_by(GEOID, NAME, year, tenure, income) %>% 
  summarise(estimate = sum(estimate), .groups = "drop")

# Join with locality lookup and save
b25118_data <- b25118_data %>% 
  left_join(load_locality_lookup(), by = "GEOID")

write_rds(b25118_data, "data/b25118_data.rds")

################################################################################
# SECTION 8: Table B25119 - Median Household Income by Tenure
################################################################################

# Get variables for Table B25119
b25119_vars <- load_table_vars("B25119") %>% 
  separate(label, into = c("est", "medincome", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, tenure) %>%
  mutate(
    tenure = case_when(
      tenure == "Owner occupied (dollars)" ~ "Homeowner",
      tenure == "Renter occupied (dollars)" ~ "Renter",
      TRUE ~ "All households"
    )
  )

# Function to get B25119 data for different geographies
get_b25119_data <- function(geography, state = NULL, name_col = "NAME") {
  map_dfr(years, function(yr) {
    args <- list(
      geography = geography,
      table = "B25119",
      survey = "acs5",
      year = yr
    )
    
    if (!is.null(state)) {
      args$state <- state
    }
    
    acs_pull <- do.call(get_acs, args) %>%
      mutate(year = yr) %>% 
      left_join(b25119_vars, by = "variable")
    
    if (geography == "state") {
      acs_pull %>% select(variable, year, state = NAME, fips = GEOID, tenure, estimate, moe)
    } else if (geography == "metropolitan statistical area/micropolitan statistical area") {
      acs_pull %>% 
        select(variable, year, cbsa = NAME, GEOID, tenure, estimate, moe) %>%
        filter(str_detect(cbsa, "VA"))
    } else {
      acs_pull %>% 
        select(variable, year, locality = NAME, GEOID, tenure, estimate, moe) %>%
        mutate(
          locality = str_remove_all(locality, ", Virginia"),
          year = as.numeric(year),
          estimate = as.numeric(estimate)
        )
    }
  })
}

# Get data for different geographies
b25119_state <- get_b25119_data("state")
b25119_cbsa <- get_b25119_data("metropolitan statistical area/micropolitan statistical area")
b25119_local <- get_b25119_data("county", "VA")

# Adjust for inflation and save
med_inc_state <- b25119_state %>% 
  adjust_for_inflation(cpi) %>%
  select(state, year, tenure, estimate, moe, adjusted)

med_inc_cbsa <- b25119_cbsa %>% 
  adjust_for_inflation(cpi) %>%
  select(GEOID, cbsa, year, tenure, estimate, moe, adjusted)

med_inc_local <- b25119_local %>% 
  adjust_for_inflation(cpi) %>%
  select(locality, year, tenure, estimate, moe, adjusted)

# Save data
write_rds(med_inc_state, "data/b25119_state.rds")
write_rds(med_inc_cbsa, "data/b25119_cbsa.rds")
write_rds(med_inc_local, "data/b25119_local.rds")

################################################################################
# SECTION 9: Table B17001 - Poverty Status by Race and Age
################################################################################

# Create an object for all tables needed
b17001 <- paste0("B17001", LETTERS[2:9])

# Function to create race and ethnicity category
concept_to_race <- function(x) {
  x %>%
    str_remove_all("POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE \\(|\\)") %>%
    str_to_title()
}

# Get variables for Table B17001
b17001_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 7) %in% b17001) %>%
  filter(str_detect(name, "PR") == FALSE)

# Clean B17001 variables
b17001_cleaned <- b17001_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, c("estimate", "total", "poverty", "sex", "age"), sep = "!!") %>%
  select(variable = name, race, poverty, sex, age) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":"))
  )

# Function to get B17001 data for all tables
get_all_b17001_data <- function() {
  map_dfr(b17001, function(tb) {
    map_dfr(years, function(yr) {
      get_acs(
        geography = "county",
        table = tb,
        year = yr,
        state = "VA"
      ) %>%
        left_join(b17001_cleaned, by = "variable") %>%
        mutate(year = yr) %>%
        select(variable, year, locality = NAME, fips = GEOID, race, sex, poverty, age, estimate, moe)
    })
  })
}

# Get B17001 data and clean
output_b17001 <- get_all_b17001_data()

output_b17001_clean <- output_b17001 %>%
  mutate(
    locality = str_remove_all(locality, ", Virginia"),
    race = case_when(
      race == "Black Or African American Alone" ~ "Black",
      race == "American Indian And Alaska Native Alone" ~ "Some Other Race",
      race == "Asian Alone" ~ "Asian",
      race == "Native Hawaiian And Other Pacific Islander Alone" ~ "Some Other Race",
      race == "Some Other Race Alone" ~ "Some Other Race",
      race == "Two Or More Races" ~ "Multiracial",
      race == "White Alone, Not Hispanic Or Latino" ~ "White, Non-Hispanic",
      race == "Hispanic Or Latino" ~ "Hispanic or Latino",
      TRUE ~ race
    ),
    age = case_when(
      age %in% c("Under 5 years", "5 years", "6 to 11 years", 
                 "12 to 14 years", "15 years", "16 and 17 years") ~ "17 years and under",
      age == "18 to 24 years" ~ "18 to 24 years",
      age %in% c("65 to 74 years", "75 years and over") ~ "65 years and over",
      TRUE ~ age
    ),
    estimate = as.numeric(estimate)
  ) %>% 
  filter(poverty != "All", sex != "All", age != "All") %>% 
  group_by(year, locality, fips, race) %>% 
  mutate(totalrace = sum(estimate)) %>% 
  group_by(year, locality, fips, age) %>% 
  mutate(totalage = sum(estimate))

# Calculate poverty rates by race and age
rate_race <- output_b17001_clean %>% 
  filter(poverty == "Income in the past 12 months below poverty level") %>% 
  group_by(year, locality, fips, race, totalrace) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  mutate(rate = estimate/totalrace)

rate_age <- output_b17001_clean %>% 
  filter(poverty == "Income in the past 12 months below poverty level") %>% 
  group_by(year, locality, fips, age, totalage) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>% 
  mutate(rate = estimate/totalage)

# Save data
write_rds(rate_race, "data/poverty_race.rds")
write_rds(rate_age, "data/poverty_age.rds")

################################################################################
# SECTION 10: Table B25003 - Households by Tenure
################################################################################

# Create objects for all tables needed
b25003 <- c("B25003", paste0("B25003", LETTERS[2:9]))
b25007 <- "B25007"

# Function to convert variable to race or ethnicity
concept_to_race <- function(x) {
  x %>%
    str_remove_all("HOUSEHOLDER") %>%
    str_remove_all("TENURE \\(|\\)") %>%
    str_to_title() %>%
    str_replace_all("And", "and") %>%
    str_replace_all("Or", "or") %>%
    str_remove_all(" Alone")
}

# Get variables for Tables B25003 and B25007
b25003_defns <- load_variables(current_acs, "acs5") %>%
  filter(str_sub(name, end = 6) %in% b25003) %>% 
  filter(str_detect(name, "PR") == FALSE)

b25007_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) %in% b25007) %>%
  filter(str_detect(name, "PR") == FALSE)

# Clean B25003 variables
b25003_cleaned <- b25003_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, into = c("est", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, race, tenure) %>% 
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":")),
    across(.fns = ~str_remove_all(.x, " --")),
    across(.fns = ~str_replace_all(.x, "total", "All")),
    across(.fns = ~str_replace_all(.x, "Tenure", "All"))
  )

# Clean B25007 variables
b25007_cleaned <- b25007_defns %>%
  separate(label, into = c("est", "tot", "tenure", "age"), sep = "!!") %>%
  select(variable = name, tenure, age) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":"))
  )

# Function to get tenure data for different tables and geographies
get_tenure_data <- function(tables, geography = "county", state = "VA", vars_cleaned) {
  map_dfr(tables, function(tb) {
    map_dfr(years, function(yr) {
      get_acs(
        geography = geography,
        table = tb,
        year = yr,
        state = state
      ) %>%
        left_join(vars_cleaned, by = "variable") %>%
        mutate(year = yr)
    })
  })
}

# Get B25003 data for localities and state
output_b25003 <- get_tenure_data(b25003, "county", "VA", b25003_cleaned) %>%
  select(variable, year, locality = NAME, fips = GEOID, race, tenure, estimate, moe)

output_b25003_state <- get_tenure_data(b25003, "state", NULL, b25003_cleaned) %>%
  select(variable, year, locality = NAME, fips = GEOID, race, tenure, estimate, moe)

# Get B25007 data
output_b25007 <- get_tenure_data(b25007, "county", "VA", b25007_cleaned) %>%
  select(variable, year, locality = NAME, fips = GEOID, tenure, age, estimate, moe)

# Process B25003 data into wide format
output_b25003_wide <- output_b25003 %>% 
  select(-variable) %>% 
  pivot_wider(
    names_from = tenure,
    values_from = c(estimate, moe)
  ) %>% 
  select(
    year:race, 
    est_all = estimate_All, 
    est_owner = "estimate_Owner occupied",
    est_renter = "estimate_Renter occupied", 
    moe_all = moe_All, 
    moe_owner = "moe_Owner occupied",
    moe_renter = "moe_Renter occupied"
  )

output_b25003_wide_state <- output_b25003_state %>% 
  select(-variable) %>% 
  pivot_wider(
    names_from = tenure,
    values_from = c(estimate, moe)
  ) %>% 
  select(
    year:race, 
    est_all = estimate_All, 
    est_owner = "estimate_Owner occupied",
    est_renter = "estimate_Renter occupied", 
    moe_all = moe_All, 
    moe_owner = "moe_Owner occupied",
    moe_renter = "moe_Renter occupied"
  )

# Clean and join locality info
output_b25003_wide_clean <- output_b25003_wide %>%
  mutate(
    locality = str_remove_all(locality, ", Virginia"),
    locality = if_else(locality == "Bedford city", "Bedford County", locality),
    fips = if_else(fips == "51515", "51019", fips)
  ) %>% 
  left_join(load_locality_lookup(), by = c("fips" = "GEOID")) %>% 
  mutate(
    est_all = as.numeric(est_all),
    est_owner = as.numeric(est_owner)
  ) %>% 
  select(year, locality, cbsa = cbsa_title, race, est_owner, est_all)

output_b25007_clean <- output_b25007 %>%
  mutate(
    locality = str_remove_all(locality, ", Virginia"),
    locality = if_else(locality == "Bedford city", "Bedford County", locality),
    fips = if_else(fips == "51515", "51019", fips)
  ) %>% 
  filter(tenure != "All", age != "All") %>% 
  mutate(
    estimate = as.numeric(estimate),
    age = case_when(
      age == "Householder 15 to 24 years" ~ "24 years and younger",
      age == "Householder 25 to 34 years" ~ "25 to 34 years",
      age == "Householder 35 to 44 years" ~ "35 to 44 years",
      age == "Householder 45 to 54 years" ~ "45 to 54 years",
      age %in% c("Householder 55 to 59 years", "Householder 60 to 64 years") ~ "55 to 64 years",
      TRUE ~ "65 years and over"
    )
  ) %>% 
  group_by(year, locality, fips, tenure, age) %>% 
  summarise(estimate = sum(estimate), .groups = "drop") %>%
  left_join(load_locality_lookup(), by = c("fips" = "GEOID"))

# Save data
write_rds(output_b25003_wide, "data/b25003_wide.rds")
write_rds(output_b25003_wide_state, "data/b25003_state.rds")
write_rds(output_b25007_clean, "data/b25007_clean.rds")

################################################################################
# SECTION 11: Table B25063/B25064 - Gross Rent and Median Gross Rent
################################################################################

# Create objects for tables
b25063 <- "B25063" # Gross Rent
b25064 <- "B25064" # Median Gross Rent

# Get variables for tables
b25063_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) == b25063) %>%
  filter(str_detect(name, "PR") == FALSE)

b25063_defns_2014 <- load_variables(2014, "acs5") %>%
  filter(str_sub(name, end = 6) == b25063) %>%
  filter(str_detect(name, "PR") == FALSE)

b25064_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) == b25064) %>%
  filter(str_detect(name, "PR") == FALSE)

# Clean variables
b25063_cleaned_2014 <- b25063_defns_2014 %>%
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") %>%
  select(variable = name, cash, rent) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":"))
  )

b25063_cleaned <- b25063_defns %>%
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") %>%
  select(variable = name, cash, rent) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":"))
  )

b25064_cleaned <- b25064_defns %>%
  separate(label, into = c("est", "medgrossrent"), sep = "!!") %>%
  select(variable = name, medgrossrent) %>%
  mutate(across(.fns = ~replace_na(.x, "All")))

# Function to get rent data for different geographies
get_rent_data <- function(table, geography, state = NULL, vars_cleaned, years_to_use = years) {
  map_dfr(years_to_use, function(yr) {
    args <- list(
      geography = geography,
      table = table,
      year = yr
    )
    
    if (!is.null(state)) {
      args$state <- state
    }
    
    get_acs(!!!args) %>%
      left_join(vars_cleaned, by = "variable") %>%
      mutate(year = yr)
  })
}

# Get B25063 data
output_b25063_2014 <- get_rent_data(
  b25063, "county", "VA", b25063_cleaned_2014, years_partial_1
) %>% select(variable, year, locality = NAME, fips = GEOID, cash, rent, estimate, moe)

output_b25063 <- get_rent_data(
  b25063, "county", "VA", b25063_cleaned, years_partial_2
) %>% select(variable, year, locality = NAME, fips = GEOID, cash, rent, estimate, moe)

# Get B25064 data for different geographies
output_b25064_locality <- get_rent_data(
  b25064, "county", "VA", b25064_cleaned
) %>% select(variable, year, locality = NAME, fips = GEOID, medgrossrent, estimate, moe)

output_b25064_cbsa <- get_rent_data(
  b25064, "cbsa", NULL, b25064_cleaned
) %>% 
  select(variable, year, cbsa = NAME, fips = GEOID, medgrossrent, estimate, moe) %>%
  filter(str_detect(cbsa, "VA"))

output_b25064_state <- get_rent_data(
  b25064, "state", NULL, b25064_cleaned
) %>% select(variable, year, state = NAME, fips = GEOID, medgrossrent, estimate, moe)

# Combine B25063 data from different year ranges
output_b25063_combined <- bind_rows(output_b25063_2014, output_b25063)

# Clean and process B25063 data
output_b25063_clean <- output_b25063_combined %>% 
  filter(cash != "All") %>% 
  mutate(
    rent = case_when(
      cash == "No cash rent" ~ "No cash rent",
      rent %in% c("Less than $100", "$100 to $149", "$150 to $199", "$200 to $249", 
                  "$250 to $299", "$300 to $349", "$350 to $399", "$400 to $449", 
                  "$450 to $499") ~ "Less than $500",
      rent %in% c("$500 to $549", "$550 to $599", "$600 to $649", 
                  "$650 to $699", "$700 to $749") ~ "$500 to $749",
      rent %in% c("$750 to $799", "$800 to $849", "$850 to $899", 
                  "$900 to $949", "$950 to $999") ~ "$750 to $999",
      rent == "$1,000 to $1,249" ~ "$1,000 to $1,249",
      rent == "$1,250 to $1,499" ~ "$1,250 to $1,499",
      rent == "$1,500 to $1,999" ~ "$1,500 to $1,999",
      TRUE ~ "$2,000 or more"
    )
  )

# Function to adjust for inflation using rent CPI
adjust_for_rent_inflation <- function(estimates, cpi_values) {
  ((current_rent_index/cpi_values) * estimates)
}

# Adjust B25064 data for inflation
b25064_locality <- output_b25064_locality %>%
  left_join(cpi_rent, by = 'year') %>%
  mutate(adjusted = adjust_for_rent_inflation(estimate, cpi))

b25064_cbsa <- output_b25064_cbsa %>%
  left_join(cpi_rent, by = 'year') %>%
  mutate(adjusted = adjust_for_rent_inflation(estimate, cpi))

b25064_state <- output_b25064_state %>%
  left_join(cpi_rent, by = 'year') %>%
  mutate(adjusted = adjust_for_rent_inflation(estimate, cpi))

# Save data
write_rds(b25064_locality, "data/b25064_locality.rds")
write_rds(b25064_cbsa, "data/b25064_cbsa.rds")
write_rds(b25064_state, "data/b25064_state.rds")
write_rds(output_b25063_clean, "data/b25063.rds")

################################################################################
# SECTION 12: Rental Vacancy
################################################################################

# Get variables for Tables B25003 and B25004
b25003_vars <- load_table_vars("B25003")
b25004_vars <- load_table_vars("B25004")

# Get B25003 and B25004 data
b25003_raw <- map_dfr(years, function(yr) {
  get_acs(
    geography = "county",
    table = "B25003",
    year = yr,
    state = "VA"
  ) %>%
    mutate(year = yr) %>%
    select(variable, year, NAME, GEOID, estimate, moe)
})

b25004_raw <- map_dfr(years, function(yr) {
  get_acs(
    geography = "county",
    table = "B25004",
    year = yr,
    state = "VA"
  ) %>%
    mutate(year = yr) %>%
    select(variable, year, NAME, GEOID, estimate, moe)
})

# Clean B25003 variable names
b25003_vars_clean <- b25003_vars %>%
  separate(label, into = c("est", "tot", "tenure"), sep = "!!") %>%
  select(variable = name, tenure) %>%
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(tenure), ~!is.na(.x))) %>%
  filter(str_detect(tenure, "Owner occupied") == FALSE) %>% # Keep only renter housing unit estimates
  mutate(
    tenure = case_when(
      str_detect(tenure, "Renter occupied") ~ "Renter",
      TRUE ~ tenure
    )
  )

# Clean B25004 variable names
b25004_vars_clean <- b25004_vars %>%
  separate(label, into = c("est", "total", "status"), sep = "!!") %>%
  select(variable = name, status) %>%
  mutate(across(.fns = ~str_remove_all(.x, ":"))) %>%
  filter(across(c(status), ~!is.na(.x))) %>%
  filter(grepl("rent", status, ignore.case = TRUE))

# Process B25003 data
b25003_data <- b25003_raw %>%
  right_join(b25003_vars_clean, by = "variable") %>%
  select(GEOID, year, tenure, estimate, moe) %>%
  group_by(GEOID, year, tenure) %>%
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  ) %>%
  add_reliability_metrics()

# Process B25004 data
b25004_data <- b25004_raw %>%
  right_join(b25004_vars_clean, by = "variable") %>%
  select(GEOID, year, status, estimate, moe) %>%
  group_by(GEOID, year, status) %>%
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  ) %>%
  add_reliability_metrics()

# Convert B25004 data to wide format
b25004_data_wide <- b25004_data %>%
  pivot_wider(
    names_from = status,
    values_from = c(estimate, moe, cv, reliability)
  )

# Merge B25003 and B25004 data
rental_vacancy <- inner_join(b25003_data, b25004_data_wide, by = c("GEOID", "year"))

# Calculate vacancy rates
output_vacancy <- rental_vacancy %>%
  select(
    fips = GEOID, 
    year, 
    renter_occupied = estimate, 
    for_rent_vacant = `estimate_For rent`, 
    rented_vacant = `estimate_Rented, not occupied`
  ) %>%
  mutate(
    total_units = pmap_dbl(
      list(renter_occupied, for_rent_vacant, rented_vacant), 
      function(x, y, z) sum(c(x, y, z), na.rm = TRUE)
    )
  )

# Join with locality lookup and save
vacancy_rate <- output_vacancy %>% 
  left_join(load_locality_lookup(), by = c("fips" = "GEOID"))

write_rds(vacancy_rate, "data/renter_vacancy.rds")

################################################################################
# SECTION 13: Table B25106 - ACS Cost Burden
################################################################################

# Get variables for Table B25106
b25106_vars <- load_table_vars("B25106")

# Get B25106 data
b25106_raw <- get_acs_data("B25106")

# Clean B25106 variable names
b25106_vars_cleaned <- b25106_vars %>% 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") %>% 
  select(variable = name, tenure, income, cb) %>% 
  drop_na() %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = case_when(
      tenure == "Owner-occupied housing units" ~ "Homeowner",
      tenure == "Renter-occupied housing units" ~ "Renter"
    ),
    cb = case_when(
      cb == "30 percent or more" ~ "Cost-burdened",
      TRUE ~ "Not cost-burdened"
    )
  )

# Process B25106 data
b25106_data <- b25106_raw %>% 
  right_join(b25106_vars_cleaned, by = "variable") %>% 
  select(NAME, GEOID, year, tenure, income, cb, estimate) %>%
  mutate(
    NAME = str_remove_all(NAME, ", Virginia"),
    NAME = if_else(NAME == "Bedford city", "Bedford County", NAME)
  ) %>%
  group_by(NAME, GEOID, year, tenure, income, cb) %>% 
  summarise(estimate = sum(estimate), .groups = "drop")

write_rds(b25106_data, "data/b25106_data.rds")

################################################################################
# ----- SECTION 14: Table B19013 - Median Household Income by Race ----
################################################################################

# Create object for tables
b19013 <- paste0("B19013", LETTERS[2:9])

# Get variables for Table B19013
b19013_defns <- load_variables(2023, "acs5") %>%
  filter(str_sub(name, end = 7) %in% b19013) %>%
  filter(str_detect(name, "PR") == FALSE)

# Function to extract race from concept
concept_to_race <- function(x) {
  x %>%
    str_remove_all("Median Household Income in the Past 12 Months \\(in 2023 Inflation-Adjusted Dollars\\)") %>%
    str_extract("(?<=\\().*(?=\\))") %>%
    str_trim()
}

# Clean B19013 variables
b19013_cleaned <- b19013_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, c("estimate", "medhhincome"), sep = "!!") %>%
  select(variable = name, medhhincome, race) %>%
  mutate(
    across(.fns = ~replace_na(.x, "All")),
    across(.fns = ~str_remove_all(.x, ":")),
    across(.fns = ~str_remove_all(.x, "Householder")),
    across(.fns = ~str_remove_all(.x, "Alone"))
  )

# Function to get B19013 data for different tables and geographies
get_b19013_data <- function(tables, geography, state = NULL) {
  map_dfr(tables, function(tb) {
    map_dfr(years, function(yr) {
      args <- list(
        geography = geography,
        table = tb,
        year = yr
      )
      
      if (!is.null(state)) {
        args$state <- state
      }
      
      do.call(get_acs, args) %>%
        left_join(b19013_cleaned, by = "variable") %>%
        mutate(year = yr)
    })
  })
}

# Get B19013 data for different geographies
output_b19013_locality <- get_b19013_data(b19013, "county", "VA") %>%
  select(variable, year, locality = NAME, fips = GEOID, race, medhhincome, estimate, moe)

output_b19013_cbsa <- get_b19013_data(b19013, "metropolitan statistical area/micropolitan statistical area") %>%
  select(variable, year, CBSA = NAME, fips = GEOID, race, medhhincome, estimate, moe) %>%
  filter(str_detect(CBSA, "VA"))

output_b19013_state <- get_b19013_data(b19013, "state") %>%
  select(variable, year, state = NAME, fips = GEOID, race, medhhincome, estimate, moe)

# Function to join with CPI and adjust for inflation
process_median_income <- function(data) {
  data %>%
    left_join(cpi, by = "year") %>%
    mutate(adjusted = ((current_index/index) * estimate))
}

# Process data for all geographies
output_b19013_locality <- process_median_income(output_b19013_locality)
output_b19013_cbsa <- process_median_income(output_b19013_cbsa)
output_b19013_state <- process_median_income(output_b19013_state)

# Save data
write_rds(output_b19013_locality, "data/b19013_locality.rds")
write_rds(output_b19013_cbsa, "data/b19013_cbsa.rds")
write_rds(output_b19013_state, "data/b19013_state.rds")

################################################################################
# ---- SECTION 15: Table B25032 - Housing Type by Tenure-----
################################################################################

# Get variables for Table B25032
b25032_vars <- load_table_vars("B25032") %>% 
  filter(str_length(name) == 10)

# Get B25032 data
b25032_raw <- get_acs_data("B25032")

# Clean B25032 variable names
b25032_vars_clean <- b25032_vars %>% 
  separate(label, into = c("est", "tot", "tenure", "type"), sep = "!!") %>% 
  select(variable = name, tenure, type) %>%
  filter(if_any(c(tenure, type), ~!is.na(.x))) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = str_remove_all(tenure, "-occupied housing units")
  ) %>% 
  mutate(
    type = case_when(
      type == "1, detached" ~ "Single-family (detached)",
      type == "1, attached" ~ "Single-family (attached)",
      type %in% c("2", "3 or 4", "5 to 9", "10 to 19") ~ "Small-scale multifamily",
      type == "20 to 49" ~ "Medium-scale multifamily",
      type == "50 or more" ~ "Large-scale multifamily",
      type == "Mobile home" ~ "Mobile (manufactured) home",
      type == "Boat, RV, van, etc." ~ "Other"
    )
  )

# Process B25032 data
b25032_prep <- b25032_raw %>% 
  right_join(b25032_vars_clean, by = "variable") %>% 
  select(GEOID, tenure, year, type, estimate, moe) %>% 
  group_by(GEOID, year, tenure, type) %>% 
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  )

# Add all tenure totals and reliability info
b25032_data <- b25032_prep %>% 
  bind_rows(
    b25032_prep %>%  
      group_by(GEOID, type) %>% 
      summarise(
        estimate = sum(estimate),
        moe = moe_sum(moe, estimate),
        .groups = "drop"
      )
  ) %>% 
  mutate(tenure = replace_na(tenure, "All")) %>% 
  add_reliability_metrics() %>% 
  drop_na(type) %>%
  left_join(va_lookup, by = c("GEOID" = "fips_full"))

write_rds(b25032_data, "data/b25032.rds")

################################################################################
# ----- SECTION 16: Table B25127 - Tenure by Year Structure Built by Units in Structure-----
################################################################################

# Get variables for Table B25127
b25127_vars <- load_table_vars("B25127")

# Get B25127 data
b25127_raw <- get_acs_data("B25127")

# Clean B25127 variable names
b25127_vars_cleaned <- b25127_vars %>% 
  separate(label, into = c("est", "total", "tenure", "yrbuilt", "structure"), sep = "!!") %>% 
  select(variable = name, tenure, yrbuilt, structure) %>% 
  drop_na() %>% 
  mutate(
    across(2:3, .fns = ~str_remove_all(.x, ":")),
    tenure = case_when(
      tenure == "Owner occupied" ~ "Homeowner",
      tenure == "Renter occupied" ~ "Renter"
    )
  )

# Process B25127 data
b25127_data <- b25127_raw %>% 
  right_join(b25127_vars_cleaned, by = "variable") %>% 
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate, moe) %>%
  left_join(va_lookup, by = c("GEOID" = "fips_full"))

write_rds(b25127_data, "data/b25127.rds")

################################################################################
# ----- SECTION 17: Table B25042 - Tenure by Bedrooms -----
################################################################################

# Get variables for Table B25042
b25042_vars <- load_table_vars("B25042")

# Get B25042 data
b25042_raw <- get_acs_data("B25042")

# Clean B25042 variable names
b25042_vars_cleaned <- b25042_vars %>% 
  separate(label, into = c("est", "total", "tenure", "br"), sep = "!!") %>% 
  select(variable = name, tenure, br) %>% 
  drop_na() %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = case_when(
      tenure == "Owner occupied" ~ "Homeowner",
      tenure == "Renter occupied" ~ "Renter"
    )
  )

# Process B25042 data
b25042_data <- b25042_raw %>% 
  right_join(b25042_vars_cleaned, by = "variable") %>% 
  select(NAME, GEOID, year, tenure, br, estimate) %>% 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) %>%
  left_join(va_lookup, by = c("GEOID" = "fips_full"))

write_rds(b25042_data, "data/b25042.rds")

################################################################################
# SECTION 18: Table B25014 - Tenure by Occupants Per Room
################################################################################

# Get variables for Table B25014
b25014_vars <- load_table_vars("B25014") %>% 
  filter(str_length(name) == 10)

# Get B25014 data
b25014_raw <- get_acs_data("B25014")

# Clean B25014 variable names
b25014_vars_clean <- b25014_vars %>% 
  separate(label, into = c("est", "tot", "tenure", "opr"), sep = "!!") %>% 
  select(variable = name, tenure, opr) %>%
  filter(across(c(tenure, opr), ~!is.na(.x))) %>% 
  mutate(
    across(.fns = ~str_remove_all(.x, ":")),
    tenure = str_remove_all(tenure, " occupied"),
    opr = str_remove_all(opr, " occupants per room")
  ) %>% 
  mutate(
    overcrowded = case_when(
      opr %in% c("0.50 or less", "0.51 to 1.00") ~ "Not overcrowded",
      opr == "1.01 to 1.50" ~ "Overcrowded",
      TRUE ~ "Very overcrowded"
    )
  )

# Process B25014 data
b25014_prep <- b25014_raw %>% 
  right_join(b25014_vars_clean, by = "variable") %>% 
  select(GEOID, year, tenure, opr, overcrowded, estimate, moe) %>% 
  group_by(year, GEOID, tenure, opr, overcrowded) %>% 
  summarise(
    estimate = sum(estimate),
    moe = moe_sum(moe, estimate),
    .groups = "drop"
  )

# Add all tenure totals and reliability info
b25014_data <- b25014_prep %>% 
  bind_rows(
    b25014_prep %>%  
      group_by(GEOID, opr, overcrowded) %>% 
      summarise(
        estimate = sum(estimate),
        moe = moe_sum(moe, estimate),
        .groups = "drop"
      )
  ) %>% 
  mutate(tenure = replace_na(tenure, "All")) %>% 
  add_reliability_metrics() %>%
  left_join(va_lookup, by = c("GEOID" = "fips_full"))

write_rds(b25014_data, "data/b25014.rds")