library(tidycensus)
library(tidyverse)

# Set year range for data collection

years <- c(2010, 2022)

# Table B25009: Tenure by Household Size

# Get B25009 data for every locality in Virginia from 2010 and 2022

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


# Table B25010: Average Household Size of Occupied Housing Units by Tenure

# Average household size data from B25010 cannot be aggregated to CBSA or state level
# Must pull B25010 for each geographic level separately
# Create list of CBSA codes for all that are in or partly in Virginia

cbsa <- c("13720", "13980", "14140", "16820", "19260", "25500", "28700", "31340", "32300", "40060", "40220", "44420", "47260", "47900", "49020")

# Get variables for Table B25010

b25010_vars <- load_variables(2021, "acs5") %>% 
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
