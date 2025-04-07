# Load packages and set global chunk options
library(tidycensus)
library(tidyverse)


# Data collection

# Create an objects for years.

years <- 2010:2022

# Get variables for Table B25003

b25003_vars <- load_variables(2021, "acs5") %>%
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

b25004_vars <- load_variables(2021, "acs5") %>%
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


# Data prep

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