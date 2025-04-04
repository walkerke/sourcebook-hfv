library(tidyverse)
library(tidycensus)


# Data collection

# Create an object for all years.
years <- 2010:2022

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
b25003_defns <- load_variables(2021, "acs5") %>%
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
# 
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
# 
# Write to rds.
write_rds(output_b25003_wide, "data/b25003_wide.rds")
write_rds(output_b25003_wide_state, "data/b25003_state.rds")
