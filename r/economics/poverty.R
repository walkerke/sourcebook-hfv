library(tidyverse)
library(tidycensus)

# Create an object for years needed.

years <- 2010:2022

# Create an object for all tables needed.

b17001 <- paste0("B17001", LETTERS[2:9]) # Need to append letters B-I

# Create a function to createa a race and ethnicity category.

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
      select(variable, year, locality = NAME, fips = GEOID, race, poverty, age,
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
  group_by(year,locality, fips, race, poverty, age) |> 
  summarise(estimate = sum(estimate))
  

# Write all data to csv.
write_rds(output_b17001, "data/b17001.csv")
