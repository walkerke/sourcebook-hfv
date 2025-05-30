# Load the necessary packages to pull data from the Census API.
library(tidycensus)
library(tidyverse)

# The lubridate package allows us to easily work with date fields that exist in the
# CPI data file.
# 
library(lubridate)

# Data collection

# Create an object for years.
years_full <- 2010:2022
years_partial_1 <- 2010:2014
years_partial_2 <- 2015:2022

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

output_b25064_locality <- map_dfr(years_full, function(yr){
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

library(fredr)

# look <- fredr_series_search_text(search_text = "All items less shelter")

#  Consumer Price Index for All Urban Consumers: Rent of Primary Residence 
# in U.S. City Average (CUSR0000SEHA)

cpi_rent <- fredr(
  series_id = "CUSR0000SEHA"
) |> 
  select(date, value) |> 
  mutate(date = as.Date(date),
         value = as.numeric(value),
         year = year(date)) |> 
  group_by(year) |> 
  summarise(index = mean(value))

adjustment <- function(x) {
  transform(x, dollars22 = ((370.2016/index)*estimate))
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
  mutate(dollars22 = ((370.2016/cpi)*estimate))

b25064_cbsa <- output_b25064_cbsa %>%
  left_join(cpi_rent, by = 'year') %>%
  rename(cpi = index) %>%
  mutate(dollars22 = ((370.2016/cpi)*estimate))

b25064_state <- output_b25064_state %>%
  left_join(cpi_rent, by = 'year') %>%
  rename(cpi = index) %>%
  mutate(dollars22 = ((370.2016/cpi)*estimate))


# Data export

# Write to csv files.

write_rds(b25064_locality, "data/b25064_locality.rds")
write_rds(b25064_cbsa, "data/b25064_cbsa.rds")
write_rds(b25064_state, "data/b25064_state.rds")
write_rds(output_b25063, "data/b25063.rds")
