library(tidyverse)
library(glue)

# Collect building permits data

years <- 2000:2024

# Generate the appropriate names for the data

header_rows <- read_csv("https://www2.census.gov/econ/bps/County/co2023a.txt", 
                        col_names = FALSE,
                        n_max = 2)

# Parse column names

column_names <- header_rows %>%
  select(X1:X18) %>%
  t() %>%
  as_tibble() %>%
  mutate(group = rep(1:6, each = 3)) %>%
  group_by(group) %>%
  fill(V1, .direction = "updown") %>%
  mutate(names = paste0(V1, ": ", V2)) %>%
  pull(names)

# Scrape annual county data and apply column names

cbps_raw <- map_df(years, ~{

  raw <- read_csv(glue::glue("https://www2.census.gov/econ/bps/County/co{.x}a.txt"), skip = 2, 
                    col_names = FALSE) %>%
    select(X1:X18) %>%
    set_names(column_names)
  
  raw
  
})

cbps_ytd <- read_csv("https://www2.census.gov/econ/bps/County/co2503y.txt", 
                     col_names = FALSE,
                     skip = 2) |> 
  select(X1:X18) |> 
  set_names(column_names) |> 
  mutate(`Survey: Date` = 2025)

cbps_data <- cbps_raw %>% 
  rbind(cbps_ytd) |> 
  mutate(year = `Survey: Date`,
         GEOID = paste0(`FIPS: State`, `FIPS: County`)) %>%
  select(`1-unit: Bldgs`:GEOID) %>%
  filter(str_sub(GEOID, 1, 2) == "51") %>%
  pivot_longer(`1-unit: Bldgs`:`5+ units: Value`,
               names_to = "type",
               values_to = "value") %>%
  separate(type, into = c("Type", "col"), sep = ": ") %>%
  pivot_wider(names_from = col,
              values_from = value) %>%
  rename_with(tolower, Type:Value) %>% 
  select(GEOID, year, type:value) %>% 
  mutate(GEOID = str_replace_all(GEOID, "51515", "51019"), # Merge 'Bedford city' values to 'Bedford county'
         GEOID = str_replace_all(GEOID, "51560", "51005")) # Merge 'Clifton Forge city' values to 'Alleghany county'


lookup <- read_csv("data/local_lookup.csv") |> # Read in lookup csv
  mutate(fips_full = as.character(fips_full)) |> # Convert numeric GEOID to character in order to complete join
  select(GEOID = fips_full, name_long, cbsa_title) # Simplify data

cbps_data <- cbps_data %>% 
  left_join(lookup, by = "GEOID")

write_rds(cbps_data, "data/rds/bps.rds")
