# Load packages and set global chunk options

library(tidyverse)
library(readxl)
library(httr)

# Data collection



# Read in the csv files for statewide and CBSA data

hpi_state_raw <- read_csv("https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_state.csv",
                          col_names = FALSE)

hpi_cbsa_raw <- read_csv("https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv",
                         col_names = FALSE)

# Non-metro data is only available as an xls file; must read in via temp file

# Set up temp file container

temp <- tempfile(fileext = ".xls")

# Write nonmetro xls file to temp file on disk

httr::GET(url = "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_nonmetro.xls",
          write_disk(temp))

# Import xls and skip header rows

hpi_nonmetro_raw <- read_excel(temp, sheet = 1, skip = 2)


# Data prep

# Prep state data and filter only Virginia values

hpi_state_data <- hpi_state_raw %>% 
  setNames(c("name", "year", "quarter", "hpi")) %>% 
  filter(name == "VA") %>% 
  mutate(geography = "State",
         .before = 1) %>%
  mutate(fips = 51,
         .after = 2) %>% 
  mutate(name = "Virginia") %>% 
  add_column(stderror = NA)

# Prep CBSA data and filter only Virginia values

hpi_cbsa_data <- hpi_cbsa_raw %>% 
  setNames(c("name", "fips", "year", "quarter", "hpi", "stderror")) %>% 
  filter(str_detect(name, "VA")) %>% 
  mutate(geography = "CBSA",
         .before = 1) %>% 
  mutate(name = str_remove_all(name, "  \\(MSAD\\)")) %>% 
  mutate(stderror = str_replace_all(stderror, "[^0-9.]+", "")) %>% 
  mutate(hpi = na_if(hpi, "-"),
         stderror = na_if(stderror, "")) %>% 
  mutate(across(hpi:stderror, ~as.numeric(.x)))

# Prep non-metro data and filter only Virginia values

hpi_nonmetro_data <- hpi_nonmetro_raw %>% 
  setNames(c("name", "year", "quarter", "hpi", "stderror")) %>%
  filter(name == "VA") %>% 
  mutate(geography = "Nonmetro",
         .before = 1) %>%
  mutate(fips = NA,
         .after = 2) %>% 
  mutate(name = "Nonmetropolitan area")  %>% 
  mutate(stderror = str_replace_all(stderror, "[^0-9.]+", ""),
         stderror = as.numeric(stderror))

# Bind three geography levels together and create new date field with year and quarter

hpi_all_data <- bind_rows(hpi_state_data, hpi_cbsa_data, hpi_nonmetro_data) %>% 
  mutate(date = paste0(year, " Q", quarter),
         .after = 5)


# Data export

# Save HPI data

write_rds(hpi_all_data, "data/hpi.rds")
