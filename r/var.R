library(tidyverse)
library(readxl)
library(glue)

# Data collection



# Data provided in .xlsx files separated by locality and MSA
# Historical quarterly data (2014 to 2021) are in separate files
# Beginning 2022 Q1, data provided in new individual quarterly files

# Set paths for locality data

var_hist_locality <- "data/raw/var_2014-2021_locality.xlsx"

var_2022Q1_locality <- "data/raw/var_2022-Q1_locality.xlsx"

# Set paths for MSA data

var_hist_msa <- "data/raw/var_2014-2021_msa.xlsx"

var_2022Q1_msa <- "data/raw/var_2022-Q1_msa.xlsx"

# Set paths for combined data

var_2022Q2 <- "data/raw/var_2022-Q2.xlsx"

var_2022Q3 <- "data/raw/var_2022-Q3.xlsx"

var_2022Q4 <- "data/raw/var_2022-Q4.xlsx"

var_2023Q1 <- "data/raw/var_2023-Q1.xlsx"

var_2023Q2 <- "data/raw/var_2023-Q2.xlsx"

var_2023Q3 <- "data/raw/var_2023-Q3.xlsx"

var_2023Q4 <- "data/raw/var_2023-Q4.xlsx"

# Bring in locality data

locality_data <- var_hist_locality %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = var_hist_locality, 
                      sheet = .x,
                      skip = 4),
         .id = "quarter")

locality_2022Q1 <- read_excel(var_2022Q1_locality,
                              sheet = "SF CT",
                              skip = 4) %>% 
  mutate(quarter = "2022 Q1",
         .before = 1)

locality_2022Q2 <- read_excel(var_2022Q2,
                              sheet = "SF CT CityCounty",
                              skip = 4) %>% 
  mutate(quarter = "2022 Q2",
         .before = 1)

locality_2022Q3 <- read_excel(var_2022Q3,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2022 Q3",
         .before = 1)

locality_2022Q4 <- read_excel(var_2022Q4,
                              sheet = "Q4 CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2022 Q4",
         .before = 1)

locality_2023Q1 <- read_excel(var_2023Q1,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2023 Q1",
         .before = 1)

locality_2023Q2 <- read_excel(var_2023Q2,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2023 Q2",
         .before = 1)

locality_2023Q3 <- read_excel(var_2023Q3,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2023 Q3",
         .before = 1)

locality_2023Q4 <- read_excel(var_2023Q4,
                              sheet = "CityCounty SFCT",
                              skip = 4) %>% 
  mutate(quarter = "2023 Q4",
         .before = 1)

# Bring in MSA data

msa_data <- var_hist_msa %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = var_hist_msa, 
                      sheet = .x,
                      skip = 4),
         .id = "quarter")

msa_2022Q1 <- read_excel(var_2022Q1_msa,
                         sheet = "SF CT",
                         skip = 4) %>% 
  mutate(quarter = "2022 Q1",
         .before = 1)

msa_2022Q2 <- read_excel(var_2022Q2,
                         sheet = "SF CT MSA",
                         skip = 4) %>% 
  mutate(quarter = "2022 Q2",
         .before = 1)

msa_2022Q3 <- read_excel(var_2022Q3,
                         sheet = "MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2022 Q3",
         .before = 1)

msa_2022Q4 <- read_excel(var_2022Q4,
                         sheet = "Q4 MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2022 Q4",
         .before = 1)

msa_2023Q1 <- read_excel(var_2023Q1,
                         sheet = "MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2023 Q1",
         .before = 1)

msa_2023Q2 <- read_excel(var_2023Q2,
                         sheet = "MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2023 Q2",
         .before = 1)

msa_2023Q3 <- read_excel(var_2023Q3,
                         sheet = "MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2023 Q3",
         .before = 1)

msa_2023Q4 <- read_excel(var_2023Q4,
                         sheet = "MSA SFCT",
                         skip = 4) %>% 
  mutate(quarter = "2023 Q4",
         .before = 1)


# Data prep

# Bind historical and recent data
# Add geography type variable

locality_data <- locality_data %>%
  bind_rows(locality_2022Q1, locality_2022Q2, locality_2022Q3, 
            locality_2022Q4, locality_2023Q1, locality_2023Q2, 
            locality_2023Q3, locality_2023Q4) %>% 
  mutate(geography = "Locality",
         .before = 1) %>% 
  rename(name = 'City County')

msa_data <- msa_data %>% 
  bind_rows(msa_2022Q1, msa_2022Q2, msa_2022Q3, msa_2022Q4, 
            msa_2023Q1, msa_2023Q2, msa_2023Q3, msa_2023Q4) %>% 
  mutate(geography = "MSA",
         .before = 1) %>% 
  rename(name = 'MSA') %>% 
  filter(!name == "Grand Total")

# Bind locality and MSA data
# Select and rename columns
# Change statewide 'Grand Total' entries to 'Virginia'

full_data <- bind_rows(locality_data, msa_data) %>% 
  select(geography,
         quarter,
         name,
         units = 'Units',
         med_price = 'Median Price',
         med_dom = 'Median DOM',
         med_asratio = 'Median A/S Ratio') %>% 
  mutate(name = str_replace_all(name, "Grand Total", "Virginia")) %>% 
  mutate(geography = 
           case_when(
             name == "Virginia" ~ "State",
             TRUE ~ geography
           ))


# Data export

# Save home sales data

write_rds(full_data, "data/xls_csv/home-sales.rds")