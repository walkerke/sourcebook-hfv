# Load necessary packages to work with Excel file and clean it.
library(readxl)
library(janitor)
library(tidyverse)


# Create an object for years in character format. This is used to account for 
# sheets in the Excel file and then combine later.

years <- as.character(2007:2023)

# Create an object for the file path of the Excel file.

path <- "data/PIT-Counts-by-CoC.xlsx"

data <- read_excel("data/PIT-Counts-by-CoC.xlsx") |> 
  clean_names()


# Create a function to read through each sheet and then combine them into a single
# dataframe.

full_set <- map_dfr(years, function(yr) {
  
  # Read in the dataset
  pit_set <- read_excel(path, sheet = yr) %>%
    clean_names() |> 
    select(co_c_number, co_c_name, overall_homeless, sheltered_total_homeless, 
           unsheltered_homeless, overall_homeless_individuals, overall_homeless_family_households,
           sheltered_total_homeless_family_households, unsheltered_homeless_family_households,
           overall_chronically_homeless_individuals, sheltered_total_chronically_homeless_individuals, 
           unsheltered_chronically_homeless_individuals) |> 
    filter(str_detect(co_c_number,"VA"))
    
  
  # Remove the year suffix from the column names
  # suffix_to_remove <- paste0("_", yr)
  # 
  # # Handle the new category column
  # # Also note the use of `across()` to convert cols that should be numeric to numeric
  # # which will prevent errors when row-binding the data
  # if ("co_c_category" %in% names(pit_set)) {
  #   pit_renamed <- pit_set %>%
  #     rename_with(~str_remove(.x, suffix_to_remove)) %>%
  #     mutate(year = yr) %>%
  #     select(1:2, co_c_category, type_of_count, year, everything()) %>%
  #     mutate(across(-c(co_c_number:year), .fns = as.numeric))
  # } else {
  #   pit_renamed <- pit_set %>%
  #     rename_with(~str_remove(.x, suffix_to_remove)) %>%
  #     mutate(year = yr, co_c_category = NA_character_, 
  #            type_of_count = NA_character_) %>%
  #     select(co_c_number:co_c_name, co_c_category, type_of_count, year, everything()) %>%
  #     mutate(across(-c(co_c_number:year), .fns = as.numeric))
  # }
  # 
  # pit_renamed
  
})

# An alternative approach, with a "category" column to reduce the large number of columns:
full_set_longer <- full_set %>%
  pivot_longer(
    cols = 3:12, 
    names_to = "category",
    values_to = "value"
  ) |> 
  mutate(category = case_when(
    category == "overall_homeless" ~ "Overall Homeless",
    category == "sheltered_total_homeless" ~ "Total Sheltered Homeless",
    category == "unsheltered_homeless" ~ "Total Unsheltered Homeless",
    category == "overall_homeless_individuals" ~ "Overall Homeless Individuals",
    category == "overall_homeless_family_households" ~ "Overall Homeless Family Households"
  ))


# Data export

# write the file to csv.

write_rds(full_set_longer, "data/va_pit.rds")
