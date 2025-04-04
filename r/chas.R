# The following R script pulls down data for the set jurisdictions and years from 
# HUD's Comprehensive Housing Affordability Data.

#Load the necessary packages to download the data from the HUD website.

library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(httr)

source("config.R")


## DO NOT MODIFY THE CODE BELOW -----

# Create an object for the geographic level needed. "50" denotes county-level.
sumlev <- "050"

# Create a file folder based on the specified geographic level within the
# working directory.

dir.create(glue("data/","{sumlev}"))

# Download and unzip all years to individual folders based on respective years. 

walk(chas_years, ~{
  
  url <- glue("https://www.huduser.gov/PORTAL/datasets/cp/{.x - 4}thru{.x}-{sumlev}-csv.zip")
  
  file <- basename(url)
  
  path <- file.path("data/", sumlev, file)
  
  if (!file.exists(path)) {
    GET(url, write_disk(path, overwrite = TRUE), progress(type = "down"))
  }
  
  print(glue("Unzipping {.x}..."))
  unzip(path, exdir = file.path("data/", sumlev, .x))
  
  # Delete the zip file after unzipping
  print(glue("Deleting zip file {file}..."))
  unlink(path)
  
})


# Tables to get
tables <- c(7,9, paste0(18, LETTERS[3]))

# Go through and write out the various tables
walk(tables, function(table) {
  
  mytable <- purrr::map_df(chas_years, function(year) {
    
    # Identify the year-specific folder
    path <- file.path("data/", "050", year)
    
    # Find the file - it may be buried so use recursive = TRUE
    file <- list.files(path, pattern = glue("Table{table}.csv"), recursive = TRUE)
    
    # Read in the file quietly
    raw <- read_csv(file.path(path, file), col_types = cols())
    
    # Clean it up
    cleaned <- raw %>%
      clean_names() %>%
      mutate(fips = paste0(st, cnty)) |> 
      separate(name, into = c("county", "state"), sep = ",") %>%
      filter(st == "51") %>%
      pivot_longer(starts_with("T"), 
                   names_to = "code",
                   values_to = "value") %>%
      mutate(id = str_extract(code, "\\d+$"),
             type = str_extract(code, "est|moe")) %>%
      select(-code) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      rename(Estimate = est, MOE = moe) %>%
      mutate(Code := glue("T{table}_est{id}"),
             Year = year) %>%
      select(Code, Year, Estimate, MOE, everything(), -id)%>%      
      mutate(fips = case_when(
        fips == "51515" ~ "51019",
        TRUE ~ fips
      )) %>%
      mutate(county = case_when(
        county == "Bedford city" ~ "Bedford County",
        TRUE ~ county
      )) 
    
    # Account for different data dictionaries
    # Find the data dictionary in the appropriate folder
    dict_path <- list.files(path, pattern = "dictionary", recursive = TRUE, full.names = TRUE) 
    
    # Read in the data dictionary and merge
    dict <- read_excel(dict_path, 
                       sheet = glue("Table {table}"))
    
    cleaned_with_dict <- cleaned %>%
      left_join(dict, by = c("Code" = "Column Name"))
    
    cleaned_with_dict
    
  }) 
  
  file_name <- glue("Table{table}.csv")
  
  message(glue("Writing file {file_name}..."))
  
  write_csv(mytable, glue("data/","{file_name}"))
  
})

unlink("data/050", recursive = TRUE)

Table7 <- read_csv("data/Table7.csv") |> 
  filter(Line_Type == "Detail") |> 
  clean_names() |> 
  subset(fips %in% juris) |> 
  select(year, estimate, moe, county, fips, tenure, household_income, household_type, cost_burden) |>
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |>
  mutate(household_income = case_when(
    household_income == "household income is less than or equal to 30% of HAMFI" ~ "30% AMI or less",
    household_income == "household income is greater than 30% but less than or equal to 50% of HAMFI" ~ "31 to 50% AMI",
    household_income == "household income is greater than 50% but less than or equal to 80% of HAMFI" ~ "51 to 80% AMI",
    household_income == "household income is greater than 80% but less than or equal to 100% of HAMFI" ~ "81 to 100% AMI",
    household_income == "household income is greater than 100% of HAMFI" ~ "101% AMI or greater"
  )) |>
  mutate(household_type = case_when(
    household_type == "household type is elderly family (2 persons, with either or both age 62 or over)" ~ "Elderly family",
    household_type == "household type is small family (2 persons, neither person 62 years or over, or 3 or 4 persons)" ~ "Small family",
    household_type == "household type is large family (5 or more persons)" ~ "Large family",
    household_type == "household type is elderly non-family" ~ "Elderly non-family",
    household_type == "other household type (non-elderly non-family)" ~ "Non-elderly non-family"
  ))  |>
  mutate(cost_burden = case_when(
    cost_burden == "housing cost burden is less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "housing cost burden is greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "housing cost burden is greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "housing cost burden not computed (household has no/negative income)" ~ "No or negative income"
  )) |>
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  ))

Table9 <- read_csv("data/Table9.csv") |> 
  filter(Line_Type == "Detail") |> 
  subset(fips %in% juris) |>  
  clean_names() |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(race = case_when(
    race_ethnicity == "White alone, non-Hispanic" ~ "White, non-Hispanic",
    race_ethnicity == "Black or African-American alone, non-Hispanic" ~ "Black",
    race_ethnicity == "Asian alone, non-Hispanic" ~ "Asian",
    race_ethnicity == "Hispanic, any race" ~ "Hispanic or Latino",
    TRUE ~ "Another race, including multiracial"
  ))   |> 
  mutate(cost_burden = case_when(
    cost_burden == "less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "greater than 50%" ~ "Severely cost-burdened",
    cost_burden == "not computed (no/negative income)" ~ "No or negative income"
  )) |> 
  mutate(cb_group = case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    TRUE ~ cost_burden
  )) |> 
  select(county, fips, year, race, cost_burden, cb_group, estimate, moe)


Table18c <- read_csv("data/Table18C.csv")  %>% 
  clean_names() %>% 
  subset(fips %in% juris) |>  
  filter(line_type == "Detail") %>% 
  select(county, fips, year, estimate, tenure, rent, household_income) %>% 
  group_by(county, fips, year, tenure, rent, household_income) %>% 
  summarise(estimate = sum(estimate)) |> 
  filter(tenure == "Renter occupied") %>% 
  mutate(rent = case_when(
    rent == "greater than RHUD30 and less than or equal to RHUD50" ~ "31 to 50 percent AMI",
    rent == "greater than RHUD50 and less than or equal to RHUD80" ~ "51 to 80 percent AMI",
    rent == "greater than RHUD80" ~ "80 percent AMI or greater",
    rent == "less than or equal to RHUD30" ~ "30 percent AMI or below")) %>% 
  mutate(household_income = case_when(
    household_income == "greater than 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51 to 80 percent AMI",
    household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31 to 50 percent AMI",
    household_income == "less than or equal to 30% of HAMFI" ~ "30 percent AMI or below"
  )) |>
  group_by(county, fips, year, rent, household_income) %>% 
  mutate(fips = as.double(fips)) |> 
  summarise(estimate = sum(estimate)) %>% 
  mutate(match = case_when(
    rent == household_income ~ "Affordable",
    rent == "30 percent AMI or below" & household_income == "31 to 50 percent AMI" ~ "Very affordable",
    rent == "30 percent AMI or below" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    rent == "30 percent AMI or below" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    rent == "31 to 50 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Affordable",
    rent == "31 to 50 percent AMI" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    rent == "31 to 50 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    rent == "31 to 50 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    rent == "51 to 80 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    rent == "51 to 80 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    rent == "51 to 80 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    rent == "80 percent AMI or greater" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    rent == "80 percent AMI or greater" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    rent == "80 percent AMI or greater" & household_income == "51 to 80 percent AMI" ~ "Unaffordable"
  )) %>% 
  mutate(gapcode = case_when(
    match == "Unaffordable" ~ "Gap",
    TRUE ~ "Matches or less than income"
  ))

write_rds(Table7, "data/table7_chas.rds")
write_rds(Table9, "data/table9_chas.rds")
write_rds(Table18c, "data/table18c_chas.rds")
