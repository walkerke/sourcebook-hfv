library(tidyverse)
library(httr)
library(jsonlite)
library(httr)
library(glue)

# Data collection

#We're using `dplyr::cols_only()` to create a column specification that will only
#return the 35 columns we want, and will format them correctly (allowing us to appropriately 
# merge the datasets together over time)

col_spec <- cols_only(
  "activity_year" = col_double(),
  "lei" = col_character(),
  "county_code" = col_character(),
  "census_tract" = col_character(),
  "derived_loan_product_type" = col_character(),
  "derived_dwelling_category" = col_character(),
  "derived_ethnicity" = col_character(),
  "derived_race" = col_character(),
  "derived_sex" = col_character(),
  "action_taken" = col_integer(),
  "purchaser_type" = col_integer(),
  "loan_type" = col_integer(),
  "loan_purpose" = col_integer(),
  "reverse_mortgage" = col_integer(),
  "loan_amount" = col_character(),
  "loan_to_value_ratio" = col_character(),
  "interest_rate" = col_character(),
  "total_loan_costs" = col_character(),
  "loan_term" = col_character(),
  "property_value" = col_character(),
  "construction_method" = col_integer(),
  "occupancy_type" = col_integer(),
  "manufactured_home_secured_property_type" = col_integer(),
  "manufactured_home_land_property_interest" = col_integer(),
  "total_units" = col_character(),
  "applicant_age" = col_character(),
  "income" = col_double(),
  "debt_to_income_ratio" = col_character(),
  "denial_reason-1" = col_integer(),
  "denial_reason-2" = col_integer(),
  "denial_reason-3" = col_integer(),
  "denial_reason-4" = col_integer(),
  "tract_minority_population_percent" = col_double(),
  "ffiec_msa_md_median_family_income" = col_double(),
  "tract_to_msa_income_percentage" = col_double(),
  "tract_owner_occupied_units" = col_double()
)

# HMDA data are available via API in two forms: 1) an aggregated JSON API, and 2) a bulk API
# that returns the raw data in CSV format.  We're more interested in the raw data here in CSV
#  format which we could use to do custom aggregations, so we'll pull that down for 2018 through
# 2022.  
#
#  A few notes about the process below:
# - We use `map_dfr()` to iterate through the three years then return the result 
# as a combined dataset.  As we are using the formula notation below (`~`), `.x` takes
# on the value of each year on each run of the iterator. 
# - The `GET()` function from httr is used to make a GET request to the HMDA API.  It requires
# a base URL to request along with optional queries.  
# - We convert the HTTP request result to text then read in the CSV; 
# we pass our column specification defined above to `col_types` to align each dataset
# correctly.
# 
hmda_pull <- map_dfr(2018:2022, ~{
  GET("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv", 
      query = list(
        states = "VA",
        years = .x
      ), 
      progress()) %>%
    content(as = "text") %>%
    read_csv(col_types = col_spec) 
})


# Data prep


# Data export
#' Writing out the raw loan-level data; there are interesting analyses that could be done as-is
#' or many ways to do custom aggregations given that we have tract-level information
write_rds(hmda_pull, "data/hmda_va.rds")

