library(tidyverse)
library(httr)
library(glue)
library(tigris)
library(devtools)

# Create a visualization of the top occupations by industry for each metro area 
# with information about average wages/income.

# The blscrapeR packages is no longer available, while the blsAPI package is not easy-to-use.

virginia <- list_counties("VA") |> 
mutate(fips = paste0(51, county_code)) |> 
  filter(fips != 51515) |> # Remove BEDFORD CITY
  filter(fips != 51560) |>  # Remove CLIFTON FORGE CITY
  filter(fips != 51780) # Remove SOUTH BOSTON


fips <- as.list(virginia$fips)

nn <- blsAPI('CN5170000000000')



unemployment <- mapdfr(fips, function(fips){
  blsAPI(payload = glue("CN{fips} )}


years <- 22

url <- glue("https://www.bls.gov/oes/special-requests/oesm22ma.zip")

download.file(url, "oews.zip")

unzip("oesm22ma.zip", exdir = "extracted_folder")

