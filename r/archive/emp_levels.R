library(glue)
library(tidyverse)
library(tigris)

years <- 2015:2022

virginia <- list_counties("VA") |> 
  mutate(fips = paste0(51, county_code)) |> 
  filter(fips != 51515) |> # Remove BEDFORD CITY
  filter(fips != 51560) |>  # Remove CLIFTON FORGE CITY
  filter(fips != 51780) # Remove SOUTH BOSTON

cnty <- virginia$fips

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


qcew_data <- map_dfr(cnty, function(cnty){
  yearly_data <- map_dfr(years, function(yr){
    qcew_pull <- qcewGetAreaData(yr, "a", cnty) |> 
      filter(own_code == 0) |> 
      filter(industry_code == 10) |> 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
      mutate(year = yr)
  })
}) 


lookup <- read_csv("data/local_lookup.csv") 

qcew_data_label <- qcew_data |> 
  mutate(fips_full = area_fips) |> 
  left_join(lookup, by = 'fips_full') |> 
  select(fips_full, name_long, cbsa_title, annual_avg_emplvl, annual_avg_wkly_wage, avg_annual_pay)

write_rds(qcew_data_label, "data/qcew_data.rds")
