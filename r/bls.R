# Your original setup
library(glue)
library(tidyverse)
library(tigris)
library(readxl)
library(janitor)
library(httr)
source("config.R")



# Virginia county setup seems fine
virginia <- list_counties("VA") |> 
  mutate(fips = paste0(51, county_code)) |> 
  filter(fips != 51515) |> # Remove BEDFORD CITY
  filter(fips != 51560) |>  # Remove CLIFTON FORGE CITY
  filter(fips != 51780) # Remove SOUTH BOSTON
cnty <- virginia$fips

# Modified function without the timeout parameter
qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  
  # Use httr to check if the URL is accessible
  response <- tryCatch({
    httr::GET(url)
  }, error = function(e) {
    message(paste("Error accessing", url, ":", e$message))
    return(NULL)
  })
  
  # If we got a response, try to read the CSV
  if(!is.null(response) && httr::status_code(response) == 200) {
    tryCatch({
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- read.csv(text = content, header = TRUE, sep = ",", 
                       quote="\"", dec=".", na.strings=" ", skip=0)
      return(data)
    }, error = function(e) {
      message(paste("Error parsing CSV from", url, ":", e$message))
      return(NULL)
    })
  } else {
    message(paste("Failed to get data from", url, 
                  "Status code:", 
                  if(!is.null(response)) httr::status_code(response) else "NA"))
    return(NULL)
  }
}

# # Test with one county to make sure it works
# test_county <- cnty[1]
# test_data <- qcewGetAreaData(2022, "a", test_county)
# print(paste("Test county:", test_county))
# print(paste("Got data:", !is.null(test_data)))
# if(!is.null(test_data)) {
#   print(paste("Number of rows:", nrow(test_data)))
#   print(head(test_data, 2))
# }

# Modified function to handle multiple years
qcew_data <- map_dfr(cnty, function(county_fips) {
  message(paste("Processing county:", county_fips))

  yearly_data <- map_dfr(bls_years, function(yr) {
    message(paste("  Processing year:", yr))
    
    qcew_pull <- qcewGetAreaData(yr, "a", county_fips)
    
    if(is.null(qcew_pull) || nrow(qcew_pull) == 0) {
      message(paste("  No data found for county:", county_fips, "year:", yr))
      return(NULL)
    }
    
    # Check if the necessary columns exist
    req_columns <- c("own_code", "industry_code", "annual_avg_emplvl", 
                     "annual_avg_wkly_wage", "avg_annual_pay")
    
    if(!all(req_columns %in% names(qcew_pull))) {
      message(paste("  Missing required columns for county:", county_fips, "year:", yr))
      return(NULL)
    }
    
    # Filter and select the data
    filtered_data <- qcew_pull %>% 
      filter(own_code == 0) %>% 
      filter(industry_code == 10) %>% 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage, avg_annual_pay) %>% 
      mutate(year = yr)
    
    # Check if we got any data after filtering
    if(nrow(filtered_data) == 0) {
      message(paste("  No data after filtering for county:", county_fips, "year:", yr))
      return(NULL)
    }
    
    return(filtered_data)
  })
  
  return(yearly_data)
})

local_lookup <- read_csv("data/local_lookup.csv") %>% 
  mutate(fips = fips_full)

  

qcew_data_label <- qcew_data %>% 
  mutate(fips = area_fips) %>% 
  left_join(local_lookup, by = "fips")

write_rds(qcew_data_label, "data/qcew_data.rds")


# Download and unzip all years to individual folders based on respective years. 
walk(bls_years_abbrev, ~{
  
  url <- glue("https://www.bls.gov/oes/special-requests/oesm{.x}ma.zip")
  
  file <- basename(url)
  
  path <- file.path("data", file)
  
  if (!file.exists(path)) {
    GET(url, 
        write_disk(path, overwrite = TRUE),
        progress(type = "down"),
        add_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    )}
  
  print(glue("Unzipping {.x}..."))
  unzip(path, exdir = file.path("data", .x))
  
  # Delete the zip file after unzipping
  print(glue("Deleting zip file {file}..."))
  unlink(path)
})

combined <- map_dfr(bls_years_abbrev, function(year){
  
  path <- file.path("data", year)
  
  # List all xlsx files
  files <- list.files(path, 
                      pattern = "\\.xlsx$", 
                      full.names = TRUE, 
                      recursive = TRUE)
  
  # Filter out temporary Excel files
  files <- files[!grepl("^~\\$", basename(files))]
  
  # Filter to get only the MSA files we want
  files <- files[grepl(glue("MSA_M20{year}_dl"), basename(files))]
  
  if(length(files) == 0) {
    warning(glue("No matching file found in {path}"))
    return(NULL)
  }
  
  # Use the first matching file
  file <- files[1]
  
  raw <- read_xlsx(file,
                   sheet = glue("MSA_M20{year}_dl"),
                   na = c("*", "**", "#"))
  
  va <- raw |> 
    filter(PRIM_STATE == "VA") |> 
    mutate(year = year)
  
  va
})

write_rds(combined, "data/oews.rds")

# Clean up - delete the year folders after creating the combined RDS file
print("Cleaning up year folders...")
walk(bls_years_abbrev, function(year) {
  folder_path <- file.path("data", year)
  if(dir.exists(folder_path)) {
    print(glue("Deleting folder {folder_path}..."))
    unlink(folder_path, recursive = TRUE)
  }
})
print("Cleanup complete")


remotes::install_github("keberwein/blscrapeR")

library(blscrapeR)


# The following pulls all localities in Virginia to help iterate through BLS series.
# The TIGRIS package allows us to do this and pull FIPS codes.

virginia <- list_counties("VA")

# The following creates an additional column of series ids based on BLS labels. The
# mutate() function creates a new column called 'seriesid' while the paste0() function
# concatenates the necessary pieces of the series id without spaces.

virginia <- virginia %>% mutate(seriesid = paste0("LAUCN51", county_code, "0000000005"), #employment
                                rateseries = paste0("LAUCN51", county_code, "0000000003"), #unemployment rate
                                lfseries = paste0("LAUCN51", county_code, "0000000006"), #labor force
                                unemployedseries = paste0("LAUCN51", county_code, "0000000004") # unemployment
)

# Only the 'seriesid' field is needed to pull from the BLS API, therefore we need to
# create a list of series ids for all 136 Virginia localities. 

list <- list(virginia$seriesid)
ratelist <- list(virginia$rateseries)
lflist <- list(virginia$lfseries)
unemploymentlist <- list(virginia$unemployedseries)

# The blscrapeR does not accept a list in its query pull and requires a vector. Therefore, 
# unlist() is used in order to simplifies the list to a vector.

list <- unlist(list)   
ratelist <- unlist(ratelist)
lflist <- unlist(lflist)
unemploymentlist <- unlist(unemploymentlist)

# The BLS API has a series query limit of 50, but there are 136 localities (series ids)
# to pull. So we need to split the vector into chunks using the split() function.

list <- split(list, ceiling(seq_along(list)/50))
ratelist <- split(ratelist, ceiling(seq_along(ratelist)/50))
lflist <- split(lflist, ceiling(seq_along(lflist)/50))
unemploymentlist <- split(unemploymentlist, ceiling(seq_along(unemploymentlist)/50))


# A function is needed that will run through the bls_api() using the chunked vector we
# created previously. This function will be utilized in map_dfr() to combine the three 
# iterations together into a single table

bls_employment_pull <- function(list){
  bls_api(list,
          startyear = 2008, endyear = bls_latest, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_employed <- map_dfr(list, bls_employment_pull)

bls_unemployment_rate_pull <- function(ratelist){
  bls_api(ratelist,
          startyear = 2008, endyear = bls_latest, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_unemployment_rate <- map_dfr(ratelist, bls_unemployment_rate_pull)

bls_laborforce_pull <- function(lflist){
  bls_api(lflist,
          startyear = 2008, endyear = bls_latest, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_laborforce <- map_dfr(lflist, bls_laborforce_pull)

bls_unemployment_pull <- function(unemploymentlist){
  bls_api(unemploymentlist,
          startyear = 2008, endyear = bls_latest, Sys.getenv("BLS_KEY")) %>%
    dateCast()
}

va_unemployment <- map_dfr(unemploymentlist, bls_unemployment_pull)


# Now that a data frame is created with all localities and years needed, we need to create
# an additional column for the FIPS code of each locality, so that we can join to a table 
# of locality names later.

fip_function <- function(x) {
  x %>%
    mutate(fips = substr(seriesID, 6, 10))
}

result <- list(va_employed, va_unemployment_rate, va_laborforce, va_unemployment) %>% 
  lapply(fip_function)

names(result) = c('va_employed', 'va_unemployment_rate', 'va_laborforce', 'va_unemployment')

list2env(result, envir = .GlobalEnv)


# Write to rds.

write_rds(va_employed, "data/va_employment_level.rds")
write_rds(va_unemployment_rate, "data/va_unemployment_rate.rds")
write_rds(va_laborforce, "data/va_laborforce_level.rds")
write_rds(va_unemployment, "data/va_unemployment_level.rds")