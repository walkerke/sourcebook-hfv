################################################################################
# BLS Data Processing Script
# Purpose: Process QCEW and OEWS data for Virginia housing analysis
################################################################################

## Setup ------------------------------

# Load required libraries

library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(httr2)

# Load locality table

local_lookup <- read_csv("data/local_lookup.csv") |> 
  rename(fips = fips_full)

## BLS reference tables ---------------



## get_qcew_data() --------------------

#' Get QCEW (Quarterly Census of Employment and Wages) data
#'
#' This function retrieves QCEW data from the BLS API for specified areas, years,
#' and quarters. It allows filtering by ownership and aggregation level codes,
#' and can join with local lookup data.
#'
#' @param years Numeric vector of years to retrieve data for (required)
#' @param qtr Either "a" for annual data or a vector of quarters 1-4 (default: c(1:4))
#' @param fips Vector of 5-digit FIPS codes for localities (required)
#' @param fields Optional vector of column names to retain (defaults based on qtr)
#' @param own_code Optional ownership code(s) to filter by (0-5, 8, 9)
#' @param agglvl_code Optional aggregation level code(s) to filter by (default: 70)
#' @param join_lookup Optional vector of field names from local_lookup to join
#' @param local_lookup Data frame containing lookup table with FIPS codes
#'
#' @return A tibble containing the requested QCEW data
#'
get_qcew_data <- function(
    years = NULL,
    qtr = c(1:4),
    fips = NULL,
    fields = NULL,
    own_code = NULL,
    agglvl_code = 70,
    join_lookup = NULL,
    local_lookup = NULL
) {
  # Check required packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required. Please install it.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it.")
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required. Please install it.")
  }
  
  # Import required functions
  `%>%` <- dplyr::`%>%`
  
  # Input validation
  if (is.null(years)) {
    stop("Please provide one or more years to retrieve data")
  }
  
  if (is.null(fips)) {
    stop("Please provide one or more FIPS codes")
  }
  
  # Handle quarterly vs annual data
  is_annual <- FALSE
  if (length(qtr) == 1 && (qtr == "a" || qtr == "A")) {
    is_annual <- TRUE
    qtr <- "a"  # Ensure lowercase for API
  } else {
    # For quarterly data, ensure quarters are in the correct format (1-4)
    qtr <- as.character(qtr)
    if (!all(qtr %in% c("1", "2", "3", "4"))) {
      stop("Quarterly values must be 1, 2, 3, or 4")
    }
  }
  
  # Define default fields based on quarterly or annual
  default_qtr_fields <- c("area_fips", "year", "qtr", "own_code", "industry_code", "agglvl_code", 
                          "qtrly_estabs", "month1_emplvl", "month2_emplvl", "month3_emplvl", "avg_wkly_wage")
  
  default_annual_fields <- c("area_fips", "year", "qtr", "own_code", "industry_code", "agglvl_code", 
                             "annual_avg_estabs", "annual_avg_emplvl", "annual_avg_wkly_wage", "avg_annual_pay")
  
  # Set fields based on input or default
  if (is.null(fields)) {
    fields <- if (is_annual) default_annual_fields else default_qtr_fields
  }
  
  # Helper function to get QCEW data for a specific area, year, and quarter
  get_area_data <- function(year, qtr, area) {
    url <- glue::glue("http://data.bls.gov/cew/data/api/{year}/{qtr}/area/{toupper(area)}.csv")
    
    message(glue::glue("    Fetching: {url}"))
    
    # Use httr2 to fetch the data
    response <- tryCatch({
      httr2::request(url) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    }, error = function(e) {
      message(glue::glue("    Error accessing URL: {e$message}"))
      return(NULL)
    })
    
    # Process the response
    if(!is.null(response) && httr2::resp_status(response) == 200) {
      tryCatch({
        content <- httr2::resp_body_string(response)
        data <- readr::read_csv(content, na = " ", show_col_types = FALSE)
        
        # Add year and qtr if they don't exist in the data
        if (!"year" %in% names(data)) {
          data <- data |> dplyr::mutate(year = year)
        }
        if (!"qtr" %in% names(data)) {
          data <- data |> dplyr::mutate(qtr = qtr)
        }
        
        message(glue::glue("    Successfully retrieved {nrow(data)} rows"))
        return(data)
      }, error = function(e) {
        message(glue::glue("    Error parsing CSV: {e$message}"))
        return(NULL)
      })
    } else {
      status <- if(!is.null(response)) httr2::resp_status(response) else "NA"
      message(glue::glue("    Failed with status code: {status}"))
      return(NULL)
    }
  }
  
  # Process for each area and year/quarter combination
  all_data <- purrr::map(fips, function(area) {
    message(glue::glue("Processing area: {area}"))
    
    # Process for each year
    year_data <- purrr::map(years, function(yr) {
      message(glue::glue("  Processing year: {yr}"))
      
      # Process each quarter (or annual)
      qtr_data <- purrr::map(qtr, function(q) {
        period_label <- if(q == "a") "annual" else toupper(q)
        message(glue::glue("    Processing {period_label} data"))
        
        qcew_pull <- get_area_data(yr, q, area)
        
        if(is.null(qcew_pull) || nrow(qcew_pull) == 0) {
          return(NULL)
        }
        
        # Apply filters
        filtered_data <- qcew_pull
        
        # Filter by own_code if specified
        if (!is.null(own_code)) {
          filtered_data <- filtered_data |> dplyr::filter(own_code %in% !!own_code)
          
          if (nrow(filtered_data) == 0) {
            message(glue::glue("    No data after own_code filter"))
            return(NULL)
          }
        }
        
        # Filter by agglvl_code if specified
        filtered_data <- filtered_data |> dplyr::filter(agglvl_code %in% !!agglvl_code)
        
        if (nrow(filtered_data) == 0) {
          message(glue::glue("    No data after agglvl_code filter"))
          return(NULL)
        }
        
        # Select requested fields
        if (all(fields %in% names(filtered_data))) {
          filtered_data <- filtered_data |> dplyr::select(dplyr::all_of(fields))
        } else {
          missing_fields <- setdiff(fields, names(filtered_data))
          if (length(missing_fields) > 0) {
            message(glue::glue("    Warning: Missing fields: {paste(missing_fields, collapse = ', ')}"))
          }
          filtered_data <- filtered_data |> dplyr::select(dplyr::any_of(fields))
        }
        
        message(glue::glue("    Final data rows: {nrow(filtered_data)}"))
        return(filtered_data)
      }) |>
        purrr::compact() |>
        purrr::list_rbind()
      
      return(qtr_data)
    }) |>
      purrr::compact() |>
      purrr::list_rbind()
    
    return(year_data)
  }) |>
    purrr::compact() |>
    purrr::list_rbind()
  
  # Check if we got any data
  if (is.null(all_data) || nrow(all_data) == 0) {
    message("No data retrieved for the specified parameters")
    return(dplyr::tibble())
  }
  
  # Join with lookup data if requested
  if (!is.null(join_lookup) && !is.null(local_lookup)) {
    if (!all(join_lookup %in% names(local_lookup))) {
      missing_fields <- setdiff(join_lookup, names(local_lookup))
      message(glue::glue("Warning: Missing lookup fields: {paste(missing_fields, collapse = ', ')}"))
      join_lookup <- intersect(join_lookup, names(local_lookup))
    }
    
    # Make sure local_lookup has a fips column
    if (!"fips" %in% names(local_lookup)) {
      if ("fips_full" %in% names(local_lookup)) {
        local_lookup <- local_lookup |> dplyr::mutate(fips = fips_full)
      } else {
        stop("local_lookup must have a 'fips' or 'fips_full' column")
      }
    }
    
    # Prepare lookup data for joining
    lookup_to_join <- local_lookup |> 
      dplyr::select(fips, dplyr::all_of(join_lookup)) |>
      dplyr::distinct()
    
    # Join with lookup data
    all_data <- all_data |>
      dplyr::rename(fips = area_fips) |>
      dplyr::left_join(lookup_to_join, by = "fips")
    
    message(glue::glue("Joined with {length(join_lookup)} fields from lookup table"))
  }
  
  message(glue::glue("Final dataset: {nrow(all_data)} rows, {ncol(all_data)} columns"))
  return(all_data)
}

## Test example -----------------------

test <- get_qcew_data(
  years = c(2022:2023),                       # 2022 and 2023
  qtr = "a",                                  # Annual data
  fips = c(51041, 51760),                     # Chesterfield and Richmond
  agglvl_code = c(70, 71, 72),                # Total, Total by ownership, Total by domain
  join_lookup = c("name_long", "cbsa_title"), # Add name_long and cbsa_title fields
  local_lookup = local_lookup                 # Define table to join
)

