# Load necessary packages to work with Excel file and clean it.
library(readxl)
library(janitor)
library(tidyverse)


# Create an object for years in character format. This is used to account for 
# sheets in the Excel file and then combine later.

years <- as.character(2007:2024)

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
    category == "overall_homeless_family_households" ~ "Overall Homeless Family Households",
    category == "sheltered_total_homeless_family_households" ~ "Overall Sheltered Homeless Family Households",
    category == "unsheltered_homeless_family_households" ~ "Overall Unsheltered Homeless Family Households",
    category == "overall_chronically_homeless_individuals" ~ "Overall Chronically Homeless Individuals", 
    category == "sheltered_total_chronically_homeless_individuals" ~ "Total Sheltered Chronically Homeless Individuals"
  ))


# Data export

# write the file to csv.

write_rds(full_set_longer, "data/va_pit.rds")

# Install required packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("velofrog/readxlsb")

# Load necessary packages
library(readxlsb)
library(tidyverse)
library(janitor)

# URL for the .xlsb file
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-PIT-Counts-by-CoC.xlsb"

# Create a temporary file to download to
temp_file <- tempfile(fileext = ".xlsb")

# Download the file
download.file(url, temp_file, mode = "wb")

# Years to process
years <- as.character(2007:2024)

# Function to process each sheet
process_sheet <- function(yr, file_path) {
  tryCatch({
    # Read the sheet
    sheet_data <- read_xlsb(file_path, sheet = yr)
    
    # Clean column names
    sheet_data <- sheet_data %>% clean_names()
    
    # Find CoC number column
    coc_col <- grep("coc.*number|co_c.*number", colnames(sheet_data), ignore.case = TRUE, value = TRUE)
    
    if (length(coc_col) > 0) {
      # Filter for VA CoCs
      va_data <- sheet_data %>%
        filter(grepl("VA", !!sym(coc_col[1])))
      
      if (nrow(va_data) > 0) {
        # Add year
        va_data$year <- yr
        return(va_data)
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Error processing sheet ", yr, ": ", e$message)
    return(NULL)
  })
}

# Process all sheets
all_data <- lapply(years, process_sheet, file_path = temp_file)
all_data <- all_data[!sapply(all_data, is.null)]

# If we have data, handle it properly
if (length(all_data) > 0) {
  # First, identify common columns across all data frames
  common_cols <- Reduce(intersect, lapply(all_data, colnames))
  
  # Function to standardize column types - convert all numeric-like columns to numeric
  standardize_types <- function(df, cols) {
    df_subset <- df[, cols, drop = FALSE]
    
    # For each column, try to convert to numeric if it makes sense
    for (col in colnames(df_subset)) {
      # Skip columns that are clearly not numeric
      if (!grepl("number|name|category|type|year", col, ignore.case = TRUE)) {
        # Try to convert to numeric, handling NAs appropriately
        df_subset[[col]] <- as.numeric(as.character(df_subset[[col]]))
      }
    }
    
    return(df_subset)
  }
  
  # Apply type standardization to all data frames
  standardized_data <- lapply(all_data, standardize_types, cols = common_cols)
  
  # Now bind rows should work
  combined_data <- bind_rows(standardized_data)
  
  # Find columns related to homeless counts for pivoting
  homeless_cols <- grep("homeless|chroni", colnames(combined_data), value = TRUE)
  
  if (length(homeless_cols) > 0) {
    # Create longer version with proper naming
    full_set_longer <- combined_data %>%
      pivot_longer(
        cols = all_of(homeless_cols),
        names_to = "category",
        values_to = "value"
      ) %>%
      mutate(category = case_when(
        grepl("overall_homeless$", category) ~ "Overall Homeless",
        grepl("sheltered_total_homeless$", category) ~ "Total Sheltered Homeless",
        grepl("unsheltered_homeless$", category) ~ "Total Unsheltered Homeless",
        grepl("overall_homeless_individuals$", category) ~ "Overall Homeless Individuals",
        grepl("overall_homeless_family_households$", category) ~ "Overall Homeless Family Households",
        grepl("sheltered_total_homeless_family_households$", category) ~ "Overall Sheltered Homeless Family Households",
        grepl("unsheltered_homeless_family_households$", category) ~ "Overall Unsheltered Homeless Family Households",
        grepl("overall_chronically_homeless_individuals$", category) ~ "Overall Chronically Homeless Individuals", 
        grepl("sheltered_total_chronically_homeless_individuals$", category) ~ "Total Sheltered Chronically Homeless Individuals",
        TRUE ~ category
      ))
    
    # View summary of the data
    print(dim(full_set_longer))
    print(head(full_set_longer))
    
    # Save the result
    write_csv(full_set_longer, "data/pit_data_virginia_longer.csv")
    write_csv(combined_data, "data/pit_data_virginia.csv")
    
    print("Data successfully processed and saved!")
  } else {
    print("No homeless-related columns found for pivoting")
  }
} else {
  print("No data was successfully processed from any sheet")
}

# Clean up
unlink(temp_file)