# Load necessary packages 
library(readxl) 
library(janitor) 
library(tidyverse)

# URL for the .xlsx file
url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-HIC-Counts-by-CoC.xlsx"

# Create a file path to download to
download_path <- "data/HIC-Counts-by-CoC.xlsx"
dir.create("data", showWarnings = FALSE)
download.file(url, download_path, mode = "wb")

# Function to read a sheet and add year column
read_hic_sheet <- function(sheet_name, file_path) {
  # Skip first row which contains merged cells with section headers
  sheet_data <- read_excel(file_path, sheet = sheet_name, skip = 1) %>%
    # Add a year column based on sheet name
    mutate(Year = as.numeric(sheet_name)) %>%
    # Convert all columns to character to ensure consistent types across sheets
    mutate(across(everything(), as.character))
  
  return(sheet_data)
}

# Get all sheet names (excluding "Revisions" sheet which has different format)
sheet_names <- excel_sheets(download_path)
sheet_names <- sheet_names[sheet_names != "Revisions"]

# Read all sheets and combine
all_hic_data <- map_df(sheet_names, ~read_hic_sheet(.x, download_path))

# Before cleaning, get the original name of the first column (should be "CoC Number")
coc_column_original <- names(all_hic_data)[1]
cat("Original CoC column name:", coc_column_original, "\n")

# Clean column names
all_hic_data <- all_hic_data %>% 
  clean_names() %>%
  # Standardize missing values (may be represented as "." in some sheets)
  mutate(across(where(is.character), ~na_if(.x, ".")))

# After cleaning, get the new name of the first column and year column
coc_column_cleaned <- names(all_hic_data)[1]
year_column <- "year"  # This should be the cleaned name after janitor::clean_names()
cat("Cleaned CoC column name:", coc_column_cleaned, "\n")
cat("Year column name:", year_column, "\n")

# Verify the year column exists
if(!year_column %in% names(all_hic_data)) {
  stop("Year column not found! Check column names: ", paste(names(all_hic_data)[1:10], collapse=", "))
}

# Convert numeric columns back to numeric type
# These are typically columns with rates, counts, or percentages
all_hic_data <- all_hic_data %>%
  mutate(
    # Make sure year is numeric
    year = as.numeric(year),
    # Convert columns that should be numeric back to numeric
    across(
      # Select columns that contain certain patterns in their names
      contains(c("beds", "units", "rate", "participation", "percentage")), 
      ~as.numeric(.x)
    )
  )

# Filter to keep only Virginia CoCs (those starting with "VA-")
virginia_hic_data <- all_hic_data %>%
  filter(str_detect(!!sym(coc_column_cleaned), "^VA-"))

# Preview the Virginia data - showing CoC, Year and a sample metric
sample_beds_column <- names(virginia_hic_data)[grep("total_year_round_beds_es_th_sh", names(virginia_hic_data))[1]]
cat("First few rows of Virginia data with Year column:\n")
virginia_preview <- virginia_hic_data %>% 
  select(!!sym(coc_column_cleaned), year, !!sym(sample_beds_column)) %>%
  arrange(year, !!sym(coc_column_cleaned))
print(head(virginia_preview, 10))

# Check the range of years in the data
year_range <- range(virginia_hic_data$year, na.rm = TRUE)
cat("Year range in the data:", year_range[1], "to", year_range[2], "\n")

# Count records by year
cat("Number of Virginia records by year:\n")
virginia_hic_data %>%
  count(year) %>%
  arrange(year) %>%
  print(n = nrow(.))

# Save the Virginia-only data
write_csv(virginia_hic_data, "data/virginia_hic_data.csv")

# Show dimensions of the Virginia dataset
cat("Virginia data dimensions: ", dim(virginia_hic_data)[1], "rows x", 
    dim(virginia_hic_data)[2], "columns\n")

# Summary of Virginia CoCs in the dataset
cat("Virginia CoCs in the dataset:\n")
virginia_hic_data %>%
  count(!!sym(coc_column_cleaned)) %>%
  arrange(!!sym(coc_column_cleaned)) %>%
  print(n = nrow(.))