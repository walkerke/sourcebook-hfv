library(tibble)
library(tidygeocoder)
library(tidyverse)
library(readxl)

Sys.getenv('GEOCODIO_API_KEY')

# Data collection
# Access the NHPD database and navigate to the full data.
# Set filter for "State: VA" and export the filtered grid via "Export Subsidies"
# As of 2/1/2024 the data was last refreshed in December 2023.

va_subsidies <- read_xlsx("data/nhpd_subsidies_va.xlsx")

va_subsidies <- va_subsidies %>% 
  mutate(fulladdress = paste(va_subsidies$`Street Address`, va_subsidies$`City`, va_subsidies$`State`, va_subsidies$`Zip Code`, sep = ", "))

lat_long <- va_subsidies %>%
  geocode(address = fulladdress, method = 'geocodio',
          full_results = TRUE,
          unique_only = FALSE,
          lat = Latitude, 
          long = Longitude)

write_rds(lat_long, "data/va_subsidies_geocoded.rds")
