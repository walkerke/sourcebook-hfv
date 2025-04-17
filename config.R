# config.R is used to update R scripts that pull down data from public sources.

# The following sets the latest ACS year:

current_acs <- 2023
years_abbrev <- 2015:2023 # Certain tables only go so far back.
years <- 2010:2023
years_partial_1 <- 2010:2014
years_partial_2 <- 2015:2023
comp_years <- c(2010, 2023)


source("r/acs.R")

## ---- The following sets the latest PEP year: ----

latest_pep <- 2024

# Race and other PEP tables lag behind in their release.

lag_pep <- 2023

# Set years for BLS pull:

bls_years <- 2015:2023
bls_years_abbrev <- 20:24
bls_latest <- 2024

# Set years for CHAS pull:

chas_years <- 2016:2021

