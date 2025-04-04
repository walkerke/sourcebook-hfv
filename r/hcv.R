library(tidyverse)
library(sf)

hcv <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Housing_Choice_Vouchers_by_Tract/FeatureServer/17/query?f=json&where=(STATE%20IN%20('51'))&outFields=*", quiet = TRUE)

write_rds(hcv, "data/hcv.rds")