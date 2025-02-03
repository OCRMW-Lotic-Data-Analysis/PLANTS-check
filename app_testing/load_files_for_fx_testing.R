library(dplyr)
library(sf)
library(purrr)
library(readxl)

# # Lotic
# plotLocations <- "C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/LoticAIM_2024_Points.csv"
# counties <- counties <- st_read("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/counties.gpkg", quiet = TRUE) %>%
#   st_transform(crs = 4326)
# stateAbbrv = "NV"
# plantDB <- read.csv("./appData/ALL_PLANT_sum.csv") %>% select(-sci_name)
# 
# # Way to accept multiple files
# files <- list.files("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/MIM_compile/output_to_share/mim_data_2023/2023/battle_mtn_nv_mim_2023/data_analysis_battle_mtn_nv_mim_2023/", full.names = TRUE)[1]
# 
# # Terr
# speciesFile <- "C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/sampleData/species_richness_terrestrial.csv"
# counties <- counties <- st_read("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/counties.gpkg", quiet = TRUE) %>%
#   st_transform(crs = 4326)
# stateAbbrv = "ID"
# plantDB <- read.csv("./appData/NV_PLANT_sum.csv") %>% select(-sci_name)
