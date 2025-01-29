# speciesFile <- "C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/sampleData/species_richness_terrestrial.csv"
# counties <- counties <- st_read("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/counties.gpkg", quiet = TRUE) %>%
#   st_transform(crs = 4326) 
# stateAbbrv = "ID"
# plantDB <- read.csv("./appData/NV_PLANT_sum.csv") %>% select(-sci_name)
#TERRESTRIAL CODE - 

# !!! NOT WORKING AS EXTERNAL FUNCTION YET
terr_check <- function(speciesFile, counties, plantDB, stateAbbrv){
  
  
  # Load terrestrial species richness
  species_richness_dat_raw <- read.csv(speciesFile, check.names = FALSE)
  
  # Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
  species_richness_dat <- species_richness_dat_raw %>%
    select(`Plot ID`,`Plot Key`, AllSpeciesCheck, AdminState,x, y) %>%
    separate_longer_delim(AllSpeciesCheck, delim = ",") %>%
    mutate(AllSpeciesCheck = str_trim(AllSpeciesCheck)) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join where plot points intersect county polygons
  species <- st_join(species_richness_dat, 
                     counties %>% select(NAME, STATE_NAME), 
                     join = st_intersects) %>%
    rename(Plot_ID = `Plot ID`, Plot_Key = `Plot Key`, state_full = STATE_NAME, state = AdminState, county = NAME) %>%
    filter(state == stateAbbrv)
  
  
  sample_county <- unique(species$county)
  
  species$present <-  species %>%
    left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
    select(PLANT_code.y) %>%
    st_drop_geometry() %>%
    unlist()
  
  
  sample_county <- unique(species$county)
  species$present <-  species %>%
    left_join(plantDB, by=join_by("AllSpeciesCheck" == "PLANT_code", "county"), keep=TRUE) %>%
    select(PLANT_code) %>%
    st_drop_geometry() %>%
    unlist()
  
  #species$status <- species$present %in% species$PLANT_code
  species$expectedInCounty <- species$present %in% species$speciesCode
}

