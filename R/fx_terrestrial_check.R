# Check terrestrial data
terr_check <- function(speciesFile, counties, plantDB){
  
  # Load terrestrial species richness
  species_richness_dat_raw <- read.csv(speciesFile, check.names = FALSE)
  
  # Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
  species_richness_dat <- species_richness_dat_raw %>%
    select(`Plot ID`,`Plot Key`, AllSpeciesCheck, AdminState,x, y) %>%
    separate_longer_delim(AllSpeciesCheck, delim = ",") %>%
    mutate(AllSpeciesCheck = str_trim(AllSpeciesCheck)) %>%
    rename(speciesCode = AllSpeciesCheck) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join where plot points intersect county polygons
  species <- st_join(species_richness_dat, 
                     counties %>% select(NAME, STATE_NAME), 
                     join = st_intersects) %>%
    rename(Plot_ID = `Plot ID`, Plot_Key = `Plot Key`, state_full = STATE_NAME, state = AdminState, county = NAME)
  
  
  sample_county <- unique(species$county)
  
  # Join that lists the plant code again if it is located in the county
  species$present <-  species %>%
    left_join(plantDB, by=join_by("speciesCode" == "PLANT_code", "county", "state"), keep=TRUE) %>%
    select(PLANT_code) %>%
    st_drop_geometry() %>%
    unlist()
  
  #species$status <- species$present %in% species$PLANT_code
  species$expectedInCounty <- species$present %in% species$speciesCode
  
  return(species)
}

