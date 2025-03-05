# Check terrestrial data
terr_check <- function(speciesFile, counties, plantDB, adjCounties){
  
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
  
  # Check if plant is expected in county
  speciesFinal <- species %>%
    left_join(plantDB, by=join_by("speciesCode" == "PLANT_code", "county", "state"), keep = TRUE) %>%
    mutate(expectedInCounty = case_when(is.na(PLANT_code) ~ FALSE,
                                        speciesCode == PLANT_code ~ TRUE)) %>%
    select(Plot_ID, # unique to terrestrial
           Plot_Key, # unique to terrestrial
           speciesCode, 
           speciesCode, 
           state = state.x, 
           county = county.x, 
           expectedInCounty)
  
  # Check if plant is found in adjacent counties
  adjCountyCheck <- function(speciesCode, state, county, plantDBadj = plantDB,...) {
    species <- speciesCode
    st <- state
    co <- county
    neighbors <- adjCounties %>% filter(state == st, county == co)
    adjcnty <- plantDBadj %>% rename(speciesCode = "PLANT_code") %>%
      filter(state %in% neighbors$neighbor_state,
             county %in% neighbors$neighbor_county,
             speciesCode == species) %>%
      pull(county) %>%
      str_subset(pattern = county, negate = TRUE)
    
    if (length(adjcnty) == 0){
      dat <- "None"
    } else {
      dat <- paste0(adjcnty, collapse = ", ")
    }
    
    return(dat)
  }
  
  speciesFinal$expectedInAdjacentCounty <- pmap(speciesFinal, adjCountyCheck) %>% unlist(use.names = FALSE)
  
  return(speciesFinal)
}

