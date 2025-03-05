# Check R&W data
rw_check <- function(speciesFile, plotLocations, counties, plantDB, adjCounties){
  
  # Load species richness data---- 
  species_richness_dat_raw <- read.csv(speciesFile)
  
  species_richness_dat <- species_richness_dat_raw %>% select(SpecRichDetailEvaluationID, Select.Plant.Species) %>%
    rename(speciesCode = Select.Plant.Species) %>%
    mutate(PlotID = str_split_i(SpecRichDetailEvaluationID , pattern = "_", 1), .keep = "unused" , .before = speciesCode) %>%
    distinct()
  
  # Load data file with spatial data----
  species_richness_spatial_raw <- read.csv(plotLocations)
  
  # Filter spatial data file to include Plot ID (4), Evaluation ID (5), X and Y (20 and 21)----
  species_richness_spatial <- species_richness_spatial_raw %>%
    select(PlotID,AdminState, x, y) %>%
    distinct()
  
  # Join spatial data and species richness data by PlotID There are many coordinates with R&W data so this just grabs the first match.
  #NOTE: currently filter to NV but that will need to be updated with the radio button we talked about to filter by state.
  species_sum <- species_richness_dat %>%
    left_join(species_richness_spatial, join_by(PlotID),multiple='first') %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join where plot points intersect county polygons
  species <- st_join(species_sum, 
                     counties %>% select(NAME, STATE_NAME), 
                     join = st_intersects) %>%
    rename(state_full = STATE_NAME, state = AdminState, county = NAME)
  
  # Check if plant is expected in county
  speciesFinal <- species %>%
    left_join(plantDB, by=join_by("speciesCode" == "PLANT_code", "county", "state"), keep = TRUE) %>%
    mutate(expectedInCounty = case_when(is.na(PLANT_code) ~ FALSE,
                                        speciesCode == PLANT_code ~ TRUE)) %>%
    select(PlotID, # unique to RW
           speciesCode,
           state = state.x,
           county = county.x,
           expectedInCounty)
  
  print("before")
  print(speciesFinal)
  
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
  
  print("after")
  print(speciesFinal)
  
  return(speciesFinal)
}

