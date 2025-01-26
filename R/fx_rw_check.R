# Check R&W data

rw_check <- function(speciesFile, plotLocations, counties){

  # Load species richness data---- 
  species_richness_dat <- read.csv(speciesFile)
  
  # Filter species richness data to only include evaluation ID and species codes----
  species_richness_dat <- species_richness_dat %>%
    select(SpecRichDetailEvaluationID, Select.Plant.Species) %>%
    rename(EvaluationID = SpecRichDetailEvaluationID, PLANT_code = Select.Plant.Species) %>%
    distinct()
  
  # Load data file with spatial data----
  species_richness_spatial <- read.csv(plotLocations)
  
  # Filter spatial data file to include Plot ID (4), Evaluation ID (5), X and Y (20 and 21)----
  species_richness_spatial <- species_richness_spatial %>%
    select(PlotID,AdminState, EvaluationID, x, y) %>%
    distinct()
  
  # Join spatial data and species richness data by evaluationID. There are many coordinates with R&W data so this just grabs the first match.
  #NOTE: currently filter to NV but that will need to be updated with the radio button we talked about to filter by state.
  species_sum <- species_richness_dat %>%
    left_join(species_richness_spatial, join_by(EvaluationID),multiple='first') %>%
    filter(AdminState == "NV")
  
  # Format spatial data
  species <- species_sum %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Load our version of the PLANTS database. Remove Sci names because there are rendering issues due to odd symbols in some of the sci names. 
  NV_PLANT_sum <- read.csv("./appData/NV_PLANT_sum.csv")
  NV_PLANT_sum <- NV_PLANT_sum %>% select(-sci_name)

  # Spatial join where plot points intersect county polygons
  species <- st_join(species, 
                 counties %>% select(NAME, STATE_NAME), 
                 join = st_intersects) %>%
    rename(state_full = STATE_NAME, state = AdminState, county = NAME)
  
  # Join that lists the plant code again if it is located in the county
  species$present <-  species %>%
    left_join(NV_PLANT_sum, by=c("PLANT_code", "county"), keep=TRUE) %>%
    select(PLANT_code.y) %>%
    st_drop_geometry() %>%
    unlist()
  
  # Creates a new column with True or False by comparing columns.
  species$status <- species$present %in% species$PLANT_code
  
  return(species)
}
