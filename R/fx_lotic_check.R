# Extract species codes from single MIM file.  Used in map() when user uploads single or multiple MIM files.
get_single_MIM_data <- function(filePath) {

  PointID <- read_excel(filePath, sheet = 'Header', range = 'A10', col_names = FALSE, .name_repair = "unique_quiet")[[1]]
  speciesCode <- read_excel(filePath, sheet = 'DMA', range = 'B5:B502', col_names = FALSE, .name_repair = "unique_quiet")[[1]] %>% 
    na.omit() %>% 
    as.character() %>%
    toupper()

  df <- data.frame(PointID, speciesCode)
}

#speciesFile <- map_dfr(files, get_single_MIM_data)

# Check Lotic Data
lotic_check <- function(speciesFile, plotLocations, counties, plantDB){
  # Load species richness data----
  species_richness_dat_raw <- speciesFile

  species_richness_dat <- species_richness_dat_raw %>%
    distinct()
  
  # Load data file with spatial data----
  species_richness_spatial_raw <- read.csv(plotLocations)
  
  # Simplify spatial data
  species_richness_spatial <- species_richness_spatial_raw %>%
    select(PointID,AdminState, District, FieldOffice, x, y) %>%
    distinct()
  
  species_sum <- species_richness_dat %>%
    left_join(species_richness_spatial, join_by(PointID),multiple='first') %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join where plot points intersect county polygons
  species <- st_join(species_sum, 
                     counties %>% select(NAME, STATE_NAME), 
                     join = st_intersects) %>%
    rename(state_full = STATE_NAME, state = AdminState, county = NAME)
  
  # Join that lists the plant code again if it is located in the county
  species$present <-  species %>%
    left_join(plantDB, by = join_by("speciesCode" == "PLANT_code", "county", "state"), keep=TRUE) %>%
    select(PLANT_code) %>%
    st_drop_geometry() %>%
    unlist()
  
  # Creates a new column with True or False by comparing columns.
  species$expectedInCounty <- species$present %in% species$speciesCode
  
  return(species)
  }
