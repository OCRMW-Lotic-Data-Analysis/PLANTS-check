# speciesFile <- "C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/sampleData/species_richness_terrestrial.csv"
# plotLocations <- "C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/LoticAIM_2024_Points.csv"
# counties <- counties <- st_read("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/counties.gpkg", quiet = TRUE) %>%
#   st_transform(crs = 4326)
# stateAbbrv = "NV"
# plantDB <- read.csv("./appData/NV_PLANT_sum.csv") %>% select(-sci_name)
# 
# # Way to accept multiple files
# files <- list.files("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/MIM_compile/output_to_share/mim_data_2023/2023/battle_mtn_nv_mim_2023/data_analysis_battle_mtn_nv_mim_2023/", full.names = TRUE)[1:2]


get_single_MIM_data <- function(filePath) {

  PointID <- read_excel(filePath, sheet = 'Header', range = 'A10', col_names = FALSE, .name_repair = "unique_quiet")[[1]]
  speciesCode <- read_excel(filePath, sheet = 'DMA', range = 'B5:B502', col_names = FALSE, .name_repair = "unique_quiet")[[1]] %>% 
    na.omit() %>% 
    as.character() %>%
    toupper()

  df <- data.frame(PointID, speciesCode)
}

#speciesFile <- map_dfr(files, get_single_MIM_data)


lotic_check <- function(speciesFile, plotLocations, counties, plantDB, stateAbbrv){
  # Load species richness data----
  species_richness_dat_raw <- speciesFile

  species_richness_dat <- species_richness_dat_raw %>%
    distinct()
  
  # Load data file with spatial data----
  species_richness_spatial_raw <- read.csv(plotLocations)
  
  # Filter spatial data file to include
  species_richness_spatial <- species_richness_spatial_raw %>%
    select(PointID,AdminState, x, y) %>%
    distinct()
  
  species_sum <- species_richness_dat %>%
    left_join(species_richness_spatial, join_by(PointID),multiple='first') %>%
    filter(AdminState == stateAbbrv) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Spatial join where plot points intersect county polygons
  species <- st_join(species_sum, 
                     counties %>% select(NAME, STATE_NAME), 
                     join = st_intersects) %>%
    rename(state_full = STATE_NAME, state = AdminState, county = NAME)
  
  # Join that lists the plant code again if it is located in the county
  species$present <-  species %>%
    left_join(plantDB, by = join_by("speciesCode" == "PLANT_code", "county"), keep=TRUE) %>%
    select(PLANT_code) %>%
    st_drop_geometry() %>%
    unlist()
  
  # Creates a new column with True or False by comparing columns.
  species$status <- species$present %in% species$PLANT_code
  
  return(species)
  

  }

  
  
  
  
  
  
  
  



  # 
  # # Load terrestrial file
  # species_richness_dat_raw <- read.csv(speciesFile, check.names = FALSE)
  # 
  # # Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
  # species_richness_dat <- species_richness_dat_raw %>%
  #   select(`Plot ID`,`Plot Key`, AllSpeciesCheck, AdminState,x, y) %>%
  #   separate_longer_delim(AllSpeciesCheck, delim = ",") %>%
  #   mutate(AllSpeciesCheck = str_trim(AllSpeciesCheck)) %>%
  #   st_as_sf(coords = c("x", "y"), crs = 4326)
  # 
  # species <- st_join(species_richness_dat, 
  #                    counties %>% select(NAME, STATE_NAME), 
  #                    join = st_intersects) %>%
  #   rename(Plot_ID = `Plot ID`, Plot_Key = `Plot Key`, state_full = STATE_NAME, state = AdminState, county = NAME) %>%
  #   filter(state == stateAbbrv)
  # 
  # 
  # sample_county <- unique(species$county)
  # species$present <-  species %>%
  #   left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
  #   select(PLANT_code.y) %>%
  #   st_drop_geometry() %>%
  #   unlist()
  # 
  # 
  # sample_county <- unique(species$county)
  # species$present <-  species %>%
  #   left_join(plantDB, by=join_by("AllSpeciesCheck" == "PLANT_code", "county"), keep=TRUE) %>%
  #   select(PLANT_code) %>%
  #   st_drop_geometry() %>%
  #   unlist()
  # 
  # species$status <- species$present %in% species$PLANT_code
  
