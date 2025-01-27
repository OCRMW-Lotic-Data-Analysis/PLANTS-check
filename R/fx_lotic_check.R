
# Way to accept multiple files
#files <- list.files("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/MIM_compile/output_to_share/mim_data_2023/2023/battle_mtn_nv_mim_2023/data_analysis_battle_mtn_nv_mim_2023/", full.names = TRUE)[1:2]

# get_single_MIM_data <- function(filePath) {
#   
#   PointID <- read_excel(filePath, sheet = 'Header', range = 'A10', col_names = FALSE, .name_repair = "unique_quiet")[[1]]
#   speciesCodes <- read_excel(filePath, sheet = 'DMA', range = 'B5:B502', col_names = FALSE, .name_repair = "unique_quiet")[[1]] %>% na.omit() %>% as.character()
#   
#   df <- data.frame(PointID, speciesCodes)
# }
# 
# #map(files, get_single_MIM_data)
# 
# 
# lotic_check <- function(speciesFile, plotLocations, counties, plantDB, stateAbbrv){
#   # Load species richness data---- 
#   species_richness_dat_raw <- read.csv(speciesFile)
#   
#   species_richness_dat <- species_richness_dat_raw %>% select(SpecRichDetailEvaluationID, Select.Plant.Species) %>%
#     rename(PLANT_code = Select.Plant.Species) %>%
#     mutate(PlotID = str_split_i(SpecRichDetailEvaluationID , pattern = "_", 1), .keep = "unused" , .before = PLANT_code) %>%
#     distinct()
#   }
#   
  
  
  
  
  
  
  
  



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
  
