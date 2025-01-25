# Check R&W data

rw_check <- function(speciesFile, plotLocations){
  # Require both uploaded files---- 
  req(speciesFile)
  req(plotLocations)
  
  # Load species richness data---- 
  species_richness_rw <- read.csv(speciesFile$datapath)
  
  # Filter species richness data to only include evaluation ID and species codes----
  species_richness_rw<- species_richness_rw %>%
    select(3,5) %>%
    unique()
  
  # Clean up column names----
  colnames(species_richness_rw) <- c("EvaluationID", "PLANTS_code")
  
  # Load data file with spatial data----
  species_richness_rw_spatial <- read.csv(plotLocations$datapath)
  
  # Filter spatial data file to include Plot ID (4), Evaluation ID (5), X and Y (20 and 21)----
  species_richness_rw_spatial <- species_richness_rw_spatial[,c(4,5,20,21)]
  
  # Remove duplicates
  species_richness_rw_spatial<-unique(species_richness_rw_spatial)
  
  # Join spatial data and species richness data by evaluationID. There are many coordinates with R&W data so this just grabs the first match.
  #NOTE: currently filter to NV but that will need to be updated with the radio button we talked about to filter by state.
  species_sum<-species_richness_rw %>%
    left_join(species_richness_rw_spatial, join_by(EvaluationID),multiple='first') %>%
    filter(AdminState == "NV")
  
  # Format spatial data
  species_rw<- species_sum %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # Drop geometry data for the table
  species_rw_sub<-species_rw %>% st_drop_geometry() 
  
  # Load our version of the PLANTS database. Remove Sci names because there are rendering issues due to odd symbols in some of the sci names. 
  NV_PLANT_sum<- read.csv("./appData/NV_PLANT_sum.csv")
  NV_PLANT_sum <- NV_PLANT_sum %>% select(-sci_name)
  
  # Load county data for the states of interest. 
  counties <- st_read("./appData/counties.shp", quiet = TRUE)
  
  # Transform county spatial data to match the site coordinates 
  counties$geometry<-st_transform(counties$geometry, 4326)
  
  # List the county/state where the site is located
  intersect<-unlist(st_intersects(species_rw$geometry, counties$geometry))
  
  # Clean up
  species_rw$county<-counties[intersect,]$NAME
  species_rw$state<-counties[intersect,]$STATE_NAME
  colnames(species_rw) <- c("EvaluationID","PLANT_code", "state_full","geometry", "county", "state")
  
  # Join that lists the plant code again if it is located in the county
  species_rw$present <-  species_rw %>%
    left_join(NV_PLANT_sum, by=c("PLANT_code", "county"), keep=TRUE) %>%
    select(PLANT_code.y) %>%
    st_drop_geometry() %>%
    unlist()
  
  # Creates a new column with True or False by comparing columns.
  species_rw$status<- species_rw$present %in% species_rw$PLANT_code
  
  # Drop geometry for table and make a data frame.
  final<-species_rw %>% st_drop_geometry()
  data.frame(final)
}