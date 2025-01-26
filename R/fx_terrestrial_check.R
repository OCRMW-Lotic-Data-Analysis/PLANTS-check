#TERRESTRIAL CODE - 

# !!! NOT WORKING AS EXTERNAL FUNCTION YET
terr_check <- function(speciesFile, plotLocations, counties){
# Load terrestrial file
species_richness_terrestrial <- read_csv("species_richness_terrestrial.csv")

# Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
species_sum <-species_richness_terrestrial %>%
  select(c(4,5,31,32,33)) %>%
  separate_rows(c(3))
species_coord <- species_sum %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Load counties
states <- counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)

# Transform county data to be similar to site data
states$geometry <- st_transform(states$geometry, 4326)

# Same as above for R&W
intersect <- unlist(st_intersects(species_coord$geometry, states$geometry))

species_coord$county <- states[intersect,]$NAME
species_coord$state <- states[intersect,]$STATE_NAME

colnames(species_coord) <- c('Plot_ID',"Plot_Key","PLANT_code", "geometry", "county", "state")

sample_county <- unique(species_coord$county)
species_coord$present <-  species_coord %>%
  left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
  select(PLANT_code.y) %>%
  st_drop_geometry() %>%
  unlist()

species_coord$status <- species_coord$present %in% species_coord$PLANT_code

}