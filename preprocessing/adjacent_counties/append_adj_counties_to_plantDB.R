All_PLANT_sum <- read.csv("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/appData/All_PLANT_sum.csv")
adjacentCounties <- read.csv("C:/Users/ianhe/OneDrive - The University of Montana/EMMA/analysis/PLANTS-check/preprocessing/adjacent_counties/adjacent_counties.csv")


# Check if plant is found in adjacent counties
adjCountyCheck <- function(PLANT_code, state, county, # All_PLANT_sum vars for pmap
                           plantDB = All_PLANT_sum, # df
                           adjCounties = adjacentCounties, #df
                           ...) {
  species <- PLANT_code
  st <- state
  co <- county
  neighbors <- adjCounties %>% filter(state == st, county == co)
  adjcnty <- plantDB %>%
    filter(state %in% neighbors$neighbor_state,
           county %in% neighbors$neighbor_county,
           PLANT_code == species) %>%
    pull(county) %>%
    str_subset(pattern = county, negate = TRUE)
  
  if (length(adjcnty) == 0){
    dat <- "None"
  } else {
    dat <- paste0(adjcnty, collapse = ", ")
  }

  return(dat)
}

#adjCountyCheck("EQLA", "NV", "Eureka")
foundInTheseCounties <- pmap(All_PLANT_sum, adjCountyCheck, .progress = TRUE) %>% unlist(use.names = FALSE)
finalPlantDB <- All_PLANT_sum
finalPlantDB$expectedInAdjacentCounty <- foundInTheseCounties













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