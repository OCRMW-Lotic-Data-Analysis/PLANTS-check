library(here)
library(readr)
library(dplyr)

counties <- tigris::counties(cb = TRUE)
all_states_plant_db <- read.csv("./appData/All_PLANT_sum.csv") %>% select(-sci_name)

all <- read_csv(here("test_and_dev/adjacent_counties.csv")) %>%
  filter(state %in% c("ID","NV","MT","WY"))



home <- all %>% filter(state %in% c("MT", "ID", "NV", "WY"))
neigh <- all %>% filter(!(neighbor_state %in% c("MT", "ID", "NV", "WY")))

adjCounties <- bind_rows(home, neigh)
write_csv(adjCounties, here("test_and_dev/adjacent_counties_working.csv"))

pos_sf <- inner_join(counties, adjCounties, by = join_by("NAME" == "neighbor_county", "STUSPS" == "neighbor_state"))

plot(pos_sf["NAMELSAD"])



checkTable <- tribble(
  ~PointID, ~speciesCode, ~state, ~county, ~expectedInCounty,
  "NB-SS-1122", "CAUT", "NV", "Eureka", "false",
  "NB-SS-1122", "EQLA", "NV", "Eureka", "true"
)

adjCountyCheck <- function(speciesCode, state, county, ...) {
  species <- speciesCode
  st <- state
  co <- county
  neighbors <- adjCounties %>% filter(state == st, county == co)
  adjcnty <- all_states_plant_db %>% rename(speciesCode = "PLANT_code") %>%
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
#adjCountyCheck("EQLA", "NV", "Eureka")

checkTable$expectedInAdjacentCounty <- pmap(checkTable, adjCountyCheck) %>% unlist(use.names = FALSE)

