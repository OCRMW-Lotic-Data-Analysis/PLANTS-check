# Join that lists the plant code again if it is located in the county
dat <- species %>%
  left_join(plantDB, by=join_by("speciesCode" == "PLANT_code", "county", "state"), keep = TRUE) %>%
  mutate(expectedInCounty = case_when(is.na(PLANT_code) ~ FALSE,
                                      speciesCode == PLANT_code ~ TRUE)) %>%
  select(Plot_ID, 
         Plot_Key, 
         speciesCode, 
         speciesCode, 
         state = state.x, 
         county = county.x, 
         expectedInCounty)


a <- species$present %in% species$speciesCode
length(a) - sum(a, na.rm = TRUE) # num false

b <- species$present == species$speciesCode
sum(is.na(b), na.rm = TRUE)


dat2 <- species %>% mutate(expected = case_when(speciesCode == present ~ TRUE,
                                                is.na(present) ~ FALSE)
)
