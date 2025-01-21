library("dplyr")                                                 
library("readr")   
library("readxl") 
library('fuzzyjoin')
library('tidyr')
library(tigris)
library(ggplot2)
library(sf)
library(tidyverse)
options(tigris_use_cache = TRUE)
options(digits = 15)

##Load all county level data from the PLANTS database and combine
plant.data <- list.files(path = "C:/Users/matthew.trentman/OneDrive - The University of Montana/Documents/EMMA/MIM/RShiny for plant check/NV list",     
                       pattern = "*.csv", 
                       full.names = TRUE) %>%  
  lapply(read_csv) %>%                                            
  bind_rows                                                       

#clean
colnames(plant.data) <- c('state','county', 'PLANT_code', 'synonym','sci_name', 'common_name')
plant.data$PLANT_code<- as.factor(plant.data$PLANT_code)
plant.data$county<- as.factor(plant.data$county)


##Summarize BLM FO and DO by counties
BLM_admin <- read_excel("NV_BLMofficetocounty.xlsx")


countytoBLM<-BLM_admin %>% 
  group_by(NAME) %>% 
  summarize(BLM_FO = paste(sort(unique(ADMU_NAME)),collapse=", "),
            BLM_DO = paste(sort(unique(PARENT_NAME)),collapse=", "))

countytoBLM[5,1]<-"Elko"
countytoBLM[16,1]<-"Washoe"
countytoBLM[15,1]<-"Storey"
countytoBLM[11,1]<-"Lyon"
countytoBLM[13,1]<-"Nye"
countytoBLM[7,1]<-"Eureka"



colnames(countytoBLM) <- c('county',"BLM_FO","BLM_DO")


#Merge BLM FO and DO by counties in NV_plant_sum

NV_plant_sum <-plant.data %>% stringdist_join(countytoBLM, 
                                              by = "county",
                                              mode = "left", 
                                              ignore_case = TRUE, 
                                              method="soundex"
                                            )


  
colnames(NV_plant_sum)<-c("state","county", "PLANT_code", "synonym", "sci_name", "common_name", "county_full", "BLM_FO", "BLM_DO")

write.csv(NV_plant_sum, "./NV_PLANT_sum.csv", row.names = F)


######


NV_PLANT_sum <- read_csv("NV_PLANT_sum.csv")


##Load AIM data
#Terrestrial

species_richness_terrestrial <- read_csv("species_richness_terrestrial.csv")


species_sum<-species_richness_terrestrial %>% 
  select(c(4,5,31,32,33)) %>%
  separate_rows(c(3))
species_coord<- species_sum %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) 


states <- counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)


gg <- ggplot()
gg <- gg + geom_sf(data = states, color="black",
                   fill="white", size=0.5)+
  geom_sf(data = species_coord, pch = 4, color = "red")
gg
states$geometry<-st_transform(states$geometry, 4326)

intersect<-unlist(st_intersects(species_coord$geometry, states$geometry))

species_coord$county<-states[intersect,]$NAME
species_coord$state<-states[intersect,]$STATE_NAME

colnames(species_coord) <- c('Plot_ID',"Plot_Key","PLANT_code", "geometry", "county", "state")

sample_county<-unique(species_coord$county)
species_coord$present <-  species_coord %>%
 left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
  select(PLANT_code.y) %>%
  st_drop_geometry() %>%
  unlist()

species_coord$status<- species_coord$present %in% species_coord$PLANT_code



##Load AIM data
#R&W

species_richness_rw <- read_csv("species_richness_rw.csv")

species_richness_rw<- species_richness_rw %>%
  select(3,5) %>%
  unique()

colnames(species_richness_rw) <- c("EvaluationID", "PLANTS_code")

species_richness_rw_spatial<-read_csv("species_richness_rw_spatial.csv")


species_richness_rw_spatial<-species_richness_rw_spatial[,c(4,5,20,21)]

species_richness_rw_spatial<-unique(species_richness_rw_spatial)


species_sum<-species_richness_rw %>%
left_join(species_richness_rw_spatial, join_by(EvaluationID),multiple='first') %>%
  filter(AdminState == "NV")


species_rw<- species_sum %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) 


states <- counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)


gg <- ggplot()
gg <- gg + geom_sf(data = states, color="black",
                   fill="white", size=0.5)+
  geom_sf(data = species_coord, pch = 4, color = "red")
gg
states$geometry<-st_transform(states$geometry, 4326)

intersect<-unlist(st_intersects(species_coord$geometry, states$geometry))

species_coord$county<-states[intersect,]$NAME
species_coord$state<-states[intersect,]$STATE_NAME

colnames(species_coord) <- c("EvaluationID","PLANT_code", "state_full","geometry", "county", "state")

sample_county<-unique(species_coord$county)
species_coord$PLANT_code<-as.factor(species_coord$PLANT_code)

species_coord$present <-  species_coord %>%
  left_join(NV_PLANT_sum, by=c("PLANT_code", "county"), keep=TRUE) %>%
  select(PLANT_code.y) %>%
  st_drop_geometry() %>%
  unlist()

test<-species_coord %>%
  left_join(NV_PLANT_sum, by=c("PLANT_code", "county"))

species_coord$status<- species_coord$present %in% species_coord$PLANT_code


 
