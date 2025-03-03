library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(here)

# Load and clean Lotic AIM Hub data (this includes all years up to 2023)
hub <- read_csv(here("preprocessing/lotic/BLM_Natl_AIM_Lotic_Indicators_Hub_2025-03-03.csv"), 
                col_types = cols(FieldEvalDate = col_character())) %>%
  mutate(FieldEvalDate = mdy_hms(FieldEvalDate)) %>%
  filter(lubridate::year(FieldEvalDate) >= 2021) %>%
  select(PointID,AdminState = "BLM_AdminState", District, FieldOffice, x, y) %>%
  mutate(District = str_to_title(District),
         FieldOffice = str_to_title(FieldOffice))

# Load and clean 2024 data
pts <- read_csv(here("preprocessing/lotic/LoticAIM_2024_Points.csv")) %>%
  select(PointID,AdminState, District, FieldOffice, x, y) %>%
  mutate(District = str_to_title(District),
         FieldOffice = str_to_title(FieldOffice))
  
# Combine and make sure there are no dpulicate PointID rows (we only want one location per PointID)
dat <- bind_rows(hub, pts) %>%
  group_by(PointID) %>%
  slice(1) %>%
  ungroup()


write_csv(dat, here("preprocessing/lotic/lotic_aim_points_2021-2024.csv"))
