library(tigris)
library(sf)
library(here)

counties <- tigris::counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)

st_write(counties, here("appData/counties.shp"))
#dat <- st_read(here("appData/counties.shp"))
