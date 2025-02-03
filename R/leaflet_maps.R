### Maps -----------------------------------------------------------------------
# Initial map with just basemaps and general settings.
plant_check_map <- function() {
  leaflet(
    options = leafletOptions(
      attributionControl=FALSE,
      maxZoom = 16)
  ) %>%
    setView(-113, 41, zoom = 5) %>%
    addTiles() %>%
    addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
    addProviderTiles("Esri.WorldImagery"  , group = "Esri.WorldImagery") %>%
    addProviderTiles("Esri.WorldTopoMap"  , group = "Esri.WorldTopoMap") %>%
    addProviderTiles("USGS.USTopo"        , group = "USGS.USTopo") %>%
    addProviderTiles("USGS.USImagery"     , group = "USGS.USImagery") %>%
    addProviderTiles("USGS.USImageryTopo" , group = "USGS.USImageryTopo") %>%
    addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "Esri.WorldImagery",
                                    "Esri.WorldTopoMap", "USGS.USTopo",
                                    "USGS.USImagery", "USGS.USImageryTopo"), 
                     position = "topleft")

}


# Proxy map - Adds data to map once uploaded
plant_check_map_proxy <- function(mapId, data, idName){
  
  # Convert to correct crs and only keep one point per PointID/PlotID. Otherwise, it plots
  # many overlapping points with one point per PointID/species code combo.  Renders faster this way.
  data <- data %>% st_transform(crs = 4326) %>%
    distinct(!!sym(idName), .keep_all = TRUE)
  
  labs <- data %>% pull(idName)
  
  leafletProxy(mapId, data = data) %>%
    clearMarkers() %>%
    addCircleMarkers(data = data,
                     radius = 4,
                     color = "white",
                     fillColor = "blue",
                     stroke = TRUE,
                     weight = 1,
                     fillOpacity = 1,
                     label = labs
    )
}


