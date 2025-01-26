library(bslib)
library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(sf)

# Define UI --------------------------------------------------------------------
ui <-  page_navbar(
  title = "PLANT database check",
  selected = "1. Upload raw data",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  # Page 1 - upload raw data
  nav_panel(title = "1. Upload raw data",
            page_sidebar(
              # Sidebar
              sidebar = sidebar(
                fileInput(
                  "speciesFile",
                  "Choose Species File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                    )
                  ),
                fileInput(
                  "plotLocations",
                  "Choose Spatial File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                    )
                  ),
                tags$hr(),
                radioButtons("project", "Project",
                             choices = c(
                               "Lotic" = "Lotic",
                               "Terrestrial" = "Terrestrial",
                               "R&W" = "R&W"
                               ),
                             selected = 'Lotic'
                             ),
                tags$hr(),
               ),
              # Main page
              #dataTableOutput("contents"),
              leafletOutput("plant_map")
             ),
            ),
  
  # Page 2 - compare plant list with PLANTS database
  nav_panel(
    title = "2. Compare",
    # Check box to filter out positive matches 
    page_sidebar(
      # Sidebar
      sidebar = sidebar(
        checkboxInput(
          "false",
          "False match only", TRUE
          ),
        # Check box to filter out unknowns
        checkboxInput(
          "unk",
          "Remove Unknowns", TRUE
          ),
        # Filter tool to select one or more sites to observe
        uiOutput('choose_site')
        ),
      # Main Page
      dataTableOutput("siteTable")
      )
    )
  )

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Global --------------------------------------------------------------------
  # Load counties shapefile
  counties_shp <- st_read("./appData/counties.shp", quiet = TRUE)
  ## Page 1 --------------------------------------------------------------------
  # Renders table on page 1
  output$contents <- renderDataTable({
    req(input$speciesFile)
    df <- read.csv(input$speciesFile$datapath)
  })
  
  # Map to show points - just basemap here
   output$plant_map <- renderLeaflet({
     plant_check_map()
   })
   
   # Add points to map once data is uploaded.
   observeEvent(rw_checked_data(),{
     plant_check_map_proxy_rw(mapId = "plant_map", data = rw_checked_data())
   })
  
  # R&W VERSION: Renders table on page 2  
  rw_checked_data <- reactive({
    req(input$speciesFile)
    req(input$plotLocations)
    rw_check(speciesFile = input$speciesFile$datapath, 
             plotLocations = input$plotLocations$datapath, 
             counties = counties_shp)
    })
 
  # Sidebar panel for reactive site filter 
  output$choose_site <- renderUI({
    site.names <- as.vector( unique(rw_checked_data()$EvaluationID ))
    selectInput(inputId = "selectPoint",
                label ="Select point:",
                choices=site.names, multiple=TRUE)
    })
 
  
  # Actual site filter 
  model.data <- reactive({
    subset(rw_checked_data(), EvaluationID %in% input$selectPoint)
    })
    
  # Filter for TRUE or FALSE status
  false.model.data <-reactive({
  if (input$false==TRUE) {
    subset(model.data(), status == 'FALSE')
    }
    else {
      model.data()
      }
    })

  # Filter for Unknowns included or not
  unknown.false.model.data <-reactive({
  if (input$unk==TRUE) {
    subset(false.model.data(),!grepl("[XXXX]", false.model.data()$PLANT_code))
    }
    else {
      false.model.data()
      }
    })

    # Render table after accounting for filters
    output$siteTable <- renderDataTable({unknown.false.model.data() })
}

# #TERRESTRIAL CODE
# 
# # Load terrestrial file
# species_richness_terrestrial <- read_csv("species_richness_terrestrial.csv")
# 
# # Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
# species_sum <-species_richness_terrestrial %>%
#   select(c(4,5,31,32,33)) %>%
#   separate_rows(c(3))
# species_coord <- species_sum %>%
#   st_as_sf(coords = c("x", "y"), crs = 4326)
# 
# # Load counties
# states <- counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)
# 
# # Transform county data to be similar to site data
# states$geometry <- st_transform(states$geometry, 4326)
# 
# # Same as above for R&W
# intersect <- unlist(st_intersects(species_coord$geometry, states$geometry))
# 
# species_coord$county <- states[intersect,]$NAME
# species_coord$state <- states[intersect,]$STATE_NAME
# 
# colnames(species_coord) <- c('Plot_ID',"Plot_Key","PLANT_code", "geometry", "county", "state")
# 
# sample_county <- unique(species_coord$county)
# species_coord$present <-  species_coord %>%
#   left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
#   select(PLANT_code.y) %>%
#   st_drop_geometry() %>%
#   unlist()
# 
# species_coord$status <- species_coord$present %in% species_coord$PLANT_code
# # 

# Create Shiny app ----
shinyApp(ui, server)
