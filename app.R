library(bslib)
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(DT)
library(reactable)
library(leaflet)
library(sf)
library(readxl)
library(shinyWidgets)
library(markdown)
library(here)
shiny::addResourcePath(prefix = "www", directoryPath = here::here("www"))



# Define UI --------------------------------------------------------------------
ui <-  page_navbar(
  title = "PLANT Database Check",
  selected = "1. Upload",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  # Page 1 - upload raw data
  nav_panel(
    title = "1. Upload",
    page_sidebar(
      # Sidebar
      sidebar = sidebar(
        fileInput(
          "speciesFile",
          "Upload Species File",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsm"
            )
          ),
        p("Note: The app can process ~10 plots per second so please be patient when uploading larger or many files.",
          style = "font-size: 12px;"),
        ),
      # Main page
      leafletOutput("plant_map")
      ),
    ),
  
  # Page 2 - compare plant list with PLANTS database
  nav_panel(
    title = "2. Inspect",
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
        pickerInput(inputId = "point_picker",
                    label = "Select point(s):",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE))
        ),
      # Main Page
      reactableOutput("checkTable")
      )
    ),
  nav_panel(
    title = "Help",
    card(
      includeMarkdown("./appData/plant_check_help.md")
    )
  )
  ) # end ui

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {

  
  ## Global --------------------------------------------------------------------
  # Allow uploads up to 200MB. Only useful for many MIM files.
  options(shiny.maxRequestSize = 200*1024^2)
  
  # Counties shapefile
  counties_gpkg <- st_read("./appData/counties.gpkg", quiet = TRUE) %>%
    st_transform(crs = 4326)
  
  # PlantDB
  all_states_plant_db <- read.csv("./appData/All_PLANT_sum.csv") %>% select(-sci_name)
  
  # Adjacent county info
  #adjCounties <- reactive({read.csv("./appData/adjacent_counties_working.csv")})
  adjacentCounties <- read.csv("./appData/adjacent_counties.csv")

  
  ## Page 1 --------------------------------------------------------------------
  
  # Get file type and point id column name for the uploaded file.
  # Used for filtering, tables, and maps.
  fileMetaData <- reactiveVal()
  
  observeEvent(input$speciesFile$datapath,{
    fileMetaData(identifyFileMetadata(input$speciesFile$datapath))
  })

  # Run check on uploaded data
  checked_data <- reactive({
    req(input$speciesFile)
    
    chkdat <- switch(fileMetaData()$fileType,
                     rw = rw_check(speciesFile = input$speciesFile$datapath,
                                   plotLocations = "./appData/R&WAIM_2024_Plots_0.csv",
                                   counties = counties_gpkg,
                                   plantDB = all_states_plant_db,
                                   adjCounties = adjacentCounties),
                     terrestrial = terr_check(speciesFile = input$speciesFile$datapath,
                                              counties = counties_gpkg,
                                              plantDB = all_states_plant_db,
                                              adjCounties = adjacentCounties),
                     lotic = lotic_check(speciesFile = map_dfr(input$speciesFile$datapath, get_single_MIM_data),
                                         plotLocations = "./appData/lotic_aim_points_2021-2024.csv",
                                         counties = counties_gpkg,
                                         plantDB = all_states_plant_db,
                                         adjCounties = adjacentCounties)
                     )
    
    chkdat
  })
  
  # Map to show points - just basemap here
  output$plant_map <- renderLeaflet({
    plant_check_map()
  })
  
  # Add points to map once data is uploaded.
  observeEvent(checked_data(),{
    req(nrow(checked_data()) > 0)
    switch(fileMetaData()$fileType,
           rw = plant_check_map_proxy(mapId = "plant_map", data = checked_data(), idName = "PlotID"),
           lotic = plant_check_map_proxy(mapId = "plant_map", data = checked_data(), idName = "PointID"),
           terrestrial = plant_check_map_proxy(mapId = "plant_map", data = checked_data(), idName = "Plot_ID"))
  })
  
  ## Page 2 --------------------------------------------------------------------
  # Sidebar panel for site filter
  
  # Update picker input based on available data.
  observe({
    updatePickerInput(
      session = session,
      inputId = "point_picker",
      choices = checked_data() %>% pull(fileMetaData()$ptColName) %>% unique() %>% sort(),
      #choices = as.vector(unique(checked_data()$PlotID)) %>% sort(),
      selected = NULL)
    })

  # Filter data displayed in table
  checked_data_filtered <- reactive({
    filtered_data <- checked_data()
    
    # Pickerinput filter.  !! NOTE the !!sym().  This is needed to make the string fileMetaData()$ptColName work with %in%
    filtered_data <- filtered_data %>% filter(!!sym(fileMetaData()$ptColName) %in% input$point_picker)
    
    # Include "False" matches only.  False = is NOT found in county, TRUE = is found in county.
    if (input$false==TRUE) {
      filtered_data <- subset(filtered_data, expectedInCounty == 'FALSE')
    }
    
    # Include Unknown plants or not
    if (input$unk==TRUE) {
      filtered_data <- subset(filtered_data,!grepl("[XXXX]", filtered_data$speciesCode))
    }
    
    filtered_data
  })
  
  # Render table after accounting for filters
  output$checkTable <- renderReactable({
    req(nrow(checked_data_filtered()) > 0)
    
    tableDat <- checked_data_filtered() %>% 
      st_drop_geometry()
    
    reactable(tableDat,
              pagination = FALSE, 
              highlight = TRUE, 
              compact = TRUE,
              fullWidth = FALSE,
              defaultExpanded = TRUE,
              groupBy = fileMetaData()$ptColName,
              columns = dynamicColWidths(tableDat))
  })
  
  
} # end server


# Run Shiny app ----
shinyApp(ui, server)
