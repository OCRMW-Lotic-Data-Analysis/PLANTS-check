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
                radioButtons("workingState", "State",
                             choices = c(
                               "NV" = "NV",
                               "ID" = "ID"
                             )),
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
                tags$hr(),
                h5("Data Type:")
                # radioButtons("project", "Project",
                #              choices = c(
                #                "Lotic" = "Lotic",
                #                "Terrestrial" = "Terrestrial",
                #                "R&W" = "R&W"
                #                ),
                #              selected = 'Lotic'
                #              ),
                
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
      #dataTableOutput("siteTable")
      reactableOutput("checkTable")
      )
    )
  ) # end ui

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {

  
  ## Global --------------------------------------------------------------------
  # Allow uploads up to 200MB. Only useful for many MIM files.
  options(shiny.maxRequestSize = 200*1024^2)
  
  # Load counties shapefile
  counties_gpkg <- st_read("./appData/counties.gpkg", quiet = TRUE) %>%
    st_transform(crs = 4326)
  all_states_plant_db <- read.csv("./appData/NV_PLANT_sum.csv") %>% select(-sci_name)
  
  ## Page 1 --------------------------------------------------------------------
  
  fileType <- reactive({
    identifyFileType(input$speciesFile$datapath)})
  
  # Run check on uploaded data
  checked_data <- reactive({
    req(input$speciesFile)

    # fileType <- identifyFileType(input$speciesFile$datapath)
    # print(fileType)
 
    chkdat <- switch(fileType(),
                     rw = rw_check(speciesFile = input$speciesFile$datapath,
                                   plotLocations = "./appData/R&WAIM_2024_Plots_0.csv",
                                   counties = counties_gpkg,
                                   plantDB = all_states_plant_db,
                                   stateAbbrv = input$workingState),
                     terrestrial = "terr",
                     lotic = lotic_check(speciesFile = map_dfr(input$speciesFile$datapath, get_single_MIM_data),
                                         plotLocations = "./appData/LoticAIM_2024_Points.csv",
                                         counties = counties_gpkg,
                                         plantDB = all_states_plant_db,
                                         stateAbbrv = input$workingState)
                     )
    
    chkdat
  })
  
  # Map to show points - just basemap here
  output$plant_map <- renderLeaflet({
    plant_check_map()
  })
  
  # Add points to map once data is uploaded.
  observeEvent(checked_data(),{
    switch(fileType(),
           rw = plant_check_map_proxy_rw(mapId = "plant_map", data = checked_data()),
           lotic = plant_check_map_proxy_lotic(mapId = "plant_map", data = checked_data()))
    #plant_check_map_proxy_rw(mapId = "plant_map", data = checked_data())
  })
  
  ## Page 1 --------------------------------------------------------------------
  # Sidebar panel for site filter 
  output$choose_site <- renderUI({
    site.names <- as.vector( unique(checked_data()$PlotID ))
    selectInput(inputId = "selectPoint",
                label ="Select point:",
                choices=site.names, multiple=TRUE)
    })
 
  
  # Actual site filter 
  model.data <- reactive({
    subset(checked_data(), PlotID %in% input$selectPoint)
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
    #output$siteTable <- renderDataTable({unknown.false.model.data() })
    # output$checkTable <- renderReactable({
    #   req(unknown.false.model.data())
    #   tableDat <- unknown.false.model.data() %>% 
    #     st_drop_geometry() %>% 
    #     select(!c(present, state_full))
    #   reactable(tableDat,
    #             pagination = FALSE, 
    #             highlight = TRUE, 
    #             compact = TRUE,
    #             fullWidth = FALSE,
    #             #groupBy = "PlotID",
    #             groupBy = switch(fileType(),
    #                    rw = "PlotID",
    #                    lotic= "PointID"),
    #             columns = dynamicColWidths(tableDat))
    #             })
    
  # DOESNT ACCOUNT FOR FILTERS YET
  output$checkTable <- renderReactable({
    req(checked_data())
    tableDat <- checked_data() %>% 
      st_drop_geometry() %>% 
      select(!c(present, state_full))
    reactable(tableDat,
              pagination = FALSE, 
              highlight = TRUE, 
              compact = TRUE,
              fullWidth = FALSE,
              #groupBy = "PlotID",
              groupBy = switch(fileType(),
                               rw = "PlotID",
                               lotic= "PointID"),
              columns = dynamicColWidths(tableDat))
  })
  
  
  
} # end server


# Run Shiny app ----
shinyApp(ui, server)
