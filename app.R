library(bslib)
library(shiny)
library(dplyr)
library(readr)
library(purrr)
library(DT)
library(reactable)
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
  # Load counties shapefile
  counties_gpkg <- st_read("./appData/counties.gpkg", quiet = TRUE) %>%
    st_transform(crs = 4326)
  
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
    rw_check(speciesFile = input$speciesFile$datapath, 
             plotLocations = "appData/R&WAIM_2024_Plots_0.csv", 
             counties = counties_gpkg,
             stateAbbrv = input$workingState)
    })
 
  # Sidebar panel for reactive site filter 
  output$choose_site <- renderUI({
    site.names <- as.vector( unique(rw_checked_data()$PlotID ))
    selectInput(inputId = "selectPoint",
                label ="Select point:",
                choices=site.names, multiple=TRUE)
    })
 
  
  # Actual site filter 
  model.data <- reactive({
    subset(rw_checked_data(), PlotID %in% input$selectPoint)
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
    output$checkTable <- renderReactable({
      req(unknown.false.model.data())
      tableDat <- unknown.false.model.data() %>% 
        st_drop_geometry() %>% 
        select(!c(present, state_full))
      reactable(tableDat,
                pagination = FALSE, 
                highlight = TRUE, 
                compact = TRUE,
                fullWidth = FALSE,
                groupBy = "PlotID",
                columns = dynamicColWidths(tableDat))
                })
    
} # end server


# Run Shiny app ----
shinyApp(ui, server)
