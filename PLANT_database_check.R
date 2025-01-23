library(dplyr)
library(bslib)
library(shiny)
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(ggiraph)
library(DT)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyWidgets)
library(rhandsontable)
library(reactable)
library(htmltools)
library(zip)

# Define UI 
ui <-  page_navbar(
  title = "PLANT database check",
  selected = "1. Upload raw data",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
  # Page 1- upload raw data
  bslib::nav_panel( title = "1. Upload raw data",
             page_sidebar(
               
               # Sidebar panel for inputs ----
               sidebar = sidebar(
                 
                 # Input: Select a file ----
                 fileInput(
                   "file1",
                   "Choose Species File",
                   multiple = TRUE,
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"
                   )
                 ),
                 
                 # Input: Select a file ----
                 fileInput(
                   "file2",
                   "Choose Spatial File",
                   multiple = TRUE,
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"
                   )
                 ),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Check box if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 
                 
                 # Input: Select project ----
                 radioButtons(
                   "project",
                   "Project",
                   choices = c(
                     "Lotic" = "Lotic",
                     "Terrestrial" = "Terrestrial",
                     "R&W" = "R&W"
                   ),
                   selected = 'Lotic'
                 ),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
              ),
               
               # Output: Data file ----
               tableOutput("contents")
               
             ),
  ),

# Page 2- compare plant list with PLANTS database

nav_panel(
  title = "2. Compare",
  
  # Check box to filter out positive matches 
  page_sidebar(
    sidebar = sidebar(
    #Sidebar
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

# Output: Data file ----
tableOutput("siteTable")

)
)
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Renders table on page 1----
  output$contents <- renderTable({
      req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header
    )
    
   
  })
  # R&W VERSION: Renders table on page 2----  
  final <-reactive({
      
    # Require both uploaded files---- 
      req(input$file1)
       req(input$file2)
       
       # Load species richness data---- 
       species_richness_rw <- read.csv(
         input$file1$datapath,
         header = input$header
       )
       
       # Filter species richness data to only include evaluation ID and species codes----
       species_richness_rw<- species_richness_rw %>%
         select(3,5) %>%
         unique()
       
       # Clean up column names----
       colnames(species_richness_rw) <- c("EvaluationID", "PLANTS_code")
 
       # Load data file with spatial data----
       species_richness_rw_spatial<-read.csv(
       input$file2$datapath,
       header = input$header)
       
       # Filter spatial data file to include Plot ID (4), Evaluation ID (5), X and Y (20 and 21)----
       species_richness_rw_spatial<-species_richness_rw_spatial[,c(4,5,20,21)]
       
       # Remove duplicates
       species_richness_rw_spatial<-unique(species_richness_rw_spatial)

       # Join spatial data and species richness data by evaluationID. There are many coordinates with R&W data so this just grabs the first match.
       #NOTE: currently filter to NV but that will need to be updated with the radio button we talked about to filter by state.
       species_sum<-species_richness_rw %>%
         left_join(species_richness_rw_spatial, join_by(EvaluationID),multiple='first') %>%
         filter(AdminState == "NV")

       # Format spatial data
        species_rw<- species_sum %>%
        st_as_sf(coords = c("x", "y"), crs = 4326)

        # Drop geometry data for the table
        species_rw_sub<-species_rw %>% st_drop_geometry() 

        # Load our version of the PLANTS database. Remove Sci names because there are rendering issues due to odd symbols in some of the sci names. 
        NV_PLANT_sum<- read.csv("~/GitHub/PLANTS-check/NV_PLANT_sum.csv")
        NV_PLANT_sum <- NV_PLANT_sum %>% select(-sci_name)

        # Load county data for the states of interest. 
        states <- tigris::counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)

        # Transform county spatial data to match the site coordinates 
        states$geometry<-st_transform(states$geometry, 4326)

        # List the county/state where the site is located
        intersect<-unlist(st_intersects(species_rw$geometry, states$geometry))
        
        # Clean up
        species_rw$county<-states[intersect,]$NAME
        species_rw$state<-states[intersect,]$STATE_NAME
        colnames(species_rw) <- c("EvaluationID","PLANT_code", "state_full","geometry", "county", "state")

        # Join that lists the plant code again if it is located in the county
        species_rw$present <-  species_rw %>%
          left_join(NV_PLANT_sum, by=c("PLANT_code", "county"), keep=TRUE) %>%
          select(PLANT_code.y) %>%
          st_drop_geometry() %>%
          unlist()

        # Creates a new column with True or False by comparing columns.
        species_rw$status<- species_rw$present %in% species_rw$PLANT_code

        # Drop geometry for table and make a data frame.
        final<-species_rw %>% st_drop_geometry()
        data.frame(final)


    })
    page_sidebar(
      
      # Sidebar panel for reactive site filter ----
      sidebar = sidebar(  
      output$choose_site <- renderUI({
      site.names <- as.vector( unique(final()$EvaluationID ))
      selectInput(inputId = "select", 
                  label ="Select point:",
                  choices=site.names, multiple=TRUE)
    })
)
)
    # Actual site filter ----
    model.data <- reactive({
    subset(final(), EvaluationID %in% input$select)
  })
    
    # Filter for TRUE or FALSE status ----
    false.model.data <-reactive({

     if(input$false==TRUE)
     {subset(model.data(), status == 'FALSE')}
     else(model.data())
    })

    # Filter for Unknowns included or not ----
    unknown.false.model.data <-reactive({
    
    if(input$unk==TRUE)
      { subset(false.model.data(),!grepl("[XXXX]", false.model.data()$PLANT_code) )
        }
      else(false.model.data())
    })

    # Render table after accounting for filters
    output$siteTable <- renderTable({unknown.false.model.data() })
}

# #TERRESTRIAL CODE
# 
# # Load terrestrial file
# species_richness_terrestrial <- read_csv("species_richness_terrestrial.csv")
# 
# # Grab necessary data, turn into long form (e.g., separate rows), then format X, Y to be spatial
# species_sum<-species_richness_terrestrial %>%
#   select(c(4,5,31,32,33)) %>%
#   separate_rows(c(3))
# species_coord<- species_sum %>%
#   st_as_sf(coords = c("x", "y"), crs = 4326)
# 
# # Load counties
# states <- counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)
# 
# # Transform county data to be similar to site data
# states$geometry<-st_transform(states$geometry, 4326)
# 
# # Same as above for R&W
# intersect<-unlist(st_intersects(species_coord$geometry, states$geometry))
# 
# species_coord$county<-states[intersect,]$NAME
# species_coord$state<-states[intersect,]$STATE_NAME
# 
# colnames(species_coord) <- c('Plot_ID',"Plot_Key","PLANT_code", "geometry", "county", "state")
# 
# sample_county<-unique(species_coord$county)
# species_coord$present <-  species_coord %>%
#   left_join(filter(NV_PLANT_sum, county==sample_county), join_by(PLANT_code), keep=TRUE) %>%
#   select(PLANT_code.y) %>%
#   st_drop_geometry() %>%
#   unlist()
# 
# species_coord$status<- species_coord$present %in% species_coord$PLANT_code
# # 



# Create Shiny app ----
shinyApp(ui, server)
