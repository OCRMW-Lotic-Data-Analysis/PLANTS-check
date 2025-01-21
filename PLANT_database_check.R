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

# Define UI for slider demo app ----
ui <-  page_navbar(
  title = "PLANT database check",
  selected = "1. Upload raw data",
  collapsible = TRUE,
  theme = bslib::bs_theme(font_scale = NULL, preset = "yeti"),
  
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
                 
                 # Input: Select number of rows to display ----
                 radioButtons(
                   "disp",
                   "Display",
                   choices = c(
                     Head = "head",
                     All = "all"
                   ),
                   selected = "head"
                 )
               ),
               
               # Output: Data file ----
               tableOutput("contents")
               
             ),
  ),

nav_panel(
  title = "2. Compare",

  page_sidebar(
    sidebar = sidebar(
    #Sidebar
      checkboxInput(
          "false",
    "False match only", TRUE
  ),
  checkboxInput(
    "unk",
    "Remove Unknowns", TRUE
  ),
  uiOutput('choose_site')
),

# Output: Data file ----
tableOutput("siteTable")

)
)
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header
    )
    
   
  })
    final <-reactive({
      
      req(input$file1)
       req(input$file2)

       species_richness_rw <- read.csv(
         input$file1$datapath,
         header = input$header
       )

       species_richness_rw<- species_richness_rw %>%
         select(3,5) %>%
         unique()

       colnames(species_richness_rw) <- c("EvaluationID", "PLANTS_code")

 species_richness_rw_spatial<-read.csv(
   input$file2$datapath,
   header = input$header
 )

 species_richness_rw_spatial<-species_richness_rw_spatial[,c(4,5,20,21)]

 species_richness_rw_spatial<-unique(species_richness_rw_spatial)


 species_sum<-species_richness_rw %>%
   left_join(species_richness_rw_spatial, join_by(EvaluationID),multiple='first') %>%
   filter(AdminState == "NV")



 species_rw<- species_sum %>%
   st_as_sf(coords = c("x", "y"), crs = 4326)

 species_rw_sub<-species_rw %>% st_drop_geometry() 

#
NV_PLANT_sum<- read.csv("C:/Users/Matt Trentman/OneDrive - The University of Montana/Documents/EMMA/MIM/RShiny for plant check/NV_PLANT_sum.csv")
NV_PLANT_sum <- NV_PLANT_sum %>% select(-sci_name)
#

states <- tigris::counties(c("Montana","Idaho","NV", "North Dakota", "South Dakota"), cb = TRUE)


# gg <- ggplot()
# gg <- gg + geom_sf(data = states, color="black",
#                    fill="white", size=0.5)+
#   geom_sf(data = species_coord, pch = 4, color = "red")
# gg
states$geometry<-st_transform(states$geometry, 4326)

intersect<-unlist(st_intersects(species_rw$geometry, states$geometry))

species_rw$county<-states[intersect,]$NAME
species_rw$state<-states[intersect,]$STATE_NAME

colnames(species_rw) <- c("EvaluationID","PLANT_code", "state_full","geometry", "county", "state")

sample_county<-unique(species_rw$county)
species_rw$PLANT_code<-as.factor(species_rw$PLANT_code)

species_rw$present <-  species_rw %>%
  left_join(NV_PLANT_sum, by=c("PLANT_code", "county"), keep=TRUE) %>%
  select(PLANT_code.y) %>%
  st_drop_geometry() %>%
  unlist()


species_rw$status<- species_rw$present %in% species_rw$PLANT_code

final<-species_rw %>% st_drop_geometry()

data.frame(final)


    })
    page_sidebar(
      
      # Sidebar panel for inputs ----
      sidebar = sidebar(  
output$choose_site <- renderUI({
      site.names <- as.vector( unique(final()$EvaluationID ))
      selectInput(inputId = "select", 
                  label ="Select point:",
                  choices=site.names, multiple=TRUE)
    })
)
)

model.data <- reactive({
  subset(final(), EvaluationID %in% input$select)
})

false.model.data <-reactive({

 if(input$false==TRUE)
 {subset(model.data(), status == 'FALSE')}
 else(model.data())
})


unknown.false.model.data <-reactive({

  if(input$unk==TRUE)
  { subset(false.model.data(),!grepl("[XXXX]", false.model.data()$PLANT_code) )
    }
  else(false.model.data())
})

output$siteTable <- renderTable({unknown.false.model.data() })
}

# Create Shiny app ----
shinyApp(ui, server)
