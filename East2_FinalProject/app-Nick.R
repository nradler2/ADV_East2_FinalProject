#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(shinythemes)
library(tidyverse)

## Set Working Directory to Folder holding app file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load Data Sets
census = st_read("Data/2020_CensusData/2020_CensusData.shp")
city_council_districts = st_read("Data/City_Council_Districts/City_Council_Districts.shp")

parks = read.csv("Data/Parks_Locations_and_Features.csv")
public_facilities = read.csv("Data/Public_Facilities.csv")
street_lights = read.csv("Data/Street_Lights.csv")

public_facilities.spatial = public_facilities %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

public_facilities.spatial = st_join(public_facilities.spatial, city_council_districts)

public_facilities.spatial$popup <- paste("<b>",public_facilities$POPL_NAME,"</b><br>",
                                 "Type:", public_facilities$POPL_TYPE,"<br>",
                                 "Address:", public_facilities$POPL_ADDR1,"<br>",
                                 "Phone Number:", public_facilities$POPL_PHONE, "<br>")

facilities_pal <- colorFactor(pal = c("#d95f02", "#1b9e77", "#7570b3"), 
                              domain = public_facilities.spatial$POPL_TYPE)



# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "South Bend Mayoral Dashboard",
  tabPanel(
    title = "Public Facilities",
    sidebarPanel(
      checkboxGroupInput(
        inputId = "facilitiesDomainInput",
        label = "Select Facilities",
        choices = unique(public_facilities.spatial$POPL_TYPE),
        selected = unique(public_facilities.spatial$POPL_TYPE)
      ), ## END PARK DOMAIN INPUT
      checkboxGroupInput(
        inputId = "DistFilterInput",
        label = "Select District",
        choices = unique(public_facilities.spatial$Dist),
        selected = unique(public_facilities.spatial$Dist)
      ) ## END District Input
    ), ## END SIDEBAR PANEL
    mainPanel(
      leafletOutput(
        outputId = "facilitiesMapOutput"
      )## END LEAFLET OUTPUT
    ) ## END MAIN PANEL
  ), ## END TAB 1
  tabPanel(
    title = "Parks"
  ), ## END TAB 2
  tabPanel(
    title = "Street Lights"
  ) ## END TAB 3
) ## END UI


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pub_facility_filter = reactive({
    req(input$facilitiesDomainInput)
    req(input$DistFilterInput)
    filter(public_facilities.spatial, POPL_TYPE %in% input$facilitiesDomainInput) %>%
      filter(POPL_CITY == 'South Bend') %>%
      filter(Dist %in% input$DistFilterInput)
  })
  
  city_council_districts_filtered = reactive({
    req(input$DistFilterInput)
    filter(city_council_districts, Dist %in% input$DistFilterInput)
  })
  
  # create leaflet map
  output$facilitiesMapOutput = renderLeaflet({
    leaflet(city_council_districts_filtered()) %>%
      addPolygons(opacity = 0.3) %>%
      addTiles() %>%
      addCircleMarkers(data = pub_facility_filter(),
                       popup = ~popup,
                       radius = 5,
                       color = ~facilities_pal(POPL_TYPE),
                       stroke = FALSE,
                       fillOpacity = 1
                       ) %>%
      addLegend(pal=facilities_pal, values = pub_facility_filter()$POPL_TYPE)
  }) ## END DIST MAP

} ## END SERVER


# Run the application 
shinyApp(ui = ui, server = server)