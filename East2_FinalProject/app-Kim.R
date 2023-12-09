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
library(ggplot2)
library(RColorBrewer)

## Set Working Directory to Folder holding app file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load Data Sets
census = st_read("Data/2020_CensusData/2020_CensusData.shp")
city_council_districts = st_read("Data/City_Council_Districts/City_Council_Districts.shp")
parks = read.csv("Data/Parks_Locations_and_Features.csv")
public_facilities = read.csv("Data/Public_Facilities.csv")
street_lights = read.csv("Data/Street_Lights.csv")


# convert parks dataset to spatial + city council districts
parks_spatial <- parks %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326) %>% st_join(city_council_districts)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  navbarPage("South Bend Civic Data Overview",
             tabPanel("Parks",
                      sidebarLayout(
                        sidebarPanel(
                          h5("Select the park types and districts of interest to adjust the map."),
                          h5("Click on the points on the map to view more information about the park."),
                          checkboxGroupInput(inputId = "parkstype_check",
                                             label="Park types to show:",
                                             choices=unique(parks_spatial$Park_Type),
                                             selected=unique(parks_spatial$Park_Type)),
                          checkboxGroupInput(inputId = "parksdist_check",
                                             label="Districts:",
                                             choices=unique(parks_spatial$Dist),
                                             selected=unique(parks_spatial$Dist))),
                        
                        mainPanel(
                          wellPanel(fluidRow(h3("South Bend Parks"),
                                             leafletOutput("parkmap"))),
                          wellPanel(fluidRow(h3("District-Specific Park Features"),
                                             plotOutput("dist_park_features")))
                        )
                      )),
             tabPanel("Public Facilities"),
             tabPanel("Street Lights")
  )
)

server <- function(input, output) {

  
  parks_spatial$popup <- paste0("<b>",parks_spatial$Park_Name,"</b><br>",
                                "Type: ",parks_spatial$Park_Type,"<br>",
                                "District: ",parks_spatial$Dist,"<br>",
                                parks_spatial$Address,"<br>")
  
  parks_pal <- colorFactor(palette='Dark2', 
                           domain=parks_spatial$Park_Type)
  
  # update map based on checkbox selections for park type
    output$parkmap <- renderLeaflet({

        showplot<-leaflet(dplyr::filter(city_council_districts,Dist%in%input$parksdist_check)) %>%
          addPolygons(opacity=0.5)%>%
          addTiles() %>%
          addCircleMarkers(data = dplyr::filter(parks_spatial,Park_Type%in%input$parkstype_check&
                                                  Dist%in%input$parksdist_check),
                           popup=~popup,
                           color=~parks_pal(Park_Type),
                           stroke=0, fillOpacity = 1, radius = 5)
    })
    
    # summary of district features
    output$dist_park_features <- renderPlot({
      ggplot(dplyr::filter(parks_spatial,Dist%in%input$parksdist_check),
             aes(x=Dist, fill=Park_Type))+
        geom_bar()+
        theme_bw()+
        theme(legend.position = "none")+
        facet_wrap(~Park_Type)+
        labs(x="District",
             y = "Number of Parks",
             title="Comparison of Park Types per District")+
        coord_flip()+
        scale_fill_brewer(palette="Dark2")
    })
}
    
  # Run the application 
shinyApp(ui = ui, server = server)