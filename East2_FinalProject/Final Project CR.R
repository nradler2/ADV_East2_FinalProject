library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

## Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load Data Sets
census <- st_read("Data/2020_CensusData/2020_CensusData.shp")
city_council_districts <- st_read("Data/City_Council_Districts/City_Council_Districts.shp")
parks <- read.csv("Data/Parks_Locations_and_Features.csv")
public_facilities <- read.csv("Data/Public_Facilities.csv")
street_lights <- read.csv("Data/Street_Lights.csv")

# Convert to spatial 
street_lights.spatial <- street_lights %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

street_lights.spatial <- st_join(street_lights.spatial, city_council_districts)

# Filter on Pole Types
pole_types <- c("Wood", "Concrete", "Fiberglass", "Metal", "Aluminum")
street_lights.spatial <- street_lights.spatial %>% 
  filter(Pole_Type %in% pole_types)

street_lights.spatial$popup <- paste("<b>", street_lights.spatial$OBJECTID, "</b><br>",
                                     "Wattage", street_lights.spatial$Wattage, "<br>",
                                     "Address", street_lights.spatial$Address, "<br>",
                                     "Type of Street Light", street_lights.spatial$Pole_Type, "<br>")

street_lights_pal <- colorFactor(palette = 'Set1', 
                                 domain = street_lights.spatial$Pole_Type)

ui <- navbarPage(
  "South Bend Dashboard",
  tabPanel(
    "Street Lights",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "StreetLightInput",
          label = "Street Light Pole Type",
          choices = unique(street_lights.spatial$Pole_Type),
          selected = unique(street_lights.spatial$Pole_Type)
        ),
        checkboxGroupInput(
          inputId = "StreetLightDistrict",
          label = "Districts",
          choices = unique(street_lights.spatial$Dist),
          selected = unique(street_lights.spatial$Dist)
        )
      ),
      mainPanel(
        leafletOutput(outputId = "StreetLightsOutput"),
        plotOutput("dist_streetlight_pole")
      )
    )
  ),
  tabPanel("Public Facilities"),
  tabPanel("Parks")
)

server <- function(input, output) {
  output$StreetLightsOutput <- renderLeaflet({
    selected_districts <- input$StreetLightDistrict[!is.na(input$StreetLightDistrict) & input$StreetLightDistrict != "NA"]
    # Create the leaflet map
    leaflet(dplyr::filter(city_council_districts, Dist %in% selected_districts)) %>%
      addPolygons(opacity = 0.5) %>%
      addTiles() %>%
      addCircleMarkers(data = dplyr::filter(street_lights.spatial, Pole_Type %in% input$StreetLightInput &
                                              Dist %in% selected_districts),
                       popup = ~popup,
                       color = ~street_lights_pal(Pole_Type),
                       stroke = 0, fillOpacity = 0.5, radius = 2)
  })

  output$dist_streetlight_pole <- renderPlot({
    # Filter out NA districts
    filtered_data <- dplyr::filter(street_lights.spatial, !is.na(Dist) & Dist %in% input$StreetLightDistrict)
    
    ggplot(filtered_data,
           aes(x = Dist, fill = Pole_Type)) +
      geom_bar() +
      theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(~Pole_Type) +
      labs(x = "District",
           y = "Type of Street Light",
           title = "Comparison of Street Light Pole Types per District") +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2")
  })
  
  # Summary of street light features by pole type
  output$dist_streetlight_pole <- renderPlot({
    # Filter out NA districts
    filtered_data <- dplyr::filter(street_lights.spatial, !is.na(Dist) & Dist %in% input$StreetLightDistrict)
    
    ggplot(filtered_data,
           aes(x = Dist, fill = Pole_Type)) +
      geom_bar() +
      theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(~Pole_Type) +
      labs(x = "District",
           y = "Number of Street Lights",
           fill = "Pole Type",
           title = "Comparison of Street Light Pole Types per District") +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2")
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)