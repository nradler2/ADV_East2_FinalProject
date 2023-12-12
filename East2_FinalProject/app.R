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
library(RColorBrewer)
library(DT)

## Set Working Directory to Folder holding app file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load Data Sets
census = st_read("Data/2020_CensusData/2020_CensusData.shp")
city_council_districts = st_read("Data/City_Council_Districts/City_Council_Districts.shp")

parks = read.csv("Data/Parks_Locations_and_Features.csv")
public_facilities = read.csv("Data/Public_Facilities.csv")
street_lights = read.csv("Data/Street_Lights.csv")

##### PREPROCESS FACILITIES DATA ##### 
public_facilities.spatial = public_facilities %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

public_facilities.spatial = st_join(public_facilities.spatial, city_council_districts)

public_facilities.spatial$popup <- paste("<b>",public_facilities$POPL_NAME,"</b><br>",
                                         "Type: ", public_facilities$POPL_TYPE,"<br>",
                                         "Address: ", public_facilities$POPL_ADDR1,"<br>",
                                         "Phone Number: ", public_facilities$POPL_PHONE, "<br>")

facilities_pal = colorFactor(pal = "Dark2", #c("#d95f02", "#1b9e77", "#7570b3"), 
                             domain = public_facilities.spatial$POPL_TYPE)

districts_pal = colorFactor(pal = 'Set1', domain = city_council_districts$Dist)

##### PREPROCESS PARKS DATA ##### 
parks_spatial <- parks %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326) %>% st_join(city_council_districts)

parks_features_long <- as.data.frame(parks_spatial) %>%
  dplyr::select(Dist, Park_Name, Park_Type, Concessions,
                Event_Space, Garden__Community, Picnic_Grounds)%>%
  pivot_longer(cols=-c(Dist, Park_Name, Park_Type),
               names_to="park_feature",
               values_to="count")%>%
  group_by(Dist, Park_Type, park_feature)%>%
  summarise(count=sum(count, na.rm=TRUE))%>%
  ungroup()

parks_spatial$popup <- paste0("<b>",parks_spatial$Park_Name,"</b><br>",
                              "Type: ",parks_spatial$Park_Type,"<br>",
                              "District: ",parks_spatial$Dist,"<br>",
                              "Address: ", parks_spatial$Address,"<br>")

parks_pal <- colorFactor(palette='Dark2', 
                         domain=parks_spatial$Park_Type)

##### PREPROCESS STREETLIGHT DATA #####

street_lights.spatial <- street_lights %>%
  st_as_sf(coords = c("Lon", "Lat")) %>%
  st_set_crs(value = 4326)

street_lights.spatial <- st_join(street_lights.spatial, city_council_districts)

# Filter on Pole Types
pole_types <- c("Wood", "Concrete", "Fiberglass", "Metal", "Aluminum")

street_lights.spatial <- street_lights.spatial %>% 
  filter(Pole_Type %in% pole_types)

street_lights.spatial$popup <- paste("<b>", street_lights.spatial$OBJECTID, "</b><br>",
                                     "Wattage: ", street_lights.spatial$Wattage, "<br>",
                                     "Type of Street Light: ", street_lights.spatial$Pole_Type, "<br>")

street_lights_pal <- colorFactor(palette = 'Dark2', 
                                 domain = street_lights.spatial$Pole_Type)


# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "South Bend Mayoral Dashboard",
  
  ###### FACILITIES #####
  
  tabPanel(
    title = "Public Facilities",
    
    sidebarPanel(
      h4("Select the public facility types and districts of interest to adjust the map."),
      h5("Click on the points on the map to view more information about the facilities"),
      checkboxGroupInput(
        inputId = "facilitiesDomainInput",
        label = "Select Facilities",
        choices = unique(public_facilities.spatial$POPL_TYPE),
        selected = unique(public_facilities.spatial$POPL_TYPE)
      ), ## END PARK DOMAIN INPUT

      checkboxGroupInput(
        inputId = "DistFilterInput",
        label = "Select District",
        choices = unique(city_council_districts$Dist),
        selected = unique(city_council_districts$Dist)
      ) ## END District Input
      
    ), ## END SIDEBAR PANEL
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 wellPanel(
                   h3("South Bend Public Facilities"),
                   leafletOutput(outputId = "facilitiesMapOutput")
                   )
                 ),
        tabPanel(title = "Public Facilities By District",
                 wellPanel(
                   fluidRow(
                     h3("District-Specific Public Facilities"),
                     plotOutput("dist_public_facilities_types")
                   )
                 )
        )
        )
      ) ## END MAIN PANEL
    ), ## END TAB 1
  
  
  ##### PARKS #####
  
  tabPanel(
    title = "Parks",
    sidebarLayout(
      
      sidebarPanel(
        h4("Select the park types and districts of interest to adjust the map."),
        h5("Click on the points on the map to view more information about the park."),
        checkboxGroupInput(inputId = "parkstype_check",
                           label="Park types to show:",
                           choices=unique(parks_spatial$Park_Type),
                           selected=unique(parks_spatial$Park_Type)
                           ), ## END CHECKBOX INPUT 1
        checkboxGroupInput(inputId = "parksdist_check",
                           label="Districts:",
                           choices=unique(city_council_districts$Dist),
                           selected=unique(city_council_districts$Dist)
                           ) ## END CHECKBOX INPUT 2
        ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Plot",
                   wellPanel(
                     fluidRow(
                       h3("South Bend Parks"),
                       leafletOutput("parkmap")
                       )
                     )
                   ),
          tabPanel(title = "Parks By District",
                   wellPanel(
                     fluidRow(
                       h3("District-Specific Park Types"),
                       plotOutput("dist_park_types")
                       )
                     )
                   ),
          tabPanel(title = "Park Features by District",
                   wellPanel(
                     fluidRow(
                       h3("District-Specific Park Features"),
                       plotOutput("dist_park_features")
                       )
                     )
                   )
          ) ## END TABSET PANEL
        ) ## END MAIN PANEL
      ) ## END SIDEBAR LAYOUT
    ), ## END TAB 2
  
  
  ##### STREET LIGHTS #####
  
  tabPanel(
    title = "Street Lights",
    sidebarLayout(
      sidebarPanel(
        h4("Select the street light types and districts of interest to adjust the map."),
        h5("Click on the points on the map to view more information about the street lights"),
        checkboxGroupInput(
          inputId = "StreetLightInput",
          label = "Street Light Pole Type",
          choices = unique(street_lights.spatial$Pole_Type),
          selected = unique(street_lights.spatial$Pole_Type)
        ),
        checkboxGroupInput(
          inputId = "StreetLightDistrict",
          label = "Districts",
          choices = unique(city_council_districts$Dist),
          selected = unique(city_council_districts$Dist)
        )
      ), ## END SIDEBAR PANEL
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", 
                   wellPanel(
                     h3("South Bend Street Lights"),
                     leafletOutput(outputId = "StreetLightsOutput")
                     )
                   ),
          tabPanel("Light Pole Type by District", 
                   wellPanel(
                     h3("District-Specific Light Pole Types"),
                     plotOutput("dist_streetlight_pole")
                     )
                   )
        )
      ) ## END MAIN PANEL
    ) ## End Sidebar layout
  ) ## END TAB 3
  
) ## END UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ########## Public Facilities  ########## 
  
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
      addPolygons(opacity = 0.3,
                  color = ~districts_pal(Dist)) %>%
      addTiles() %>%
      addCircleMarkers(data = pub_facility_filter(),
                       popup = ~popup,
                       radius = 5,
                       color = ~facilities_pal(POPL_TYPE),
                       stroke = FALSE,
                       fillOpacity = 1
      ) %>%
      addLegend(title = "Public Facility Type",pal=facilities_pal, values = pub_facility_filter()$POPL_TYPE) %>%
      addLegend(title = "City Council District", pal=districts_pal, values = pub_facility_filter()$Dist)
  }) ## END DIST MAP
  
  # summary of district features
  output$dist_public_facilities_types <- renderPlot({
    ggplot(pub_facility_filter(),
           aes(x=Dist, fill=POPL_TYPE))+
      geom_bar()+
      theme_bw()+
      theme(legend.position = "none")+
      facet_wrap(~POPL_TYPE)+
      labs(x="District",
           y = "Number of Public Facilities",
           title="Comparison of Public Facilities per District")+
      coord_flip()+
      scale_fill_brewer(palette="Dark2")
  })
  
  ########## PARKS  ########## 
  
  output$parkmap <- renderLeaflet({
    
    filtered_parks_data = filter(parks_spatial,Park_Type%in%input$parkstype_check&
                                   Dist%in%input$parksdist_check)
    
    showplot<-leaflet(dplyr::filter(city_council_districts,Dist%in%input$parksdist_check)) %>%
      addPolygons(opacity=0.5, color = ~districts_pal(Dist))%>%
      addTiles() %>%
      addCircleMarkers(data = filtered_parks_data,
                       popup=~popup,
                       color=~parks_pal(Park_Type),
                       stroke=0, fillOpacity = 1, radius = 5) %>%
      addLegend(title = "Park Type",pal=parks_pal, values = filtered_parks_data$Park_Type) %>%
      addLegend(title = "City Council District", pal=districts_pal, values = filtered_parks_data$Dist)
  })
  
  # summary of district features
  output$dist_park_types <- renderPlot({
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
  
  # summary of district features
  output$dist_park_features <- renderPlot({
    ggplot(filter(parks_features_long,Dist%in%input$parksdist_check),
           aes(x=park_feature,
               fill=Park_Type,
               y=count))+
      geom_bar(stat="identity")+
      theme_bw()+
      facet_wrap(~Dist)+
      labs(x="Feature",
           y = "Number of Parks",
           fill="Park Type",
           title="Comparison of Park Features per District")+
      coord_flip()+
      scale_fill_brewer(palette="Dark2")
  })
  
  ########## STREET LIGHTS  ########## 
  
  output$StreetLightsOutput <- renderLeaflet({
    selected_districts <- input$StreetLightDistrict[!is.na(input$StreetLightDistrict) & input$StreetLightDistrict != "NA"]
    
    filtered_street_lights = filter(street_lights.spatial, Pole_Type %in% input$StreetLightInput &
                                      Dist %in% selected_districts)
    
    leaflet(dplyr::filter(city_council_districts, Dist %in% selected_districts)) %>%
      addPolygons(opacity = 0.5, color = ~districts_pal(Dist)) %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_street_lights,
                       popup = ~popup,
                       color = ~street_lights_pal(Pole_Type),
                       stroke = 0, fillOpacity = 0.4, radius = 2) %>%
      addLegend(title = "Light Pole Type", pal=street_lights_pal, values = filtered_street_lights$Pole_Type) %>%
      addLegend(title = "City Council District", pal=districts_pal, values = filtered_street_lights$Dist)
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
  
} ## END server

# Run the application 
shinyApp(ui = ui, server = server)
