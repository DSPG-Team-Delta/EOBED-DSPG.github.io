##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: healthQualGraphics.R
##### Author: Collin Robinson
##### Date Created: July 28, 2020
##### Last Update: July 31, 2020

# Map showing Indices related to Healthcare Quality in the Eastern Oregon
# Border Region.

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tigris)
library(acs)
library(stringr)
library(rgdal)
library(sp)
library(leaflet) 
library(rappdirs)
library(sf)
library(maptools)

addHealthQualShapes <- function(map, df, group = NULL, fOp = 0.6, fc, inLab){
   # Set up highlight label text (HTML formatting)
   myLab <- paste("Index: ",inLab)
   labels <- sprintf(
      "<strong>%s</strong></br>%s",df$NAME.y,myLab
   )
   # Add Shapes to map
   addPolygons(map = map,
               data = df,
               fillColor = fc,                         # Gradient fill based on domain
               color = "#b2aeae",                      # Gray border line(must be hex)
               fillOpacity = fOp,                      # Transparency (.8 for big map, .3 for zoomed)
               weight = 1,                             # Thickness of borders
               smoothFactor = 0.2,
               group = group,
               highlightOptions = highlightOptions(    # Mouse-over popup
                  weight = 5,
                  color = "#666666",
                  fillOpacity = .8,
                  sendToBack = T),
               label = lapply(labels,htmltools::HTML), # HTML labels for formatting purposes
               labelOptions = labelOptions(
                  textsize = "13px",
                  direction = "auto"
               ))
}

drawhealthQualMap <- function(){
   
   # City mappings for markers
   cityLng <- c(-117.2382,-116.9949,-116.9165,-116.9338,-116.5598,-116.9693,-116.8190,-116.9432,-116.6874)
   cityLat <- c(43.9821,43.8768,44.0077,44.0782,43.5788,44.2510,43.9699,43.7852,43.6629)
   cityNames <- c("Vale","Nyssa","Fruitland","Payette","Nampa","Weiser","New Plymouth","Parma","Caldwell")
   
   cityLng2 <- c(-116.9629)
   cityLat2 <- c(44.0266)
   cityNames2 <- c("Ontario")
   
   # Get Malheur Tract Shapes
   OrTract <- tracts(state = "OR", county = 045)
   
   # Get Payette, Washington, and Canyon County Tract Shapes
   IdTract <- tracts(state = "ID", county = c(27,75,87))
   
   # Combine local tract shape info
   myTracts <- rbind(IdTract, OrTract)
   
   healthQual <- read.csv("../Healthcare/Healthcare_Outcomes_and_Quality/2018_Tract_Data_HealthQual.csv", header = T)
   healthQual$INDEX_Overall <- as.numeric(healthQual$INDEX_Overall)
   
   healthQual$id_Fix <- as.character(healthQual$id_Fix)
   
   healthQual_merge <<- geo_join(myTracts, healthQual, "GEOID","id_Fix")
   healthQual_merge$INDEX_Overall <- as.integer(healthQual_merge$INDEX_Overall)
   healthQual_merge$INDEX_qual <- as.integer(healthQual_merge$INDEX_qual)
   healthQual_merge$INDEX_readminrate <- as.integer(healthQual_merge$INDEX_readminrate)
   healthQual_merge$INDEX_LifeExpect <- as.integer(healthQual_merge$INDEX_LifeExpect)
   healthQual_merge$INDEX_InfantMort <- as.integer(healthQual_merge$INDEX_InfantMort)
   
   
   myDomain <- as.integer(c(1,2,3,4,5))
   
   pal <- colorBin(
      palette = c("springgreen4","palegreen","yellow","orange","red"),
      domain = myDomain,
      bins = 5,
      pretty = F
   )
   
   myCounts <- counties(state = c("OR","ID"))
   stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
   
   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   groups <- c("Healthcare Outcomes and Quality Index", "Quality of Care Ratings", "30 Day Readmission Rate Index",
               "Life Expectancy Index", "Infant Mortality Rate Index")
   
   healthQualMap <<- leaflet() %>%
      
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      addHealthQualShapes(df = healthQual_merge, fOp = 1, group = groups[1], fc = ~pal(INDEX_Overall), healthQual_merge$INDEX_Overall) %>%
      addHealthQualShapes(df = healthQual_merge, fOp = 1, group = groups[2], fc = ~pal(INDEX_qual), healthQual_merge$INDEX_qual) %>%
      addHealthQualShapes(df = healthQual_merge, fOp = 1, group = groups[3], fc = ~pal(INDEX_readminrate), healthQual_merge$INDEX_readminrate) %>%
      addHealthQualShapes(df = healthQual_merge, fOp = 1, group = groups[4], fc = ~pal(INDEX_LifeExpect), healthQual_merge$INDEX_LifeExpect) %>%
      addHealthQualShapes(df = healthQual_merge, fOp = 1, group = groups[5], fc = ~pal(INDEX_InfantMort), healthQual_merge$INDEX_InfantMort) %>%
      
      
      # Adds the state borders inside your map, put as the last added Polygon
      # so it is drawn on top of the other shapes. You can play with the weight
      # to change the border thickness and the color to change border color.
      addPolylines(data = stateM,
                   color = "#000000",                     # Black border line(must be hex)
                   fillOpacity = 0,                       # Make tracts opaque to differentiate
                   opacity = 1,                           # Make tract borders opaque
                   weight = 3,                            # Thickness of borders
                   smoothFactor = 0) %>%
      
      # Adds the county borders inside your map, put as the last added Polygon
      # so it is drawn on top of the other shapes. You can play with the weight
      # to change the border thickness and the color to change border color.
      addPolylines(data = myCounts,
                   color = "#000000",                        # Gray border line(must be hex)
                   fillOpacity = 0,                       # Make tracts opaque to differentiate
                   opacity = 1,                           # Make tract borders opaque
                   weight = 1,                            # Thickness of borders
                   smoothFactor = 0) %>%
      
      # Nyssa, Vale, Boise city markers
      addMarkers(lng=cityLng,
                 lat=cityLat,
                 label=cityNames,
                 icon = circle_black,
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "bottom")) %>%
      
      # Ontario city marker
      addMarkers(lng=cityLng2[1],
                 lat=cityLat2[1],
                 label=cityNames2[1],
                 icon = circle_black,
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "top")) %>%
      
      # Legend 
      addLegend(values = myDomain,                        # Same domain for legend
                opacity = 1,                              # Make legend opaque
                colors = c("green","palegreen","yellow","orange","red"),
                position = "bottomright",                 # Legend at bottom right
                title = "Health Determinants Index (2018)", # Legend Title
                na.label = "No Data",
                labels = c(1,2,3,4,5)) %>%
      
      addLayersControl(baseGroups = groups, 
                       position = "topleft", options = layersControlOptions(collapsed = F))
   
   
   healthQualMap
}

drawhealthQualMap()