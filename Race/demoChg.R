##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: demoChg.R
##### Author: Collin Robinson
##### Date Created: July 28, 2020
##### Last Update: July 31, 2020

# Map showing the demographic changes in the Eastern Oregon Border Region
# from 2010 to 2018, broken down into groups.

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

addDemogShapes <- function(map, df, group = NULL, fOp = 0.6, fc, inLab){
   # Set up highlight label text (HTML formatting)
   myLab <- paste("Index: ",round(inLab,2), "%")
   labels <- sprintf(
      "<strong>%s</strong></br>%s",df$Geographic.Area.Name,myLab
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

drawDemoChgMap <- function(){
   
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
   
   demoChg <- read.csv("../Race/Tract_Data_Race.csv", header = T, skip = 1)

   demoChg$id_Fix <- as.character(demoChg$id_Fix)
   
   orig_Data <- geo_join(myTracts, demoChg, "GEOID","id_Fix")
   
   demoChg_merge <- orig_Data
   
   demoChg_merge$change_nonwhite[demoChg_merge$change_nonwhite > 200] <- 200
   demoChg_merge$change_white[demoChg_merge$change_white > 200] <- 200
   demoChg_merge$change_latino[demoChg_merge$change_latino > 200] <- 200
   demoChg_merge$change_black[demoChg_merge$change_black > 200] <- 200
   demoChg_merge$change_native[demoChg_merge$change_native > 200] <- 200
   demoChg_merge$change_AAPI[demoChg_merge$change_AAPI > 200] <- 200
   

   myDomain <- -100:200
   
   pal <- colorNumeric(
      palette = c("red","yellow","green"),
      domain = myDomain
   )
   
   myCounts <- counties(state = c("OR","ID"))
   stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
   
   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   groups <- c("Change in Non-White Population", "Change in White Population", "Change in Latino Population",
               "Change in Black Population", "Change in Native American Population", "Change in Asian/Pacific Islander Population")
   
   demoChgMap <<- leaflet() %>%
      
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[1], fc = ~pal(change_nonwhite), demoChg_merge$change_nonwhite) %>%
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[2], fc = ~pal(change_white), demoChg_merge$change_white) %>%
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[3], fc = ~pal(change_latino), demoChg_merge$change_latino) %>%
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[4], fc = ~pal(change_black), demoChg_merge$change_black) %>%
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[5], fc = ~pal(change_native), demoChg_merge$change_native) %>%
      addDemogShapes(df = demoChg_merge, fOp = 1, group = groups[6], fc = ~pal(change_AAPI), demoChg_merge$change_AAPI) %>%
      
      
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
      addLegend(pal = pal,
                labFormat = labelFormat(suffix = "%"),
                values = myDomain,                        # Same domain for legend
                opacity = 1,                              # Make legend opaque
                position = "bottomright",                 # Legend at bottom right
                title = "Demographic Change from 2010-2018", # Legend Title
                na.label = "No Data") %>%
      
      addLayersControl(baseGroups = groups, 
                       position = "topleft", options = layersControlOptions(collapsed = F))
   
   
   demoChgMap
}

drawDemoChgMap()