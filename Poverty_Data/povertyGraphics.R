##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: povertyGraphics.R
##### Author: Collin Robinson
##### Date Created: July 15, 2020
##### Last Update: July 27, 2020


# Heatmap for poverty at the county level in both ID and OR, and at the 
# tract level for Malheur County, OR and Canyon, Payette, and Washington Counties
# in ID.

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


# Add counties to map
addShapes <- function(map, df, group = NULL, fOp = 0.6, fc){
   # Set up highlight label text (HTML formatting)
   miLab <- paste("Percent in Poverty: ",df$povPercent,"%",sep = "")
   moeLab <- paste("Margin of Error: ",as.numeric(df$povMOE),"%",sep = "")
   labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%s</br>",df$Geographic.Area.Name,miLab,moeLab
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
                  bringToFront = T),
               label = lapply(labels,htmltools::HTML), # HTML labels for formatting purposes
               labelOptions = labelOptions(
                  textsize = "13px",
                  direction = "auto"
               ))
}

getTracts <- function(ald, myTracts){
   # Create empty lists for data sets
   tractData <- vector(mode = "list", length = length(ald))
   tract_merge <- vector(mode = "list", length = length(ald))
   
   for(i in 1:length(ald)){
      # Separate all data into county and tract level data
      tractData[[i]] <- ald[[i]][ald[[i]]$id_Fix > 1000000,]
      
      # Change id's to char vecs for geo_join
      tractData[[i]]$id_Fix <- as.character(tractData[[i]]$id_Fix) # makes type same for geo_join
      
      # Merge the shape data with the census data
      tract_merge[[i]] <- geo_join(myTracts, tractData[[i]], "GEOID", "id_Fix")
      
      # Make usable data points numeric rather than chars
      tract_merge[[i]]$povPercent <- as.numeric(tract_merge[[i]]$povPercent)
   }
   
   return(tract_merge)
}

# getCounts <- function(ald, myCounts){
#    countyData <- vector(mode = "list", length = length(ald))
#    county_merge <- vector(mode = "list", length = length(ald))
#    
#    for(i in 1:length(ald)){
#       # Separate all data into county level data
#       countyData[[i]] <- ald[[i]][ald[[i]]$id_Fix > 100 & ald[[i]]$id_Fix < 1000000,]
#       
#       # Change id's to char vecs for geo_join
#       countyData[[i]]$id_Fix <- as.character(countyData[[i]]$id_Fix) # makes type same for geo_join
#       
#       # Merge the shape data with the census data
#       county_merge[[i]] <- geo_join(myCounts, countyData[[i]], "GEOID", "id_Fix")
#       
#       # Make usable data points numeric rather than chars
#       county_merge[[i]]$povPercent <- as.numeric(county_merge[[i]]$povPercent)
#    }
#    return(county_merge)
# }

#readFiles <- function(){
readPovertyFiles <- function(){
   #files <- list.files(path = "./", pattern = "201._Poverty.csv",full.names = T)
   files <- list.files(path = "../Poverty_Data", pattern = "201._Poverty.csv",full.names = T)

   ald <- sapply(files, read.csv, skip = 1, header = T, simplify = F)
   return(ald)
}


### GLOBAL VARS COMMENTED OUT ###
# # City mappings for markers
cityLng <- c(-117.2382,-116.9949,-116.9165,-116.9338,-116.5598,-116.9693,-116.8190,-116.9432,-116.6874)
cityLat <- c(43.9821,43.8768,44.0077,44.0782,43.5788,44.2510,43.9699,43.7852,43.6629)
cityNames <- c("Vale","Nyssa","Fruitland","Payette","Nampa","Weiser","New Plymouth","Parma","Caldwell")

cityLng2 <- c(-116.9629)
cityLat2 <- c(44.0266)
cityNames2 <- c("Ontario")

########## Tract Data ##########

# Get Tract data for Malheur
OrTract <- tracts(state = "OR", county = 045)
# Get Tract data for Canyon, Payetted, and Washington
IdTract <- tracts(state = "ID", county = c(27,75,87))

# Combine all local tracts
myTracts <- rbind(OrTract,IdTract)

########## County Data ##########
myCounts <- counties(state = c("OR","ID"))

# Function for drawing map. Calls all other functions
drawPovMap <- function(){
   myDomain <- c(2:50)
   # Color Palette (Gradient)
   pal <- colorBin(
      palette = c("green","yellow","red"),
      domain = myDomain,
      bins = 8,
      pretty = F
   )
   
   ald <- readPovertyFiles()
   tract_merge <- getTracts(ald,myTracts)
   # county_merge <- getCounts(ald,myCounts)
   
   # add this in above your map, change the myCounts variable to whichever var you use to store the county-level geocodes
   stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
   
   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   groups <- c(2012:2018)
   groups <- as.character(groups)
   
   
   povertyMap <<- leaflet() %>%
      # Map provider
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      # # County Layers
      # addShapes(df = county_merge[[1]],group = groups[1], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[2]],group = groups[2], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[3]],group = groups[3], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[4]],group = groups[4], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[5]],group = groups[5], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[6]],group = groups[6], fOp = 0.8, fc = ~pal(povPercent)) %>%
      # addShapes(df = county_merge[[7]],group = groups[7], fOp = 0.8, fc = ~pal(povPercent)) %>%
      
      
      # Tract Layers
      addShapes(df = tract_merge[[1]],group = groups[1], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[2]],group = groups[2], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[3]],group = groups[3], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[4]],group = groups[4], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[5]],group = groups[5], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[6]],group = groups[6], fOp = 1, fc = ~pal(povPercent)) %>%
      addShapes(df = tract_merge[[7]],group = groups[7], fOp = 1, fc = ~pal(povPercent)) %>%
      
      
      # Adds the state borders inside your map, put as the last added Polygon
      # so it is drawn on top of the other shapes. You can play with the weight
      # to change the border thickness and the color to change border color.
      addPolylines(data = stateM,
                  color = "#000000",                     # Black border line(must be hex)
                  fillOpacity = 0,                       # Make tracts opaque to differentiate
                  opacity = 1,                           # Make tract borders opaque
                  weight = 3,                            # Thickness of borders
                  smoothFactor = 0) %>%
      
      # # Comment out markers for big map
      # # Nyssa, Vale, Boise city markers
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
      addLegend(pal = pal,                               # Use given color palette
                values = myDomain,             # Same domain for legend
                opacity = 1,                             # Make legend opaque
                position = "bottomright",                # Legend at bottom right
                title = "Percent Below Poverty Level",   # Legend Title
                na.label = "No Data",                    # Label for NA vals
                labFormat = labelFormat(suffix = "%")) %>%   # Append $ prefix to legend values
      
      addLayersControl(baseGroups = groups, 
                       position = "topleft", options = layersControlOptions(collapsed = F))
   
   # Show the map
   povertyMap
}

drawPovMap()
