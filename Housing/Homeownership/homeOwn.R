##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: homeOwn.R
##### Author: Collin Robinson
##### Date Created: July 28, 2020
##### Last Update: July 31, 2020

# Annual Map showing percentage of population that owns housing from 2010-2018

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
# addOwnerCounties <- function(map, df, group = NULL, fc){
#    # Set up highlight label text (HTML formatting)
#    miLab <- paste("Share of Population that Owns Homes: ",round(df$percent_Own,2),"%",sep = "")
#    moeLab <- paste("Margin of Error: ",round(df$Own_MOE,2),"%",sep = "")
#    labels <- sprintf(
#       "<strong>%s</strong><br/>%s<br/>%s</br>",df$Geographic.Area.Name,miLab,moeLab
#    )
#    # Add Shapes to map
#    addPolygons(map = map,
#                data = df,
#                fillColor = fc,                         # Gradient fill based on domain
#                color = "#b2aeae",                      # Gray border line(must be hex)
#                fillOpacity = 0.8,                      # Transparency (.8 for big map, .3 for zoomed)
#                weight = 1,                             # Thickness of borders
#                smoothFactor = 0.2,
#                group = group,
#                highlightOptions = highlightOptions(    # Mouse-over popup
#                   weight = 5,
#                   color = "#666666",
#                   fillOpacity = .8,
#                   bringToFront = T,
#                   sendToBack = T),
#                label = lapply(labels,htmltools::HTML), # HTML labels for formatting purposes
#                labelOptions = labelOptions(
#                   textsize = "13px",
#                   direction = "auto"
#                ))
# }

# Add tracts to map
addOwnerTracts <- function(map, df, group = NULL, fc){
   # Set up highlight label text (HTML formatting)
   miLab <- paste("Share of Population that Owns Homes: ",round(df$percent_Own,2),"%",sep = "")
   moeLab <- paste("Margin of Error: ",round(df$Own_MOE,2),"%",sep = "")
   labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%s</br>",df$Geographic.Area.Name,miLab,moeLab
   )
   # Add Shapes to map
   addPolygons(map = map,
               data = df,
               fillColor = fc,                         # Gradient fill based on domain
               color = "#000000",                      # Black border line(must be hex)
               fillOpacity = 1,                        # Make tracts opaque to differentiate
               opacity = 1,                            # Make tract borders opaque
               weight = 1,                             # Thickness of borders
               smoothFactor = 0,
               group = group,
               highlightOptions = highlightOptions(    # Mouse-over popup
                  weight = 5,
                  color = "#666666",
                  fillOpacity = .8,
                  bringToFront = T,
                  sendToBack = T),
               label = lapply(labels,htmltools::HTML), # HTML labels for formatting purposes
               labelOptions = labelOptions(
                  textsize = "13px",
                  direction = "auto"))
}

plotHomeOwn <- function( tract_merge, groups = groups, pal, myDomain, cityLng,
                      cityLat, cityNames, cityLng2, cityLat2, cityNames2, stateM){
   
   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   ownerMap<<-leaflet() %>%
      # Map provider
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      # Add yearly county and tract maps
      # addOwnerCounties(df = county_merge[[1]], group = groups[1], fc = ~pal(percent_Own)) %>%   #2010 County Data
      addOwnerTracts(df = tract_merge[[1]], group  = groups[1], fc = ~pal(percent_Own)) %>%     #2010 Tract Data
      
      # addOwnerCounties(df = county_merge[[2]], group = groups[3], fc = ~pal(percent_Own)) %>%   #2011 County Data
      addOwnerTracts(df = tract_merge[[2]], group  = groups[2], fc = ~pal(percent_Own)) %>%     #2011 Tract Data
      
      # addOwnerCounties(df = county_merge[[3]], group = groups[5], fc = ~pal(percent_Own)) %>%   #2012 County Data
      addOwnerTracts(df = tract_merge[[3]], group  = groups[3], fc = ~pal(percent_Own)) %>%     #2012 Tract Data
      
      # addOwnerCounties(df = county_merge[[4]], group = groups[7], fc = ~pal(percent_Own)) %>%   #2013 County Data
      addOwnerTracts(df = tract_merge[[4]], group  = groups[4], fc = ~pal(percent_Own)) %>%     #2013 Tract Data
      
      # addOwnerCounties(df = county_merge[[5]], group = groups[9], fc = ~pal(percent_Own)) %>%   #2014 County Data
      addOwnerTracts(df = tract_merge[[5]], group  = groups[5], fc = ~pal(percent_Own)) %>%    #2014 Tract Data
      
      # addOwnerCounties(df = county_merge[[6]], group = groups[11], fc = ~pal(percent_Own)) %>%  #2015 County Data
      addOwnerTracts(df = tract_merge[[6]], group  = groups[6], fc = ~pal(percent_Own)) %>%    #2015 Tract Data
      
      # addOwnerCounties(df = county_merge[[7]], group = groups[13], fc = ~pal(percent_Own)) %>%  #2016 County Data
      addOwnerTracts(df = tract_merge[[7]], group  = groups[7], fc = ~pal(percent_Own)) %>%    #2016 Tract Data
      
      # addOwnerCounties(df = county_merge[[8]], group = groups[15], fc = ~pal(percent_Own)) %>%  #2017 County Data
      addOwnerTracts(df = tract_merge[[8]], group  = groups[8], fc = ~pal(percent_Own)) %>%    #2017 Tract Data
      
      # addOwnerCounties(df = county_merge[[9]], group = groups[17], fc = ~pal(percent_Own)) %>%  #2018 County Data
      addOwnerTracts(df = tract_merge[[9]], group  = groups[9], fc = ~pal(percent_Own)) %>%    #2018 Tract Data
      
      # Nyssa, Vale, Boise city markers
      addMarkers(lng=cityLng,
                 lat=cityLat,
                 label=cityNames,
                 icon = circle_black,
                 group = "Cities",
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "bottom")) %>%
      
      # Ontario city marker (Needs own Marker to point textbox in different direction)
      addMarkers(lng=cityLng2,
                 lat=cityLat2,
                 label=cityNames2,
                 icon = circle_black,
                 group = "Cities",
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "top")) %>%
      
      # Adds the state borders inside your map, put as the last added Polygon
      # so it is drawn on top of the other shapes. You can play with the weight
      # to change the border thickness and the color to change border color.
      addPolylines(data = stateM,
                   color = "#0000ff",                     # Black border line(must be hex)
                   fillOpacity = 0,                       # Make tracts opaque to differentiate
                   opacity = 1,                           # Make tract borders opaque
                   weight = 5,                            # Thickness of borders
                   smoothFactor = 0,
                   group = "State Lines") %>%
      
      # Global Legend
      addLegend(pal = pal,                                          # Use given color palette
                values = myDomain,                                  # Same domain for legend
                opacity = 1,                                        # Make legend opaque
                position = "bottomright",                           # Legend at bottom right
                title = "Share of Population that Owns Homes",         # Legend Title
                labFormat = labelFormat(suffix = "%")) %>%          # Append $ prefix to legend values
      
      # Allows user to change map layers
      addLayersControl(baseGroups = groups, overlayGroups = c("State Lines","Cities"),
                       position = "topleft", options = layersControlOptions(collapsed = F)) %>%
      
      setView(lng = cityLng2, lat = cityLat2+1.5, zoom = 6)
   
   # Show the map
   ownerMap
}

getOwnerTracts <- function(myTracts){
   
   files <- list.files(path="../Housing/Homeownership",pattern="*Tract_Data_HousingOwnership.csv",full.names=T)
   ald <- sapply(files, read.csv, skip = 1, header = T, simplify = F)

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
      tract_merge[[i]]$percent_Own <- as.numeric(tract_merge[[i]]$percent_Own)
      tract_merge[[i]]$Own_MOE <- as.numeric(tract_merge[[i]]$Own_MOE)
   }
   
   return(tract_merge)
}

# getOwnerCounts <- function(myCounts){
#    
#    files <- list.files(path="Housing Data/Homeownership",pattern="*County_Data_HousingOwnership.csv",full.names=T)
#    ald <- sapply(files, read.csv, skip = 1, header = T, simplify = F)
#    
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
#       county_merge[[i]]$percent_Own <- as.numeric(county_merge[[i]]$percent_Own)
#       county_merge[[i]]$Own_MOE <- as.numeric(county_merge[[i]]$Own_MOE)
#    }
#    return(county_merge)
# }


drawOwnerMap <- function(tract, county){
   ###### Set Variables ######

   ### GLOBAL VARS COMMENTED OUT ###
   # City locations for Markers
   cityLng <- c(-117.2382,-116.9949,-116.9165,-116.9338,-116.5598,-116.9693,-116.8190,-116.9432,-116.6874)
   cityLat <- c(43.9821,43.8768,44.0077,44.0782,43.5788,44.2510,43.9699,43.7852,43.6629)
   cityNames <- c("Vale","Nyssa","Fruitland","Payette","Nampa","Weiser","New Plymouth","Parma","Caldwell")
   
   cityLng2 <- c(-116.9629)
   cityLat2 <- c(44.0266)
   cityNames2 <- c("Ontario")

   # Get Tract shape data for Malheur
   OrTract <- tracts(state = "OR", county = 045)

   # Get Payette, Washington, and Canyon County Tract Shapes
   IdTract <- tracts(state = "ID", county = c(27,75,87))

   # Get ID and OR county shape data
   myCounts <- counties(state = c("OR","ID"))

   # Combine local tract shape info
   myTracts <- rbind(IdTract, OrTract)

   # Aggregates the county data for drawing state lines.
   stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

   # Group Names for Map Choices
   # groups <- c("2010 County","2010 Tract",
   #             "2011 County","2011 Tract",
   #             "2012 County","2012 Tract",
   #             "2013 County","2013 Tract",
   #             "2014 County","2014 Tract",
   #             "2015 County","2015 Tract",
   #             "2016 County","2016 Tract",
   #             "2017 County","2017 Tract",
   #             "2018 County","2018 Tract")
   groups <- c(2010:2018)
   groups <- as.character(groups)
   
   ###### Funcs ######
  
    # Set Global Domain to Normalize all maps
   myDomain <- 0:100
   
   # Set Global Palette to Normalize all maps
   pal <- colorNumeric(
      palette = c("red","yellow","green"),
      domain = myDomain,
      na.color = "gray" # set na color
   )
   
   # county_merge <- getOwnerCounts(myCounts)
   tract_merge <- getOwnerTracts(myTracts)
   
   
   plotHomeOwn(tract_merge = tract_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
             cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
}

drawOwnerMap()
