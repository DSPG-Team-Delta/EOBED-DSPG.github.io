##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: mortRN_cost_Data.R
##### Author: Collin Robinson
##### Date Created: August 2, 2020
##### Last Update: August 6, 2020

# Map showing percentage of income spent by Registered Nurses on Mortgages by tract/county.

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
# addRnMortCostCounts <- function(map, df, group = NULL, fc){
#    # Set up highlight label text (HTML formatting)
#    percentLab <- paste("Percent of RN Income Spent on Mortgage: ",round(df$RN_income_mort,2),"%",sep = "")
#    amountLab <- paste("Average Rent: $", df$HOC, sep = "")
#    incAm <- paste("RN Income: $", df$RN_income)
#    #moeLab <- paste("Margin of Error on Amount Spent: ",round(df$MOE_HOC,2),"%",sep = "")
#    labels <- sprintf(
#       "<strong>%s</strong><br/>%s<br/>%s<br/>%s",df$Geographic.Area.Name,incAm,percentLab,amountLab#moeLab
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
addRnMortCostTracts <- function(map, df, group = NULL, fc){
   # Set up highlight label text (HTML formatting)
   percentLab <- paste("Percent of RN Income Spent on Mortgage: ",round(df$RN_income_mort,2),"%",sep = "")
   amountLab <- paste("Average HOC: $", df$HOC, sep = "")
   incAm <- paste("RN Income: $", df$RN_income)
   #moeLab <- paste("Margin of Error on Amount Spent: ",round(df$MOE_HOC,2),"%",sep = "")
   labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%s<br/>%s",df$Geographic.Area.Name,incAm,percentLab,amountLab#moeLab
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

plotRnMortCost <- function( tract_merge, groups = NULL, pal, myDomain, cityLng,
                           cityLat, cityNames, cityLng2, cityLat2, cityNames2, stateM){
   
   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   ownerMap<<-leaflet() %>%
      # Map provider
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      # Add yearly county and tract maps
      # addRnMortCostCounts(df = county_merge[[1]], group = groups[1], fc = ~pal(RN_income_mort)) %>%   #2010 County Data
      addRnMortCostTracts(df = tract_merge[[1]], group  = groups[1], fc = ~pal(RN_income_mort)) %>%     #2010 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[2]], group = groups[3], fc = ~pal(RN_income_mort)) %>%   #2011 County Data
      addRnMortCostTracts(df = tract_merge[[2]], group  = groups[2], fc = ~pal(RN_income_mort)) %>%     #2011 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[3]], group = groups[5], fc = ~pal(RN_income_mort)) %>%   #2012 County Data
      addRnMortCostTracts(df = tract_merge[[3]], group  = groups[3], fc = ~pal(RN_income_mort)) %>%     #2012 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[4]], group = groups[7], fc = ~pal(RN_income_mort)) %>%   #2013 County Data
      addRnMortCostTracts(df = tract_merge[[4]], group  = groups[4], fc = ~pal(RN_income_mort)) %>%     #2013 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[5]], group = groups[9], fc = ~pal(RN_income_mort)) %>%   #2014 County Data
      addRnMortCostTracts(df = tract_merge[[5]], group  = groups[5], fc = ~pal(RN_income_mort)) %>%    #2014 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[6]], group = groups[11], fc = ~pal(RN_income_mort)) %>%  #2015 County Data
      addRnMortCostTracts(df = tract_merge[[6]], group  = groups[6], fc = ~pal(RN_income_mort)) %>%    #2015 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[7]], group = groups[13], fc = ~pal(RN_income_mort)) %>%  #2016 County Data
      addRnMortCostTracts(df = tract_merge[[7]], group  = groups[7], fc = ~pal(RN_income_mort)) %>%    #2016 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[8]], group = groups[15], fc = ~pal(RN_income_mort)) %>%  #2017 County Data
      addRnMortCostTracts(df = tract_merge[[8]], group  = groups[8], fc = ~pal(RN_income_mort)) %>%    #2017 Tract Data
      
      # addRnMortCostCounts(df = county_merge[[9]], group = groups[17], fc = ~pal(RN_income_mort)) %>%  #2018 County Data
      addRnMortCostTracts(df = tract_merge[[9]], group  = groups[9], fc = ~pal(RN_income_mort)) %>%    #2018 Tract Data
      
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
                title = "Percent of RN Income Spent on Mortgage",         # Legend Title
                labFormat = labelFormat(suffix = "%")) %>%          # Append $ prefix to legend values
      
      # Allows user to change map layers
      addLayersControl(baseGroups = groups, overlayGroups = c("State Lines","Cities"),
                       position = "topleft", options = layersControlOptions(collapsed = F)) %>%
      
      setView(lng = cityLng2, lat = cityLat2+1.5, zoom = 6)
   
   # Show the map
   ownerMap
}

getMortRNCostTracts <- function(myTracts,ald=NULL){
   
   files <- list.files(path="../Housing/Cost and affordability data",pattern="*Tract_Data_Housing.csv",full.names=T)
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
      tract_merge[[i]]$RN_income_mort <- as.numeric(tract_merge[[i]]$RN_income_mort)
      tract_merge[[i]]$MOE_HOC <- as.numeric(tract_merge[[i]]$MOE_HOC)
   }
   
   return(tract_merge)
}

# getMortRNCostCounts <- function(myCounts,ald=NULL){
#    # 
#    # files <- list.files(path="./Housing Data/Cost and affordability data",pattern="*County_Data_Housing.csv",full.names=T)
#    # ald <- sapply(files, read.csv, skip = 1, header = T, simplify = F)
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
#       county_merge[[i]]$RN_income_mort <- as.numeric(county_merge[[i]]$RN_income_mort)
#       county_merge[[i]]$MOE_HOC <- as.numeric(county_merge[[i]]$MOE_HOC)
#    }
#    return(county_merge)
# }


drawRnMortCostMap <- function(tract, county){
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
   groups <- c(2010:2018)
   groups <- as.character(groups)

   ###### Funcs ######

   # Set Global Domain to Normalize all maps
   myDomain <- 0:35

   # Set Global Palette to Normalize all maps
   pal <- colorNumeric(
      palette = c("green","yellow","red"),
      domain = myDomain,
      na.color = "red" # set na color
   )

   tract_merge <- getMortRNCostTracts(myTracts,tract)


   plotRnMortCost(tract_merge = tract_merge, groups = groups, pal = pal, myDomain = myDomain, cityLng = cityLng,
                  cityLat = cityLat, cityNames = cityNames, cityLng2 = cityLng2, cityLat2 = cityLat2, cityNames2 = cityNames2, stateM = stateM)
}

drawRnMortCostMap()
