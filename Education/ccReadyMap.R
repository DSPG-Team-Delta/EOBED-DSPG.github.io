##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: educationsGraphics.R
##### Author: Collin Robinson
##### Date Created: July 27, 2020
##### Last Update: July 31, 2020


# Map of Index related to College and Career Readiness

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

addCCRShapes <- function(map, df, group = NULL, fOp = 0.6, fc, indLab, addStr, addLab){
   # Set up highlight label text (HTML formatting)
   myLab <- paste("Index: ",indLab)
   myAddLab <- paste(addStr,addLab)
   labels <- sprintf(
      "<strong>%s, %s County</strong></br>%s</br>%s",df$NAME,df$County,myLab,myAddLab
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

readCCRShapes <- function(){
   districts <- readOGR(dsn = "../Education/shapes", layer = "schooldistrict_sy1819_tl19_EORWID")
   myDistricts <- districts[districts$STATEFP=="16"|districts$STATEFP=="41",]
   myDistricts <- myDistricts[myDistricts$GEOID=="4101020"|myDistricts$GEOID=="4101350"|myDistricts$GEOID=="4101500"|myDistricts$GEOID=="1600510"|myDistricts$GEOID=="1600570"|
                                 myDistricts$GEOID=="1600570"|myDistricts$GEOID=="1600152"|myDistricts$GEOID=="1601140"
                              |myDistricts$GEOID=="4106120"|myDistricts$GEOID=="1600161"|myDistricts$GEOID=="4106820"|myDistricts$GEOID=="4106960"|
                                 myDistricts$GEOID=="1600158"|myDistricts$GEOID=="1600012"|myDistricts$GEOID=="1602070"|myDistricts$GEOID=="1602130"|
                                 myDistricts$GEOID=="1602160"|myDistricts$GEOID=="1602340"|myDistricts$GEOID=="1602460"|
                                 myDistricts$GEOID=="4109000"|myDistricts$GEOID=="4109270"|myDistricts$GEOID=="1602550"|
                                 myDistricts$GEOID=="1602580"|myDistricts$GEOID=="4100014"|myDistricts$GEOID=="1600600"|
                                 myDistricts$GEOID=="1600003"|myDistricts$GEOID=="1600141"|myDistricts$GEOID=="1603330"|myDistricts$GEOID=="1603480",]
   return(myDistricts)
}

drawCCReadyMap <- function(){

   # City mappings for markers
   cityLng <- c(-117.2382,-116.9949,-116.9165,-116.9338,-116.5598,-116.9693,-116.8190,-116.9432,-116.6874)
   cityLat <- c(43.9821,43.8768,44.0077,44.0782,43.5788,44.2510,43.9699,43.7852,43.6629)
   cityNames <- c("Vale","Nyssa","Fruitland","Payette","Nampa","Weiser","New Plymouth","Parma","Caldwell")
   
   cityLng2 <- c(-116.9629)
   cityLat2 <- c(44.0266)
   cityNames2 <- c("Ontario")


   ccr <<- read.csv("../Education/2015_District_Data_CCReady.csv", header = T)
   ccr$INDEX_Overall <- as.numeric(ccr$INDEX_Overall)

   myDistricts <- readCCRShapes()

   myCounts <- counties(state = c("OR","ID"))
   ccr$GEOID <- as.character(ccr$GEOID)

   ccr_merge <- geo_join(myDistricts, ccr, "GEOID","GEOID")
   ccr_merge$INDEX_Overall <- as.integer(ccr_merge$INDEX_Overall)
   ccr_merge$AP_index <- as.integer(ccr_merge$AP_index)
   ccr_merge$Gifted_index <- as.integer(ccr_merge$Gifted_index)
   ccr_merge$Dual_index <- as.integer(ccr_merge$Dual_index)
   ccr_merge$IB_index <- as.integer(ccr_merge$IB_index)
   ccr_merge$Calc_index <- as.integer(ccr_merge$Calc_index)
   ccr_merge$Chem_index <- as.integer(ccr_merge$Chem_index)
   ccr_merge$Physics_index <- as.integer(ccr_merge$Physics_index)
   

   myDomain <- as.integer(c(1,2,3,4,5))

   pal <- colorBin(
      palette = c("springgreen4","palegreen","yellow","orange","red"),
      domain = myDomain,
      bins = 5,
      pretty = F
      )

   stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)

   # Custom Marker Icon
   circle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                            iconWidth = 12, iconHeight = 12)
   
   groups <- c("College and Career Readiness", "AP Classes", "Gifted and Talented Classes", "Dual Enrollment/Credit Recovery",
               "IB Classes", "Calculus I", "Chemistry", "Physics")

   ccrMap <<- leaflet() %>%

      addProviderTiles("CartoDB.DarkMatter") %>%

      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[1], fc = ~pal(INDEX_Overall), indLab = ccr_merge$INDEX_Overall, addStr = "Ranking Overall: ", addLab = ccr_merge$Overall.Ranking) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[2], fc = ~pal(AP_index), indLab = ccr_merge$AP_index, addStr = "School that offers AP classes?: ", addLab = ccr_merge$School.that.offers.AP.classes.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[3], fc = ~pal(Gifted_index), indLab = ccr_merge$Gifted_index, addStr = "School that offers gifted and talented education?: ", addLab = ccr_merge$School.that.offers.gifted.and.talented.education.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[4], fc = ~pal(Dual_index), indLab = ccr_merge$Dual_index, addStr = "School that offers dual enrollment/credit recovery?: ", addLab = ccr_merge$School.that.offers.dual.enrollment.credit.recovery.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[5], fc = ~pal(IB_index), indLab = ccr_merge$IB_index, addStr = "School that offers IB curriculum?: ", addLab = ccr_merge$School.that.offers.IB.curriculum.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[6], fc = ~pal(Calc_index), indLab = ccr_merge$Calc_index, addStr = "School that offers Calculus 1?: ", addLab = ccr_merge$School.offers.Calculus.1.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[7], fc = ~pal(Chem_index), indLab = ccr_merge$Chem_index, addStr = "School that offers Chemistry?: ", addLab = ccr_merge$School.offers.Chemistry.) %>%
      addCCRShapes(df = ccr_merge, fOp = 1, group = groups[8], fc = ~pal(Physics_index), indLab = ccr_merge$Physics_index, addStr = "School that offers Physics?: ", addLab = ccr_merge$School.offers.Physics.) %>%
      
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
                 group = "Cities",
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "bottom")) %>%

      # Ontario city marker
      addMarkers(lng=cityLng2[1],
                 lat=cityLat2[1],
                 label=cityNames2[1],
                 icon = circle_black,
                 group = "Cities",
                 labelOptions = labelOptions(noHide = T,
                                             textsize = "12px",
                                             direction = "top")) %>%

      # Legend
      addLegend(values = myDomain,                        # Same domain for legend
                opacity = 1,                              # Make legend opaque
                colors = c("green","palegreen","yellow","orange","red"),
                position = "bottomright",                 # Legend at bottom right
                title = "College Career Readiness Index (2015)", # Legend Title
                na.label = "No Data",
                labels = c(1,2,3,4,5)) %>%
      
      addLayersControl(baseGroups = groups, overlayGroups = "Cities", 
                       position = "topleft", options = layersControlOptions(collapsed = F))


   ccrMap
}

drawCCReadyMap()
