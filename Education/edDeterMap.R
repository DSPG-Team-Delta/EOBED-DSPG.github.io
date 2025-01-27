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


# Map of Indices related to Determinants of Education Quality

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

addEdDeterShapes <- function(map, df, group = NULL, fOp = 0.6, fc, indLab, addStr, addLab){
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

readEdDeterShapes <- function(){
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

drawEdDeterMap <- function(){
   
   # City mappings for markers
   cityLng <- c(-117.2382,-116.9949,-116.9165,-116.9338,-116.5598,-116.9693,-116.8190,-116.9432,-116.6874)
   cityLat <- c(43.9821,43.8768,44.0077,44.0782,43.5788,44.2510,43.9699,43.7852,43.6629)
   cityNames <- c("Vale","Nyssa","Fruitland","Payette","Nampa","Weiser","New Plymouth","Parma","Caldwell")
   
   cityLng2 <- c(-116.9629)
   cityLat2 <- c(44.0266)
   cityNames2 <- c("Ontario")


   edDeter <- read.csv("../Education/2015_District_Data_EducationDeterminants.csv", header = T)
   edDeter$INDEX_overall <- as.numeric(edDeter$INDEX_overall)

   myDistricts <- readEdDeterShapes()

   myCounts <- counties(state = c("OR","ID"))
   edDeter$GEOID <- as.character(edDeter$GEOID)

   edDeter_merge <- geo_join(myDistricts, edDeter, "GEOID","GEOID")
   edDeter_merge$INDEX_overall <- as.integer(edDeter_merge$INDEX_overall)

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

   groups <- c("Overall Quality Index", "Teacher to Student Ratio Index", "Counselor to Student Ratio Index",
               "Share of Experienced Teachers Index", "Teacher Salaray (as share of students) Index", "Number of Title I Schools Index")

   edDeterMap <<- leaflet() %>%

      addProviderTiles("CartoDB.DarkMatter") %>%

      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[1], fc = ~pal(INDEX_overall), indLab = edDeter_merge$INDEX_overall, addStr = "Ranking Overall: ", addLab = edDeter_merge$Rank.Overall) %>%
      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[2], fc = ~pal(INDEX_STR), indLab = edDeter_merge$INDEX_STR, addStr = "Student to Teacher Ratio: ", addLab = edDeter_merge$Student.to.teacher.ratio) %>%
      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[3], fc = ~pal(INDEX_SCR), indLab = edDeter_merge$INDEX_SCR, addStr = "Student to Counselor Ratio: ", addLab = edDeter_merge$Student.to.Counselor.ratio) %>%
      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[4], fc = ~pal(INDEX_Experience), indLab = edDeter_merge$INDEX_Experience, addStr = "% 1st and 2nd Year Teachers: ", addLab = edDeter_merge$X..1st.and.2nd.Year.Teachers) %>%
      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[5], fc = ~pal(INDEX_TeacherSalary), indLab = edDeter_merge$INDEX_TeacherSalary, addStr = "Teacher Salary (pupil): ", addLab = edDeter_merge$Teacher.Salary..pupil.) %>%
      addEdDeterShapes(df = edDeter_merge, fOp = 1, group = groups[6], fc = ~pal(INDEX_Title1), indLab = edDeter_merge$INDEX_Title1, addStr = "Number of Title 1 Schools: ", addLab = edDeter_merge$Number.of.Title.1.Schools) %>%


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
                title = "Education Determinant Quality Index (2015)", # Legend Title
                na.label = "No Data",
                labels = c(1,2,3,4,5)) %>%

      addLayersControl(baseGroups = groups, overlayGroups = "Cities",
                       position = "topleft", options = layersControlOptions(collapsed = F))


   edDeterMap
}

drawEdDeterMap()
