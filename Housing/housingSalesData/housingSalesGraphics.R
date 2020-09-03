##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: housingSalesGraphics.R
##### Author: Collin Robinson
##### Date Created: July 12, 2020
##### Last Update: July 15, 2020

# Graphs median sale values over time and a count of how many houses are sold
# in Malheur and Payette counties.


library(ggplot2)
library(dplyr)
library(tidyverse)

####### New Home Sales Data #######
totSales <- read.csv("numHomeSales2.csv",header = T)
p1 <- ggplot() + 
   geom_bar(stat="identity",
            aes(x=totSales$ï..Year,y=totSales$NewHomes,fill=totSales$Area),
            position = position_dodge()) +
   
   ggtitle("New Home Sales") + 
   
   theme(plot.title = element_text(size=40,hjust=.5),
         legend.title = element_text(size=15,hjust = .5),        # Format the legend title
         legend.text = element_text(size=13),                    # Format legend text
         axis.title = element_text(size=15),                     # Format axis titles
         axis.text = element_text(size=15,color=),               # Format axis tick marks
         panel.grid.major.x = element_blank(),                   # Get rid of major gridlines on x-axis
         panel.grid.minor.y = element_line(size=.1),             # Reduce minor gridlines on y-axis
         panel.background = element_rect("lightsteelblue1")      # Format background color
   ) + 
   
   labs(x = "Year", y = "Number of Sales",fill = "Area")

p1

####### Median Home Sale $ Data #######

amountSales <- read.csv("saleAmounts.csv", header = T)

#### Small Average ####
smallAvg <- ggplot() +
   geom_bar(stat = "identity",
            aes(x = amountSales$ï..Year, y = amountSales$sfSmAvg, fill = amountSales$Area),
            position = position_dodge()) +
   
   ggtitle(label = "Single Family Less than One Acre", subtitle = "Average Selling Price") +
   
   ylim(0,250000) + 
   
   theme(plot.title = element_text(size=40,hjust=.5),
         plot.subtitle = element_text(size=25,hjust=.5),
         legend.title = element_text(size=17,hjust = .5),        # Format the legend title
         legend.text = element_text(size=15),                    # Format legend text
         axis.title = element_text(size=20),                     # Format axis titles
         axis.text = element_text(size = 20),                    # Format axis text
         panel.grid.major.x = element_blank(),                   # Get rid of major gridlines on x-axis
         panel.grid.minor.y = element_line(size=.1),             # Reduce minor gridlines on y-axis
         panel.background = element_rect("lightsteelblue1")      # Format background color
   ) + 
   
   labs(x = "Year", y = "Sale Amount ($)", fill = "Area")

#### Small Median ####
smallMedi <- ggplot() +
   geom_bar(stat = "identity",
            aes(x = amountSales$ï..Year, y = amountSales$sfSmMedian, fill = amountSales$Area),
            position = position_dodge()) +
   
   ggtitle(label = "Single Family Less than One Acre", subtitle = "Median Selling Price") +
   
   ylim(0,250000) + 
   
   theme(plot.title = element_text(size=40,hjust=.5),
         plot.subtitle = element_text(size=25,hjust=.5),
         legend.title = element_text(size=17,hjust = .5),        # Format the legend title
         legend.text = element_text(size=15),                    # Format legend text
         axis.title = element_text(size=20),                     # Format axis titles
         axis.text = element_text(size = 20),                    # Format axis text
         panel.grid.major.x = element_blank(),                   # Get rid of major gridlines on x-axis
         panel.grid.minor.y = element_line(size=.1),             # Reduce minor gridlines on y-axis
         panel.background = element_rect("lightsteelblue1")      # Format background color
   ) + 
   
   labs(x = "Year", y = "Sale Amount ($)", fill = "Area")

#### Large Average ####

lgAvg <- ggplot() +
   geom_bar(stat = "identity",
            aes(x = amountSales$ï..Year, y = amountSales$sfLgAvg, fill = amountSales$Area),
            position = position_dodge()) +
   
   ggtitle(label = "Single Family More than One Acre", subtitle = "Average Selling Price") +
   
   ylim(0,250000) + 
   
   theme(plot.title = element_text(size=40,hjust=.5),
         plot.subtitle = element_text(size=25,hjust=.5),
         legend.title = element_text(size=17,hjust = .5),        # Format the legend title
         legend.text = element_text(size=15),                    # Format legend text
         axis.title = element_text(size=20),                     # Format axis titles
         axis.text = element_text(size = 20),                    # Format axis text
         panel.grid.major.x = element_blank(),                   # Get rid of major gridlines on x-axis
         panel.grid.minor.y = element_line(size=.1),             # Reduce minor gridlines on y-axis
         panel.background = element_rect("lightsteelblue1")      # Format background color
   ) + 
   
   labs(x = "Year", y = "Sale Amount ($)", fill = "Area")

#### Large Median ####

lgMedi <- ggplot() +
   geom_bar(stat = "identity",
            aes(x = amountSales$ï..Year, y = amountSales$sfLgMedian, fill = amountSales$Area),
            position = position_dodge()) +
   
   ggtitle(label = "Single Family More than One Acre", subtitle = "Median Selling Price") +
   
   ylim(0,250000) + 
   
   theme(plot.title = element_text(size=40,hjust=.5),
         plot.subtitle = element_text(size=25,hjust=.5),
         legend.title = element_text(size=17,hjust = .5),        # Format the legend title
         legend.text = element_text(size=15),                    # Format legend text
         axis.title = element_text(size=20),                     # Format axis titles
         axis.text = element_text(size = 20),                    # Format axis text
         panel.grid.major.x = element_blank(),                   # Get rid of major gridlines on x-axis
         panel.grid.minor.y = element_line(size=.1),             # Reduce minor gridlines on y-axis
         panel.background = element_rect("lightsteelblue1")      # Format background color
   ) + 
   
   labs(x = "Year", y = "Sale Amount ($)", fill = "Area")


###### Plotting ######

# Show Plots
smallAvg
smallMedi
lgAvg
lgMedi

# Save Plots
png("../Plots/Housing_Sales/NewHomeSales.png",width = 1920, height = 1080)
plot(p1)
dev.off()

png("../Plots/Housing_Sales/sfLgAvg.png",width = 1920, height = 1080)
plot(lgAvg)
dev.off()

png("../Plots/Housing_Sales/sfSmAvg.png",width = 1920, height = 1080)
plot(smallAvg)
dev.off()

png("../Plots/Housing_Sales/sfLgMedian.png",width = 1920, height = 1080)
plot(lgMedi)
dev.off()

png("../Plots/Housing_Sales/sfSmMedian.png",width = 1920, height = 1080)
plot(smallMedi)
dev.off()
