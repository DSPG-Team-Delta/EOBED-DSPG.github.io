##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: unemploymentGraphs.R
##### Author: Collin Robinson
##### Date Created: July 12, 2020
##### Last Update: July 15, 2020

# Graphs the unemployment data for all Oregon counties and Idaho counties
# neighboring Malheur. Source code requires directories OR_unemploymentData/ and 
# ID_unemploymentData/ to exist in current working directory with source Excel 
# files in these directories.Also requires Malheur OR Unemployment.xlsx be in current
# working directory. Path names can be changed to fit current directory hierarchy.


library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)

# Make a line based on Unemployment Data df
addLine <- function(df, clr, siz=0.3, Area){
  geom_line(aes(x = date, y = `unemployment rate`, color=Area), size=siz, data = df)
}
createUnemploymentPlt <- function(){
# Read file names from directories
OR_tmp <- list.files(path="Unemployment_Data/OR_unemploymentData/",pattern="*.xlsx",full.names = TRUE)
ID_tmp <- list.files(path="Unemployment_Data/ID_unemploymentData/",pattern="*.xlsx",full.names = TRUE)
malheur <- read_excel("Unemployment_Data/Malheur OR Unemployment.xlsx", skip = 10)

# Read excel files from list of file names
OrData <- sapply(OR_tmp, read_excel, skip=10,simplify = FALSE)
IdData <- sapply(ID_tmp, read_excel, skip=10,simplify = FALSE)

# Change OrData into usable metrics
for(i in 1:length(OrData)){
  # Add a date column as a unique identifier
  OrData[[i]]$date <- paste(OrData[[i]]$Period,OrData[[i]]$Year,"01")
  
  # Convert date column to date format
  OrData[[i]]$date <- as.Date(OrData[[i]]$date, "%b %Y %d")
}

# Change IdData into usable metrics
for(i in 1:length(IdData)){
  # Add a date column as a unique identifier
  IdData[[i]]$date <- paste(IdData[[i]]$Period,IdData[[i]]$Year,"01")
  
  # Convert date column to date format
  IdData[[i]]$date <- as.Date(IdData[[i]]$date, "%b %Y %d")
}

# Change Malheur into usable metrics
malheur$date <- paste(malheur$Period, malheur$Year, "01")
malheur$date <- as.Date(malheur$date, "%b %Y %d")

# Create formatted Plot
p <- ggplot() + 
  # Add lines for every OR county (not Malheur)
  sapply(OrData, addLine, clr="gray", Area="Oregon Counties") +
  
  # Add lines for ID counties
  sapply(IdData, addLine, clr="blue", siz=1, Area="Idaho Counties") +
  
  # Add Malheur Line
  addLine(malheur, clr="red", siz=1.5, Area="Malheur") +
  
  # Set x-axis
  scale_x_date(name="Year",date_breaks="1 year", date_labels = "%Y") +
  
  # Set y-axis
  scale_y_continuous(name="Rate (%)",breaks=c(0,5,10,15,20,25,30),
                     labels = c("0%","5%","10%","15%","20%","25%","30%")) +
  
  # Set colors to correct values
  scale_color_manual(values = c("aquamarine4","red","gray35")) +
  
  # Add graph title
  ggtitle("Unemployment Data") + 
  
  # Format graph
  theme(plot.title = element_text(size=40,hjust=.5,color="white"),
        legend.title = element_text(size=15,hjust = .5),                      # Format the legend title
        legend.text = element_text(size=13),                                  # Format legend text
        axis.title = element_text(size=15,color="white"),                     # Format axis titles
        axis.text = element_text(size=15,color="white"),                      # Format axis tick marks
        plot.background = element_rect("gray40"),                             # Format background color
        panel.grid.major = element_line(size=.5,color="gray40"),              # Format major grid lines
        panel.grid.minor.x = element_blank(),                                 # Get rid of minor gridlines on x-axis
        panel.grid.minor.y = element_line(size=.1),                           # Reduce minor gridlines on y-axis
        panel.background = element_rect(fill="gray15",color="gray80",size=0)  # Format plot background color/border
  )
plot(p)
}
createUnemploymentPlt()
# Plot Data
# plot(p)
# 
# # Save to PNG
# png("../Plots/Unemployment/Unemployment_Graph.png",width = 1920, height = 1080, res = 150)
# plot(p)
# dev.off()
