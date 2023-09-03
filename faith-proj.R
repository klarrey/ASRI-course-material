


#load libraries
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(mapdeck))
suppressPackageStartupMessages(library(leaflet.minicharts))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tigris))


# import beds data
# update your file path as needed
beds = read.csv("/Users/jennifershelton/Downloads/Repository/2021-HIC-Counts-by-State.csv", 
                header = TRUE, stringsAsFactors = TRUE)

# import geocodes data
# update your file path as needed
geocodes = read.csv("/Users/jennifershelton/Downloads/Repository/hud-geocode-dict.csv", 
                header = TRUE, stringsAsFactors = TRUE)

#make a geographic code column to match the other file
geocodes$Geo.Code = geocodes$Geographic.Code

# let's make a new dataframe with only some of the columns of interest
#HudNum, Project.Type, Geo.Code, city, state, DV.Beds, Total.Beds
beds2 =  data.frame(matrix(ncol = 0, nrow = 27636))
beds2$HudNum = beds$HudNum
beds2$Project.Type = beds$Project.Type
beds2$Geo.Code = beds$Geo.Code
beds2$city = beds$city
beds2$state = beds$state
beds2$DV.Beds = beds$DV.Beds
beds2$totalbeds = beds$Total.Beds


# pull in county names from geocode dictionary
df = inner_join(beds2, geocodes, by = "Geo.Code")

# create a map of oklahoma DV beds by county

# load shapefiles for counties in Oklahoma
okcounties = counties(state = "OK")


#define the map
mymap = leaflet(data=beds) %>%
  
  #add base layer of map    
  addTiles() %>%
  
  # add markers with clustering logic
  addAwesomeMarkers(data = beds$DV.Beds,
                    clusterOptions = markerClusterOptions(), 
                    clusterId = "DV Beds") %>%
  
#display map
mymap




