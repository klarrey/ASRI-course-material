

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


# import bed count data
# update your file path as needed
beds = read.csv("/Users/jennifershelton/Downloads/Repository/2021-HIC-Counts-by-State.csv", 
                header = TRUE)


# let's make a new dataframe with only some of the columns of interest
#HudNum, Project.Type, Geo.Code, city, state, DV.Beds, Total.Beds
beds2 =  data.frame(matrix(ncol = 0, nrow = 27636))
beds2$HudNum = beds$HudNum
beds2$Organization.Name = beds$Organization.Name
beds2$Project.Type = beds$Project.Type
beds2$Geo.Code = beds$Geo.Code
beds2$Address1 = beds$address1
beds2$Address2 = beds$address2
beds2$city = beds$city
beds2$state = beds$state
beds2$DV.Beds = beds$DV.Beds
beds2$totalbeds = beds$Total.Beds
beds2$yearbeds = beds$Year.Round.Beds


# filter out only the data from Oklahoma
OKbeds = beds2[grep("OK", beds2$state), ]


# now we will import the latitudes and longitudes, but we have to do it in batches
# there is a limit of 10,000 rows per batch
library(tidygeocoder)
geogs = OKbeds %>%
  geocode(street = Address1, city = city, state = state, method = "census")

# define the icon - maybe play around with the color choices
icon1 = awesomeIcons(icon = 'bed', iconColor = 'black', library = 'glyphicon', markerColor = "purple") 


# here is just a basic map with each address marked with the purple icon
leaflet(data=geogs) %>%
  #add base layer of map    
  addTiles() %>% 
  addAwesomeMarkers(lat = geogs$lat, 
                    lng = geogs$long,
                    icon = icon1,
                    group = "DVBeds"
  ) %>%
  addLayersControl(overlayGroups = c("DVBeds"),
                   options = layersControlOptions(collapsed = FALSE))


# same map but with cluster logic to group things that are close together
leaflet(data=geogs) %>%
  #add base layer of map    
  addTiles() %>% 
  addAwesomeMarkers(lat = geogs$lat, 
                    lng = geogs$long,
                    icon = icon1,
                    group = "DVBeds",
                    clusterOptions = markerClusterOptions(), 
                    clusterId = "DV Beds"
  ) %>%
  
  addLayersControl(overlayGroups = c("DVBeds"),
                   options = layersControlOptions(collapsed = FALSE))




# same clustered map but with custom popups
leaflet(data=geogs) %>%
  #add base layer of map    
  addTiles() %>% 
  
  addAwesomeMarkers(lat = geogs$lat, 
                    lng = geogs$long,
                    icon = icon1,
                    group = "DVBeds",
                    popup = paste(paste("Organization Name: ", geogs$Organization.Name, "<br>",
                                        "City: ", geogs$city, "<br>",
                                        "Total Number of Beds: ", geogs$totalbeds)),
                    clusterOptions = markerClusterOptions(), 
                    clusterId = "DV Beds") %>%
  
  addLayersControl(overlayGroups = c("DVBeds"),
                   options = layersControlOptions(collapsed = FALSE))




