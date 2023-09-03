
# DATA WRANGLING Example with Rural Economic Data and Homelessness Data

# RECORDING covering this example: https://youtu.be/XVxlGNIVZ5s

# The goal of this is to combine certain elements of multiple datasets
# we are going to create a dataframe for analysis with features pertaining to demographic makeup of 2020 population

#load libraries
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))

#import rural dataset
# update your file path as needed
rural_long = read.csv("/Users/jennifershelton/Downloads/Repository/Rural Economy/rural-econ.csv", 
                      header = TRUE, stringsAsFactors = TRUE)
rural_long = read.csv("rural-econ.csv", 
                      header = TRUE, stringsAsFactors = TRUE)

# import HUD data
# update your file path as needed
home = read.csv("HUD-homeless-data.csv", 
                header = TRUE, stringsAsFactors = TRUE)

# filter the large rural dataset for all rows where the Attribute column contains the string "Pct2020" 
rural2 = rural_long[grep("Pct2020", rural_long$Attribute), ]

# filter the large rural dataset for all rows where the Attribute column contains the string "Num2020" 
rural3 = rural_long[grep("Num2020", rural_long$Attribute), ]

#spread the new rural dataframes from long to wide
r2 = spread(rural2, Attribute, Value)
r3 = spread(rural3, Attribute, Value)

# filter the homelessness data by selecting all the rows with "Veteran" in the Metrics column
h3 = home[grep("Veteran", home$Metric), ]

# filter the new dataframe for only the rows matching the Year of 2020
h3 = filter(h3, Year==2020)

# filter again to only select the Overall metrics 
h3 = h3[grep("Overall", h3$Metric),]  

# remove the Year column, it's all 2020 so we don't need it anymore
h3 = select(h3, -c("Year"))

# transpose the dataframe - removes column and row names
ht = transpose(h3)
ht = t(h3)

# add back the column names from the Metric labels
colnames(ht) = h3$Metric

# add back the row names as a new column called State
ht$State = colnames(h3)

# join the data elements, first by state
combined = inner_join(ht, r2, by = "State")

#join the other data elements by FIPS - have to bring this in to calculate percentages for HUD data
combined = inner_join(combined, r3, by = "FIPS")

# remove commas from the numbers in HUD data - this was causing values to be read as characters
combined$`Overall Homeless Veterans - Black or African American`  = gsub(",","",combined$`Overall Homeless Veterans - Black or African American`)
combined$`Overall Homeless Veterans - Hispanic/Latino`  = gsub(",","",combined$`Overall Homeless Veterans - Hispanic/Latino`)
combined$`Overall Homeless Veterans - Multiple Races`  = gsub(",","",combined$`Overall Homeless Veterans - Multiple Races`)

# create new columns to calculate the percentages of overall population for homeless data
combined$AsianHomelessVetPct = as.numeric(combined$`Overall Homeless Veterans - Asian`)/as.numeric(combined$AsianNonHispanicNum2020)
combined$BlackHomelessVetPct = as.numeric(combined$`Overall Homeless Veterans - Black or African American`) / as.numeric(combined$BlackNonHispanicNum2020)
combined$HispanicHomelessVetPct = as.numeric(combined$`Overall Homeless Veterans - Hispanic/Latino`) / as.numeric(combined$HispanicNum2020)
combined$MultiHomelessVetPct = as.numeric(combined$`Overall Homeless Veterans - Multiple Races`) / as.numeric(combined$MultipleRaceNum2020)
combined$NativeAmHomelessVetPct = as.numeric(combined$`Overall Homeless Veterans - American Indian or Alaska Native`) / as.numeric(combined$NativeAmericanNonHispanicNum2020)

# remove the homeless data with total numbers instead of percentages
finaldf = select(combined, State.x:NativeAmHomelessVetPct)

#drop the total population counts from the rural data that we don't want anymore
finaldf = subset(finaldf, select = -c(State.y, County.y, Age65AndOlderNum2020, 
                                      AsianNonHispanicNum2020, BlackNonHispanicNum2020, HispanicNum2020, MultipleRaceNum2020, 
                                      NativeAmericanNonHispanicNum2020, Under18Num2020, WhiteNonHispanicNum2020))

# Now we have a dataframe with the demographic percentages of population from rural dataset 
# and the percent of population that are homeless in the same demographic categories from HUD data

# write the resulting data frame to a csv file to save it
# update your file path as needed
write.csv(finaldf, file = "combined-final.csv")



