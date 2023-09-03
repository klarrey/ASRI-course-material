



#load libraries
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))

# import HUD data
# update your file path as needed
home = read.csv("/Users/jennifershelton/Downloads/Repository/Homelessness/HUD-homeless-data.csv", 
                header = TRUE, stringsAsFactors = TRUE)

h3 = home[grep("Overall Homeless", home$Metric), ]

# sort df alphabetically by metric name to get all the features desired near each other with all years
h4 = h3[with(h3, order(Metric, Year)), ]

# rename the row names in sequential order
rownames(h4) = seq(length=nrow(h4))

#slice the dataframe to  just the rows we want
h4 = slice(h4, 1:79)

# transpose the dataframe - removes column and row names
ht = transpose(h4)

# add back the column names from the Metric labels
colnames(ht) = h4$Metric

# add back the row names as a new column called State
ht$State = colnames(h4)

h4 = select(h4, one_of(c("AR", "TN", "MS", "LA", "Metric", "Year")))
#Select columns whose names are in a group of names


## -------------------
# Males homeless numbers and female, overall counts in AR TN MS LA

ggplot(data = h4, aes(y = Year)) + 
  geom_bar()


# Ridge plot showing distribution of squirrel locations
ggplot(h4, aes(x = Year, y = Metric, fill = as.factor(Year))) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

