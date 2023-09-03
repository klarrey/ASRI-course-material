#load libraries
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
library(wordcloud2) 
library(SnowballC)
library(tm)
library(ggridges)


# import squirrel data
squirrel = read.csv("/Users/jennifershelton/Downloads/Repository/Squirrel Census/squirrel-data.csv", 
                    header = TRUE, stringsAsFactors = TRUE)

# understand more about the dataset
str(squirrel)

# let's generate some plots for this data
# we will start with a word cloud for the activities of the squirrels

# first we create a corpus
activities = Corpus(VectorSource(squirrel$Activities))
                    
#Conversion to all Lowercase
activities = tm_map(activities, PlainTextDocument)
activities = tm_map(activities, tolower)
                    
#Removing Punctuation
activities = tm_map(activities, removePunctuation)
                    
# Eliminate white spaces
activities = tm_map(activities, stripWhitespace)

#Now that our corpus is prepared and cleaned, we can put the words into a matrix 
# for the word cloud function.

#create matrix
TM = TermDocumentMatrix(activities)
wordmatrix = as.matrix(TM)

# sort the matrix by frequency of words
sortedmatrix = sort(rowSums(wordmatrix),decreasing=TRUE)

# create a dataframe
worddf = data.frame(word = names(sortedmatrix),freq=sortedmatrix)

#generate word cloud
wordcloud2(data=worddf, size=3, color='random-dark')

# let's look at the squirrel data and make some more plots

# Ridge plot showing distribution of squirrel locations
ggplot(squirrel, aes(x = Area.Name, y = Park.Name, fill = Park.Name)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
