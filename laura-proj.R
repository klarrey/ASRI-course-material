# Laura


#load libraries
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))

#import rural dataset
# update your file path as needed
rural_long = read.csv("/Users/jennifershelton/Downloads/Repository/Rural Economy/rural-econ.csv", 
                      header = TRUE, stringsAsFactors = TRUE)

NConly = rural_long[grep("NC", rural_long$State), ]

# filter the large rural dataset for all rows where the Attribute column contains the string "Pct2020" 
multiplerace = NConly[grep("MultipleRace", NConly$Attribute), ]


#spread the new rural dataframes from long to wide
mrwide = spread(multiplerace, Attribute, Value)

# spread the whole rural dataset to extract the education features easily
NCwide = spread(NConly, Attribute, Value)

# create a filter for the education columns
education = select(NCwide, FIPS:Ed5CollegePlusPct)


#drop the total population counts from the  data that we don't want anymore
education = subset(education, select = -c(Age65AndOlderNum2020, Age65AndOlderPct2020,
                                   AsianNonHispanicNum2020, AsianNonHispanicPct2020, AvgHHSize,              
                                   BlackNonHispanicNum2020, BlackNonHispanicPct2020, 
                                   Deep_Pov_All, Deep_Pov_Children))

# merge the multiple race features with the educational attainment
finaldf = inner_join(education, mrwide, by = "FIPS")

# delete the extra state and county columns left over after the merge
finaldf = subset(finaldf, select = -c(State.y, County.y))

# set target variable
y = finaldf$MultipleRacePct2020
x1 = finaldf$Ed1LessThanHSPct
x2 = finaldf$Ed2HSDiplomaOnlyPct
x3 =  finaldf$Ed3SomeCollegePct
x4 = finaldf$Ed4AssocDegreePct
x5 = finaldf$Ed5CollegePlusPct

# fit a linear regression
reg1 = glm(y ~ finaldf$Ed1LessThanHSPct + finaldf$Ed2HSDiplomaOnlyPct + 
             finaldf$Ed3SomeCollegePct + finaldf$Ed4AssocDegreePct + finaldf$Ed5CollegePlusPct)
# print the summary
summary(reg1)

# Draw the best-fitted line to the scatterplot
plot(x1,y)
abline(glm(y~x1, data=finaldf), col="green", lwd=3)
title(main = "Linear Regression: Ed1LessThanHSPct")

plot(x2,y)
abline(glm(y~x2, data=finaldf), col="blue", lwd=3)
title(main = "Linear Regression: Ed2HSDiplomaOnlyPct")

plot(x3,y)
abline(glm(y~x3, data=finaldf), col="tomato", lwd=3)
title(main = "Linear Regression: Ed3SomeCollegePct")

plot(x4,y)
abline(glm(y~x4, data=finaldf), col="gold", lwd=3)
title(main = "Linear Regression: Ed4AssocDegreePct")

plot(x5,y)
abline(glm(y~x5, data=finaldf), col="purple", lwd=3)
title(main = "Linear Regression: Ed5CollegePlusPct")

# randomForest to see which of the educational attainment features is most related to multiple race pct
rf1 = randomForest(y ~ x1+x2+x3+x4+x5, data=finaldf, mtry=4, importance = TRUE)
varImpPlot(rf1, main = "Correlation of Educational Attainment and Multiple Race Pct")




