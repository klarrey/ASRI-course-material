

#load libraries - install them if you need to
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ISLR))
suppressPackageStartupMessages(library(e1071))


# import data file
# update your file path below as needed

# update your file path as needed
diabetes = read.csv("/Users/jennifershelton/Downloads/Repository/Diabetes/diabetes-data.csv", 
                      header = TRUE, stringsAsFactors = TRUE)


# create a regression model with age as the target
reg = glm(age~hypertension+heart_disease+diabetes+bmi+HbA1c_level+blood_glucose_level, data = diabetes)
summary(reg)


# create a few plots to see relationships between data

# first check relationship between age and bmi
ggplot(data = diabetes, aes(x=bmi, y=age)) +
  geom_point()


# polynomial regression
fit = lm(age~poly(bmi, 3), data = diabetes)
coef(summary(fit))

# plot the polynomial line on the scatter plot
p = ggplot(data = diabetes, aes(x=bmi, y=age)) +
  geom_point()  


# now lets use logistic regression as a binary classification model
# the formula syntax of logistic regression is model = glm(response~predictor, data = , family=binomial)
set.seed(33)
log = glm(diabetes~bmi, data = diabetes, family = binomial)

# print summary
summary(log)

# create a new column with the probability predictions for each observation
diabetes$logistic_predictions = predict(log, type = "response")

# plot the probability
ggplot(data = diabetes, aes(x = bmi, y = logistic_predictions)) +
  geom_point() +
  labs(y = "p(diabetes)", title = "p(diabetes) ~ bmi") +
  theme_minimal()






