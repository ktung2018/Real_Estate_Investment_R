---
title: "Real Estate Investment"
output:
  html_document: default
---

library(tidyverse)

# Multiple Linear Regression
# Importing the CSV dataset
dataset = read.csv('housing_r.csv')
dataset
sapply(dataset, mode)
sapply(dataset, class)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')

library(caTools)
set.seed(123)
split = sample.split(dataset$Price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
#training_set = scale(training_set)
#test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set

regressor = lm(formula = Price ~ .,
               data = training_set)


# Predicting the Test set results

y_pred = predict(regressor, newdata = test_set)
y_pred

# Building the optimal model using Backward Elimination

regressor = lm(formula = Price ~ Square_Feet + Zip_code + Bed + Bath,
               data = dataset)
summary(regressor)
regressor = lm(formula = Price ~ Square_Feet + Zip_code + Bed,
               data = dataset)
summary(regressor)
regressor = lm(formula = Price ~ Square_Feet + Zip_code,
               data = dataset)
summary(regressor)
regressor = lm(formula = Price ~ Square_Feet,
               data = dataset)
summary(regressor)

# Visualising the Training set results

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Square_Feet, y = training_set$Price),
             colour = 'red') +
  geom_line(aes(x = training_set$Square_Feet, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Price vs Square_Footage (Training set)') +
  xlab('Square_Feet') +
  ylab('Price')


# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Square_Feet, y = test_set$Price),
             colour = 'red') +
  geom_line(aes(x = training_set$Square_Feet, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Profit vs Square_Feet (Testing set)') +
  xlab('Square_Feet') +
  ylab('Price')



