require(FITSio)
library(FITSio)
library(dplyr)
library(mlbench)
library(caret)
library(e1071)
library(lime)

        
df <- read.csv(file = "Stars.csv")
df$Type <- as.factor(df$Type)
str(df)

pairs.panels(df)
table(df$Type)

set.seed(1234)
ind <- sample(2,nrow(df), replace = T, prob = c(0.5,0.5))
train <- df[ind == 1,]
test <- df[ind == 2,]

#Regression

#Bagging

set.seed(1234)
cvcontrol <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 2,
                          allowParallel = T
                          )

set.seed(1234)
bag <- train(Type ~ .,
             data = train,
             method = "treebag",
             trControl = cvcontrol,
             importance = T)
plot(varImp(bag))


#RF

set.seed(1234)
forest <- train(Type ~ .,
                data = train,
                method = "rf",
                trControl = cvcontrol,
                importance = T)
plot(forest)
plot(varImp(forest))

#Explain Predictions

explainer <- lime(test[1:10,],forest, n_features = 5)
explanation <- explain(x = test[1:10,],
                       explainer = explainer,
                       labels = NULL,
                       n_labels = 1,
                       n_features = 5)
plot_features(explanation)

# Data transformation
# give us a input of spectrum and output is a color
# to find the color there = wavelength , each of the elements temperature
# Logistic Regression model 


# Temperature
# Luminosity
# Radius
# A_M Absolute Magnitude
# Color
# Spectral Class
# Type
# HR Diagram Hertzsprung Russell Diagram

# Random Forest - Type is the response variable 
# Classification - Daniel
# Logistic Regression - Prashanth
# 
# Red Dwarf - 0
# Brown Dwarf - 1
# White Dwarf - 2
# Main Sequence - 3
# Super Giants - 4
# Hyper Giants - 5
