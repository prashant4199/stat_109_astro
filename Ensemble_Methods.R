require(FITSio)
library(FITSio)
library(dplyr)
library(mlbench)
library(caret)
library(e1071)
library(lime)
library(psych)
library(rpart)
library(rpart.plot)
library(pROC)
library(tree)

options(scipen = 999)

df <- read.csv(file = "DataStars1.csv", header = T)
df$Type <-
  as.factor(df$Type) # convert the variable Type as a factor.

pairs.panels(
  df,
  ellipses = F,
  bg = c("red", "green", "yellow", "blue", "lightgreen", "black")[df$Type],
  hist.col = "gray",
  lm = F
)

table(df$Type)
set.seed(1234)
ind <- sample(2, nrow(df), replace = T, prob = c(0.75, 0.25))
train <- df[ind == 1, ]
test <- df[ind == 2, ]


# Bagging


cvcontrol <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  allowParallel = T
)


bag <- train(
  Type ~ .,
  data = train,
  method = "treebag",
  trControl = cvcontrol,
  importance = T
)
plot(varImp(bag), main = "BAGGING")

p_train <- predict(bag, train, type = "raw")
confusionMatrix(p_train, train$Type)

p <- predict(bag, test, type = "raw")
confusionMatrix(p, test$Type)


# RF
forest <- train(
  Type ~ .,
  data = train,
  method = "rf",
  trControl = cvcontrol,
  importance = T
)
plot(forest)
plot(varImp(forest), main = "RANDOM FOREST")

p_train <- predict(forest, train, type = "raw")
confusionMatrix(p_train, train$Type)

p <- predict(forest, test, type = "raw")
confusionMatrix(p, test$Type)

# Explain Predictions

explainer <- lime(test[1:10, ], forest, n_features = 5)
explanation <- explain(
  x = test[1:10, ],
  explainer = explainer,
  labels = NULL,
  n_labels = 1,
  n_features = 5
)
plot_features(explanation)


# Boosting different gamma values

set.seed(1234)
boo <- train(
  Type ~ .,
  data = train,
  method = "xgbTree",
  trControl = cvcontrol,
  tuneGrid = expand.grid(
    nrounds = 500,
    max_depth = 4,
    eta = 0.28,
    gamma = 1,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

plot(varImp(boo), main = "XG BOOST Gamma = 1")

p_train <- predict(boo, train, type = "raw")
confusionMatrix(p_train, train$Type)

p <- predict(boo, test, type = "raw")
confusionMatrix(p, test$Type)


boo <- train(
  Type ~ .,
  data = train,
  method = "xgbTree",
  trControl = cvcontrol,
  tuneGrid = expand.grid(
    nrounds = 500,
    max_depth = 3,
    eta = 0.50,
    gamma = 2,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

plot(varImp(boo), main = "XG BOOST Gamma = 2")
p <- predict(boo, test, type = "raw")
confusionMatrix(p, test$Type)

boo <- train(
  Type ~ .,
  data = train,
  method = "xgbTree",
  trControl = cvcontrol,
  tuneGrid = expand.grid(
    nrounds = 500,
    max_depth = 3,
    eta = 0.28,
    gamma = 1.8,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

plot(varImp(boo), main = "XG BOOST Gamma = 1.8")
p <- predict(boo, test, type = "raw")
confusionMatrix(p, test$Type)


boo <- train(
  Type ~ .,
  data = train,
  method = "xgbTree",
  trControl = cvcontrol,
  tuneGrid = expand.grid(
    nrounds = 500,
    max_depth = 4,
    eta = 0.28,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

plot(varImp(boo), main = "XG BOOST Gamma = 0")
p <- predict(boo, test, type = "raw")
confusionMatrix(p, test$Type)
