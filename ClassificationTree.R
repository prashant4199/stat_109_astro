
install.packages('tree')
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)
library(caret)
library(ggplot2)
library(tidyverse)
library(cowplot)


mydata <- read.csv('C:/Users/danie/Documents/Harvard Extension School/STAT E-109/Project/DataStars1.csv')

mydata$Type <- factor(mydata$Type)

noOther <-  mydata %>% filter(Type == 'Giant' | Type =='Dwarf' )


summary(mydata)
str(mydata)


set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.75, 0.25))
trainR <- mydata[ind == 1,]
train <- trainR[,-which(names(trainR) == "R")]
testR <- mydata[ind == 2,]
test <- testR[,-which(names(testR) == "R")]

#Tree

tree <- rpart(Type~., data = train)
treeR <- rpart(Type~., data = trainR)


par(mfrow=c(2,2))
rpart.plot(tree)
rpart.plot(treeR)
plotcp(tree)
plotcp(treeR)


printcp(tree)
printcp(treeR)



# Confusion matrix -train
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$Type)

 pR <- predict(treeR, trainR, type = 'class')
confusionMatrix(pR, trainR$Type)

# Confusion matrix -test
pTest <- predict(tree, test, type = 'class')
confusionMatrix(pTest, test$Type)


pRTest <- predict(treeR, testR, type = 'class')
confusionMatrix(pRTest, testR$Type)



# Box plots for data 

par(mfrow=c(2,2))
plotT <- ggplot(noOther, aes(x=Type, y=Temperature)) + 
  geom_boxplot(aes(fill=Type)) + theme_bw()

plotL <- ggplot(noOther, aes(x=Type, y=L)) + 
  geom_boxplot(aes(fill=Type)) + theme_bw()

plotR <-ggplot(noOther, aes(x=Type, y=R)) + 
  geom_boxplot(aes(fill=Type)) + theme_bw()

plotAM <-ggplot(noOther, aes(x=Type, y=A_M)) + 
  geom_boxplot(aes(fill=Type)) + theme_bw()

plotM <-ggplot(noOther, aes(x=Type, y=mass)) + 
  geom_boxplot(aes(fill=Type)) + theme_bw()

plot_grid(plotT, plotL,plotR,plotAM,plotM, labels = c('Temp', 'Lum','Radius','A_M','Mass'), label_size = 12)


