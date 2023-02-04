# read csv files
library(readr)
CompleteResponses <- read_csv("CompleteResponses.csv")
SurveyIncomplete <- read_csv("SurveyIncomplete.csv")

library(ggplot2)
library(lattice)
library(caret)
library(C50)

set.seed(998)
#Use 75% of dataset as training set and remaining 25% as testing set
inTrain <- createDataPartition( y = CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[ inTrain,]
testing <- CompleteResponses[ -inTrain,]
str(training)
set.seed(1)

testing[, 7] <- as.factor(make.names(testing[, 7]))
training[, 7] <- as.factor(make.names(training[, 7]))
SurveyIncomplete[, 7] <- as.factor(make.names(SurveyIncomplete[, 7]))

training$salary <- as.numeric(training$salary)
training$credit <- as.numeric(training$credit)

#c50grid <- expand.grid(.winnow = c(TRUE,FALSE))
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
c50fit2 <- train(brand~., data = training, method = "C5.0", 
                 trControl = ctrl, 
                 #tunegrid = c50grid, 
                 tuneLength = 1,
                 preProc = c("center", "scale"))

c50fit2

plot(c50fit2)
varImp(c50fit2)

RocImp2 <- varImp(c50fit2, scale = FALSE)
plot(RocImp2)

#rf
rfgrid <- expand.grid(mtry=c(1,2,3,4,5))
rfFit <- train(brand ~ ., data = training, method = "rf",
               trControl = ctrl, 
               tunegrid = rfgrid,
               tuneLenght = 1, 
               preProc = c("center", "scale")
               )
rfFit

plot(rfFit)
varImp(rfFit)

RocImprf <- varImp(rfFit, scale = FALSE)
plot(RocImprf)

#predict
plsClasses <- predict(rfFit, newdata = testing)
plsClasses

plsProbs <- predict(rfFit, newdata = testing)
head(plsProbs)

plsProbsRT <- predict(c50fit2, newdata = SurveyIncomplete, type = "prob")
head(plsProbsRT)
summary(plsProbsRT)
