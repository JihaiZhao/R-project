library(ggplot2)
library(lattice)
library(caret)
library(readr)
existing_product <- read_csv("existing_product.csv")
# dummify the data 
newDataFrame <- dummyVars(" ~ .", data = existing_product) 
readyData <- data.frame(predict(newDataFrame, newdata = existing_product))

str(readyData)
summary(readyData)
readyData$BestSellersRank <- NULL

corrData <- cor(readyData)
corrData

# visualize the correlation matrix 
library(corrplot)
corrplot(corrData)

#lm
x <- readyData[,-28]
y <- data.frame(readyData[,28])

x <- as.matrix(x)
y <- as.matrix(y)

reg1 <- lm(y ~ x,readyData)
summary(reg1)

#non-parametric machine learning models
set.seed(998)
inTrain<- createDataPartition( y = readyData$Volume, p = .8, list = FALSE)
training <- readyData[ inTrain,]
testing <- readyData[ -inTrain,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
#SVM
trainX <- training[, names(training) != "Volume"]
svmTune <- train(Volume ~ .,data = training,
                 method = "svmRadial",
                 # The default grid of cost parameters go from 2^-2,
                 # 0.5 to 1,
                 # Well fit 9 values in that sequence via the tuneLength
                 # argument.
                 tuneLength = 2,
                 ## Also add options from preProcess here too
                 preProc = c("center", "scale"),
                 trControl = ctrl)
svmTune

#Random Forest
set.seed(1)
rfgrid <- expand.grid(mtry=c(1,2,3,4,5))
rfFit <- train(Volume ~ ., data = training, method = "rf", 
               trControl = ctrl, 
               tunegrid = rfgrid,
               tuneLenght = 2, 
               preProc = c("center", "scale"))
rfFit

#Gradient Boosting
#set.seed(1)
#gbFit <- train(Volume ~ ., data = training, method = "xgbTree", 
#               trControl = ctrl,
#               preProc = c("center", "scale"))
#gbFit

#prediction
svmpredict<-predict(svmTune, newdata=testing)
svmpredict


rfpredict<-predict(rfFit, newdata=testing)
rfpredict

#predict for the new product
# dummify the data 
newDataFrame2 <- dummyVars(" ~ .", data = new_product) 
readyData2 <- data.frame(predict(newDataFrame2, newdata = new_product))

str(readyData2)
summary(readyData2)
readyData2$BestSellersRank <- NULL

rfProbs <- predict(rfFit, newdata = readyData2)
rfProbs
summary(rfProbs)


#output for report
output <- new_product 
output$predictions <- rfProbs
write.csv(output, file = "abc.csv", row.names = TRUE)

varImp(rfFit)

RocImp2 <- varImp(rfFit, scale = FALSE)
plot(RocImp2)
