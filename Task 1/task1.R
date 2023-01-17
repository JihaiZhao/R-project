# read csv files
library(readr)
iris <- read_csv("iris.csv")
#erase irrelevant data
iris <- iris[,-1]
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train  <- iris[sample, ]
test   <- iris[!sample, ]

x <- as.matrix(train[-5])
y <- as.matrix(train[,5])

y[y == 'setosa'] <- 1
y[y == 'versicolor']<- 2
y[y == 'virginica']<- 3

#create data frame
df <- data.frame(x,y)

# regression
reg1 <- lm(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,df)
summary(reg1)

# predict the car type
xpre<- test[,-5]
predict(reg1, newdata = xpre, interval = 'confidence')