library(arules)
library(arulesViz)

setwd('E:\\PTA\\Task 4 - Market Basket Analysis\\Task 4 - Market Basket Analysis')
Dataset<-read.transactions('ElectronidexTransactions2017.csv',format = "basket", sep = ",")
Dataset
inspect (Dataset) # You can view the transactions. Is there a way to see a certain # of transactions?
length (Dataset) # Number of transactions.
size (Dataset) # Number of items per transaction
LIST(Dataset) # Lists the transactions by conversion (LIST must be capitalized)
x <- itemLabels(Dataset)# To see the item labels

#Visualize the Dataset
itemFrequencyPlot(Dataset,topN = 10)
image(Dataset)

image(sample(Dataset,10))

sample(Dataset,10)

#Apply the Apriori Algorithm
result_rule<-apriori(Dataset,parameter = list(supp = 0.002, conf = 0.8), minlen = 2)
summary(result_rule)

# Improve Model
top.support <- sort(result_rule, decreasing = TRUE, na.last = NA, by = "support")
top.confidence <- sort(result_rule, decreasing = TRUE, na.last = NA, by = "confidence")
top.fit <- sort(result_rule, decreasing = TRUE, na.last = NA, by = "lift")

inspect(head(top.support, 10))
inspect(head(top.confidence, 10))
inspect(head(top.fit, 10))

inspect(subset(result_rule,support>0.0025)) #use subset
inspect(subset(result_rule,confidence>0.85))
inspect(subset(result_rule,lift>4))

#check redundant
is.redundant(result_rule)

#Visualize Results
plot(result_rule, method="scatter",jitter = 0)
plot(head(top.fit, 10), method="graph")
plot(head(top.confidence, 10), method="grouped")

#parallel coordinates plot
plot(result_rule, method = "paracoord", control = list(reorder = TRUE)) #c
