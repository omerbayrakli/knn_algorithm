#reading data 
dataset <- read.table("heart.csv", header = T, sep = ",")

summary(dataset)
table(dataset$age)

dataset$presence <- as.factor(dataset$presence)

library(caret)
set.seed(1)
my_indexes <- caret::createDataPartition(y = dataset$presence, times = 1, p = .70, list = FALSE)

training <- as.data.frame(dataset[my_indexes,])
test <- as.data.frame(dataset[-my_indexes,])

table(dataset$presence)
table(training$presence)
table(test$presence)



library(class)
set.seed(1)

(knn_predictions <- knn(train = training[, -14], test = test[, -14], cl = training[[14]], k = 3))
#(knn_predictions2 <- knn(train = training[, -14], test = c(92, 0, 3, 124, 300, 0, 0, 100, 0, 2.3, 2, 3, 7), cl = training[[14]], k = 3))
head(knn_predictions)
head(test[[14]])


(my_table <- table(knn_predictions, test[[14]], dnn = c("Predictions", "Actual/Reference")))


tp <- 227
tn <- 183
fp <- 0
fn <- 1


acc <- ( tp + tn ) / sum(my_table)
err <- 1 - acc
sens <- tp / (tp + fn)
spec <- tn / (tn + fp)
ppv <- tp / (tp +fp)
npv <- tn / (tn + fn)
f1 <- (2 * sens * ppv) / (sens + ppv)


myConf <- confusionMatrix(data = knn_predictions, reference = test[[14]], dnn = c("Predictions", "Actual/Reference"), mode = "everything")
myConf