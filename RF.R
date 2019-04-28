setwd("./")
load("contest3_train_test.Rdata")
library(randomForest)
#########################################################
################train data cleaning######################
#########################################################
#replacing NA in the recommendPrice with the same amount of price
train$recommendedPrice[is.na(train$recommendedPrice)] <- train$price[is.na(train$recommendedPrice)]
anyNA(train)
Day1 <- as.Date(train$orderDate[1])
train$orderDate <- (as.Date(train$orderDate) - Day1)
train$voucherID <- ifelse(train$voucherID=='NONE', 0, 1)
train$orderID <- sapply(train$orderID, function(x){gsub('R', '', x)})
orderID1 <- min(as.numeric(train$orderID))
train$itemID <- sapply(train$itemID, function(x){gsub('A', '', x)})
itemID1 <- as.numeric(min(train$itemID))
train$customerID <- sapply(train$customerID, function(x){gsub('C', '', x)})
customerID1 <- min(as.numeric(train$customerID))
train$customerID <- as.numeric(train$customerID) - customerID1
train$itemID <- as.numeric(train$itemID) - itemID1
train$orderID <- as.numeric(train$orderID) - orderID1
train$deviceCode <- as.numeric(train$deviceCode)
train$paymentCode <- as.numeric(train$paymentCode)
train$sizeCode <- as.factor(train$sizeCode)
train$return <- as.factor(train$return)
train <- train[,-1]
#########################################################
################test data cleaning#######################
#########################################################
test$recommendedPrice[is.na(test$recommendedPrice)] <- test$price[is.na(test$recommendedPrice)]
anyNA(test)
test$orderDate <- (as.Date(test$orderDate) - Day1)
test$voucherID <- ifelse(test$voucherID=='NONE', 0, 1)
test$orderID <- sapply(test$orderID, function(x){gsub('R', '', x)})
test$itemID <- sapply(test$itemID, function(x){gsub('A', '', x)})
test$customerID <- sapply(test$customerID, function(x){gsub('C', '', x)})
test$itemID <- as.numeric(test$itemID) - itemID1
test$customerID <- as.numeric(test$customerID) - customerID1
test$orderID <- as.numeric(test$orderID) - orderID1
test$deviceCode <- as.numeric(test$deviceCode)
test$paymentCode <- as.numeric(test$paymentCode)
test$sizeCode <- as.factor(test$sizeCode)
test <- test[,-1]
#########################################################
#########################################################
#########################################################
set.seed(1367)
#K-fold cross validation
n <- round(nrow(train))
k <- 5
Er_rf <- rep(NA, k)
Folds <- split(sample(n, n, replace=FALSE), as.factor(1:k))

for (i in 1:k){
  Indx <- Folds[[i]]
  f_RF <- randomForest(return ~  ., data = train[-Indx,], ntree = 128)
  Res_RF <- predict(f_RF, newdata = train[Indx,])
  table(Res_RF)
  Er_rf[i] <- sum(Res_RF == train$y_train[Indx])/length(Indx)
  f_RF <- NULL
  paste(i/k)
  pause(1)
}
round(mean(Er_rf), 6)
#########################################################
#########################################################
#########################################################
