setwd("./Analyze_it")

train = read.csv(file = "Training_Dataset.csv", header = T)
test_ori = read.csv(file = "Leaderboard_Dataset.csv", header = T)

###v Imputing mvar28  and mvar29 with mode
train$mvar28[which(is.na(train$mvar28))] = 1   # Mode is found by using table
train$mvar29[which(is.na(train$mvar29))] = 0   # Mode is found by using table
test$mvar28[is.na(test$mvar28) == T] = 1
test$mvar29[is.na(test$mvar29) == T] = 0

## Removing the id col
reg_train = train
reg_train = reg_train[,-which(names(train) == "citizen_id")]
train = train[,which(names(train) == "citizen_id")]
test = test_ori
test = test[,-which(names(test_ori) == "citizen_id")]
test_ori = test_ori[,which(names(test_ori) == "citizen_id")]

## preparing reg_train and test set
reg_train[,1]= as.factor(reg_train[,1])
reg_train[,28]= as.factor(reg_train[,28])
reg_train[,31]= as.factor(reg_train[,31])
reg_train[,33]= as.factor(reg_train[,33])
reg_train[,34]= as.factor(reg_train[,34])
reg_train[,35]= as.factor(reg_train[,35])
reg_train$mvar28 = as.factor(reg_train$mvar28)
reg_train$mvar29 = as.factor(reg_train$mvar29)

test[,1]= as.factor(test[,1])
test[,28]= as.factor(test[,28])
test[,31]= as.factor(test[,31])
test[,33]= as.factor(test[,33])
test[,34]= as.factor(test[,34])
test$mvar28 = as.factor(test$mvar28)
test$mvar29 = as.factor(test$mvar29)

#Eliminating the empty levels
###     Imputing the empty education to the mode value
reg_train$mvar30[which(reg_train$mvar30=="")] = "Masters"
reg_train$mvar30 = as.factor(as.character(reg_train$mvar30))
test$mvar30[which(test$mvar30 == "")] = "Masters"
test$mvar30  = as.factor(as.character(test$mvar30))

# Partitioning the data into train and test on 80:20 ratio
library(caret)
set.seed(12)
ind = sample(1:nrow(reg_train), round(0.8*nrow(reg_train)))
tune_train = reg_train[ind, ]
tune_test = reg_train[-ind,]
res_train = tune_train$actual_vote
res_test = tune_test$actual_vote
rm(ind)

####   Applying the SVM model

library(e1071)
svm_mod = best.svm(tune_train[,-33] )













