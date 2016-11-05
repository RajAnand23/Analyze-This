setwd("./Analyze_it")

train = read.csv(file = "Training_Dataset.csv", header = T)
test_ori = read.csv(file = "Leaderboard_Dataset.csv", header = T)

###v Imputing mvar28  and mvar29 with mode
train$mvar28[which(is.na(train$mvar28))] = 1   # Mode is found by using table
train$mvar29[which(is.na(train$mvar29))] = 0   # Mode is found by using table
test_ori$mvar28[is.na(test_ori$mvar28) == T] = 1
test_ori$mvar29[is.na(test_ori$mvar29) == T] = 0

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

###   Removing the factor variables with large number of levels
tune_train = tune_train[,-which(names(tune_train)%in% c("mvar32", "mvar33"))]
tune_test = tune_test[,-which(names(tune_test)%in% c("mvar32", "mvar33"))]


####        Applying Random Forest
library(e1071)
library(randomForest)

best_rf = best.randomForest(x=tune_train[,-c(33)],y=tune_train[,33],importance=TRUE,ntree=500)
rf_pred = predict(best_rf, tune_test[,-33])


## removing the mvar32 and mvar33 from the data
test = test[,-c(33,34)]
rf_test_pred = predict(best_rf, test)
submit_rf2 = cbind(test_ori$citizen_id, as.data.frame(rf_test_pred))
write.csv(submit_rf2, file = "insight_IITKgp_5.csv", row.names = FALSE)
confusionMatrix(rf_pred, tune_test[,33])

####     Applying the random forest on the whole training set and then testing it on the test
rf_whole = best.randomForest(x = reg_train[-c(33:35)], y = reg_train[,35], importance= TRUE, ntree = 150)
rf_pred_whole = predict(rf_whole, test[,-34])
confusionMatrix(rf_pred_whole, test[,34])

submit_rf = as.data.frame(cbind(test_ori$citizen_id, rf_pred_whole))
write.csv(submit_rf, file = "Insight_IITKgp_2.csv", row.names = FALSE )





