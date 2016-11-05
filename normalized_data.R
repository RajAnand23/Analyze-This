###   Applying the model on the normalized data with all the variables
train_norm = read.csv(file = "train_norm.csv", header = TRUE)
test_norm = read.csv(file = "test_norm.csv", header = TRUE)

train_norm = cbind(train_norm, reg_train$actual_vote)
names(train_norm)[ncol(train_norm)] = "actual_vote"

###  Applying the rpart model first of all
library(rpart)
library(e1071)
norm_rpart = rpart(actual_vote ~., data = train_norm, method = "class",
                   control = rpart.control(cp = 0.09))
##            rpart is taking too long a time

###   Applying the random forest model
library(e1071)
library(randomForest)

norm_rf = best.randomForest(x=train_norm[,-c(35)],y=train_norm[,35],importance=TRUE,ntree=100)
norm_rf_pred = predict(best_rf, test_norm)
####      random forest is not taking variable more than 58 levels. 

### Taking the top 50 levels and converting the rest to Othes
lev_32 = sort(table(train_norm$mvar32), decreasing = TRUE)
lev_32 = cbind(names(lev_32), as.matrix(lev_32))
lev_32 = as.data.frame(lev_32, row.names = FALSE)
names(lev_32)[1] = "mvar32"
names(lev_32)[2] = "Freq32"
lev_32$mvar32 = as.factor(as.character(lev_32$mvar32))
lev_32$Freq32 = as.integer(as.character(lev_32$Freq32))

lev_33 = sort(table(train_norm$mvar33), decreasing = TRUE)
lev_33 = cbind(names(lev_33), lev_33)
lev_33 = as.data.frame(lev_33, row.names = FALSE)
names(lev_33)[1] = "mvar33"
names(lev_33)[2] = "Freq33"
lev_33$mvar33 = as.factor(as.character(lev_33$mvar33))
lev_33$Freq33 = as.integer(as.character(lev_33$Freq33))

other_train = train_norm
in32 = which(!(other_train$mvar32 %in% lev_32$mvar32[1:50]))
other_train$mvar32 = as.character(other_train$mvar32)
other_train[ in32, 33] = "Other"
other_train$mvar32 = as.factor(other_train$mvar32)

in33 = which(!(other_train$mvar33 %in% lev_33$mvar33[1:50]))
other_train$mvar33 = as.character(other_train$mvar33)
other_train[ in33, 34] = "Other"
other_train$mvar33 = as.factor(other_train$mvar33)

other_test = test_norm
in32_test = which(!(other_test$mvar32 %in% lev_32$mvar32[1:50]))
other_test$mvar32 = as.character(other_test$mvar32)
other_test[ in32_test, 33] = "Other"
other_test$mvar32 = as.factor(other_test$mvar32)

in33_test = which(!(other_test$mvar33 %in% lev_33$mvar33[1:50]))
other_test$mvar33 = as.character(other_test$mvar33)
other_test[ in33_test, 34] = "Other"
other_test$mvar33 = as.factor(other_test$mvar33)

num_data = rbind(train_norm[,-35], test_norm)
