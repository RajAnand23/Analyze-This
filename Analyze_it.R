library(class)
setwd("./Analyze_it")

train = read.csv(file = "Training_Dataset.csv", header = T)



###v Imputing mvar28  and mvar29 with mode
train$mvar28[which(is.na(train$mvar28))] = 1   # Mode is found by using table
train$mvar29[which(is.na(train$mvar29))] = 0   # Mode is found by using table



###    Clearly the variable reduction by correlation doesn't work well
###    Applying the regression model

## Removing the id col

reg_train = train
reg_train = reg_train[,-which(names(reg_train) == "citizen_id")]

## preparing test set
test_ori = read.csv(file = "Leaderboard_Dataset.csv", header = T)
test = test_ori
test = test[,-which(names(test) == "citizen_id")]
test$mvar28[is.na(test$mvar28) == T] = 1
test$mvar29[is.na(test$mvar29) == T] = 0
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


knn_train = reg_train
knn_train = knn_train[,-c(1,28,31,33:35)]
test = test[,-c(1,28,31,33,34)]
model_fit= knn(knn_train,test,cl=reg_train$actual_vote,k=10)

submit1 = cbind(test_ori[,1], as.data.frame(model_fit))
write.csv(file = "Insight_IITKgp_1.csv", submit1)

#Tuning the model

# Making the confidence matrix
library(caret)
set.seed(12)
ind = sample(1:nrow(reg_train), round(0.8*nrow(reg_train)))
tune_train = reg_train[ind, ]
tune_train = tune_train[,-which(names(tune_train)%in% c("mvar32", "mvar33"))]
tune_test = reg_train[-ind,]
tune_test = tune_test[,-which(names(tune_test)%in% c("mvar32", "mvar33"))]
res_train = tune_train$actual_vote
res_test = tune_test$actual_vote


tune_train2 = tune_train[,-c(1,28,31,33:35)]
tune_test2 = tune_test[,-c(1,28,31,33:35)]
model_fit2 = knn(tune_train2, tune_test2, cl=tune_train$actual_vote, k=10)
confusionMatrix(model_fit2, res_test)
rm(tune_train2)
rm(tune_test2)
rm(res_train)
rm(res_test)


####   Applying KnnCat

library(knncat)


###     Imputing the empty education to the mode value
reg_train$mvar30[which(reg_train$mvar30=="")] = "Masters"
reg_train$mvar30 = as.factor(as.character(reg_train$mvar30))
test$mvar30[which(test$mvar30 == "")] = "Masters"
test$mvar30  = as.factor(as.character(test$mvar30))

knn_cat_fit = knncat(train = tune_train[,c(29,35)], test = tune_test[,c(29,35)],classcol = 2)

rm(knn_train)
rm(submit1)
rm(test_ori)
rm(train_cv)
rm(ind)



####          Applying decision tree

library(rpart)
library(rpart.plot)


##tree_mod = rpart(actual_vote~., data = tune_train, method = "class", model = TRUE)

library(caret)
####         Tuning of tree with caret package
tc <- trainControl("cv",10)
rpart.grid <- expand.grid(cp=seq(0,0.1,0.01)) 
caret_train = train(actual_vote ~., data=tune_train, method="rpart", 
                    trControl=tc,  tuneGrid=rpart.grid)
rpart_ctr = rpart.control(cp = caret_train$bestTune)
rpart_mod = rpart(actual_vote~., data = tune_train, method = "class",
                  parms = list(split  = "information"),control = rpart_ctr)

library(e1071)
library(randomForest)


best_rf = best.randomForest(x=tune_train[,-c(33)],y=tune_train[,33],importance=TRUE,ntree=100)
rf_pred = predict(best_rf, tune_test[,-33])
best_rpart = best.rpart(actual_vote~.,data=tune_train)
