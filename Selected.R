other_train = read.csv(file = "Other_train.csv", header = TRUE)
other_test = read.csv(file = "Other_test.csv", header = TRUE)

library(randomForest)
rf = randomForest(other_train[,-35], y = other_train[,35], ntree = 200, mtry = 6,
                  maxnodes = 15, importance = TRUE )

rf_pred = predict(rf, other_test)
submit = cbind(as.data.frame(test_ori), rf_pred)
write.csv(submit, file = "insight_IITKgp_27.csv", row.names = FALSE)

###  Selecting top 20 variables from the randomForest and then applying random forest on them
### on default parameteres
imp_rf = read.csv(file = "Rf_imp.csv", header = TRUE)
imp_names = which(colnames(other_train)%in% imp_rf$X[1:20])
imp_train = other_train[,c(imp_names, 35)]
imp_rf_mod = randomForest(actual_vote ~., data = imp_train)

imp_name_test = which(colnames(other_test)%in% imp_rf$X[1:20])
imp_test = other_test[,c(imp_name_test)]
rf_imp_pred = predict(imp_rf_mod, imp_test)
submit = cbind(as.data.frame(test_ori), rf_imp_pred)
write.csv(submit, file="insight_IITKgp_28.csv", row.names = FALSE)

### Applying Boosting
library(adabag)
