library(caret)
setwd("./Analyze_it")

library(rpart)
rpart_model_mvar32 = rpart(mvar32 ~., data = reg_train[,-c(34,35)], method = "class")
rpart_model_mvar33 = rpart(mvar33 ~., data = reg_train[,-c(33,35)], method = "class")
new_33 = which(!(test$mvar33 %in% levels(reg_train$mvar33)))
new_32 = which(!(test$mvar32 %in% levels(reg_train$mvar32)))
test_data_mvar32 = test[new_32,-c(33,34)]
test_data_mvar33 = test[new_33,-c(33,34)]
pred_mvar32 = predict(rpart_model_mvar32,newdata = test_data_mvar32, type ="class")
pred_mvar33 = predict(rpart_model_mvar33,newdata = test_data_mvar33, type = "class")
test[new_32,33]=pred_mvar32
test[new_33,34]=pred_mvar33
corrected_test = cbind(as.data.frame(test_ori), test)
write.csv(corrected_test, file = "Test2.csv", row.names = FALSE)

