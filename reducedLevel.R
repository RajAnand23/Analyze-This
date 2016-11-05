library(e1071)

dum_train = read.csv(file = "updated_training_data.csv", header = TRUE)
dum_test = read.csv(file = "updated_test_data.csv", header = TRUE)

dum_train = cbind(dum_train[,-c(32:36)], "actual_vote" = reg_train$actual_vote)

norm_baye = naiveBayes(actual_vote ~., data = dum_train)
baye_predict = predict(norm_baye, newdata = dum_test, type = "class")

submit15 = cbind(as.data.frame(test_ori), baye_predict)
write.csv(submit10, file = "insight_IITKgp_15.csv", row.names = FALSE)
library(caretEnsemble)

### Applying SVM
library(e1071)
svm2 = svm(x = as.matrix(dum_train[,-c(30,31,46)]), y = dum_train[,46], scale = TRUE, type = "C-classification",
           kernel = "polynomial")
svm_pred = predict(svm1, as.matrix(dum_test[,-c(30,31)]))

submit16 = cbind(as.data.frame(test_ori), svm_pred)
write.csv(submit16, file = "insight_IITKgp_18.csv", row.names = FALSE)

### Taking the top 50 levels and converting the rest to Othes
lev_32 = sort(table(dum_train$mvar32), decreasing = TRUE)
lev_32 = cbind(names(lev_32), as.matrix(lev_32))
lev_32 = as.data.frame(lev_32, row.names = FALSE)
names(lev_32)[1] = "mvar32"
names(lev_32)[2] = "Freq32"
lev_32$mvar32 = as.factor(as.character(lev_32$mvar32))
lev_32$Freq32 = as.integer(as.character(lev_32$Freq32))

lev_33 = sort(table(dum_train$mvar33), decreasing = TRUE)
lev_33 = cbind(names(lev_33), lev_33)
lev_33 = as.data.frame(lev_33, row.names = FALSE)
names(lev_33)[1] = "mvar33"
names(lev_33)[2] = "Freq33"
lev_33$mvar33 = as.factor(as.character(lev_33$mvar33))
lev_33$Freq33 = as.integer(as.character(lev_33$Freq33))

other_train = dum_train
in32 = which(!(other_train$mvar32 %in% lev_32$mvar32[1:50]))
other_train$mvar32 = as.character(other_train$mvar32)
other_train[ in32, 30] = "Other"
other_train$mvar32 = as.factor(other_train$mvar32)

in33 = which(!(other_train$mvar33 %in% lev_33$mvar33[1:50]))
other_train$mvar33 = as.character(other_train$mvar33)
other_train[ in33, 31] = "Other"
other_train$mvar33 = as.factor(other_train$mvar33)

other_test = dum_test
in32_test = which(!(other_test$mvar32 %in% lev_32$mvar32[1:50]))
other_test$mvar32 = as.character(other_test$mvar32)
other_test[ in32_test, 30] = "Other"
other_test$mvar32 = as.factor(other_test$mvar32)

in33_test = which(!(other_test$mvar33 %in% lev_33$mvar33[1:50]))
other_test$mvar33 = as.character(other_test$mvar33)
other_test[ in33_test, 31] = "Other"
other_test$mvar33 = as.factor(other_test$mvar33)

ttl_mvar32 = model.matrix(~mvar32-1,other_train)
ttl_mvar33 = model.matrix(~mvar33-1,other_train)
ttl_train = cbind(other_train[,-c(30,31,46)],ttl_mvar32,ttl_mvar33, other_train$actual_vote)
names(ttl_train)[146] = "actual_vote"

ttl_32T = model.matrix(~mvar32 - 1, other_test)
ttl_33T = model.matrix(~mvar33 - 1, other_test)
ttl_test = cbind(other_test[,-c(30,31)], ttl_32T, ttl_33T) 

write.csv(ttl_train, file = "Train_reduced_dummy.csv", row.names = FALSE)
write.csv(ttl_test, file = "Test_reduced_dummy.csv", row.names = FALSE)


