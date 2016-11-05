library(adabag)
library(rpart)

rpart_ctrl = rpart.control(cp = 0.09)
### boost_train = reg_train[,-c(33,34)]
boost_mod = boosting(actual_vote ~., data = reg_train, boos = TRUE,
                        control = rpart_ctrl)

boost_test = test[,-c(33,34)]
boost_pred = predict.boosting(boost_mod, boost_test, newmfinal = 20)
submit6 = cbind(as.data.frame(test_ori),boost_pred$class)
write.csv(submit6, file = "insight_IITKgp_6.csv", row.names = FALSE)
rm(submit6)

