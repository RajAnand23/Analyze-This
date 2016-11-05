setwd("./Analyze_it")

#####    Applying Knn
library(class)
library(e1071)

ttl_train = read.csv(file = "Train_reduced_dummy.csv", header = TRUE)
ttl_test = read.csv(file = "Test_reduced_dummy.csv", header = TRUE)

###   Applying the Knn model on the data
knn = knn(ttl_train[,-146],ttl_test,cl=ttl_train$actual_vote,k=20)

####    Applying random Forest
library(randomForest)
rf = randomForest(ttl_train[,-c(146)],y = ttl_train[,146], importance = TRUE,ntree = 200)
#rf = best.randomForest(x = ttl_train[,-146], y = ttl_train[,146], importance= TRUE, ntree = 200)
rf_pred = predict(rf, newdata = ttl_test)
submit = cbind(as.data.frame(test_ori), rf_pred)
write.csv(submit, file = "insight_IITKgp_25.csv", row.names = FALSE)


####  Applying the neuralnetwork
library(neuralnet)
nnnet = neuralnet(actual_vote ~., data = ttl_train, hidden =c(25,10), err.fct = "sse")

#### Applying monlp
library(monmlp)
monlp_mod = monmlp.fit(as.matrix(ttl_train[,-c(146)]), as.matrix(ttl_train[,146]), 15,15,n.trials = 10, bag = FALSE)

submit = cbind(as.data.frame(test_ori), knn)
write.csv(submit,file="insight_IITKgp_19.csv",row.names = FALSE)


#### Applying the SVM
svm_mod = svm(actual_vote ~., data = ttl_train, kernel = "polynomial")
svm_pred = predict(svm_mod , newdata = ttl_test)
submit = cbind(as.data.frame(test_ori), svm_pred)
write.csv(submit,file="insight_IITKgp_26.csv",row.names = FALSE)

###  Tuning the knn
library(class)
knn = tune.knn(x = ttl_train[,-ncol(ttl_train)], y = ttl_train[,ncol(ttl_train)], k = 20:30,
               tunecontrol = tune.control(sampling = "boot"))














