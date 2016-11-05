library(monmlp)
setwd("./Analyze_it")
nntrain = read.csv(file = "updated_training_data.csv", header = T)
nntest = read.csv(file = "updated_test_data.csv", header = T)

### Removing the mvar32 and mvar33
nntrain = nntrain[,-c(30,31)]
nntest = nntest[,-c(30,31)]

### Making the x and y for the neural network
nn_x = as.matrix(nntrain[,c(1:43)])
nn_y = as.matrix(nntrain[,c(44:48)])
nntest = as.matrix(nntest)

monlp_mod = monmlp.fit(nn_x, nn_y, 15,15,n.trials = 10, bag = FALSE)

monlp_pred = monmlp.predict(nntest, monlp_mod)
monlp_pred = as.data.frame(monlp_pred)
res = NULL
name_list = c(levels(reg_train$actual_vote))
for(i in 1:nrow(monlp_pred)){
    res[i] = name_list[which.max(monlp_pred[i,])]
}

submit5 = cbind(as.data.frame(test_ori),res)
write.csv(submit5, file = "insight_IITKgp_5.csv", row.names = FALSE)
