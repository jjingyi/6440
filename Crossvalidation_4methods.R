##project for data mining   ##### cross Validation ## Selece the best method
attach(Datamining)
#training<-Datamining[1:500,]
#valid<-Datamining[501:1000,];valid
max(Datamining$Rings)
length(Datamining$Rings)
which(Datamining$Rings=='6')

newdata = subset(Datamining, sex != "I", drop=TRUE);newdata
newdata$sex = factor(as.character(newdata$sex)) #dropping I from levels

set.seed(80)
data<-newdata[sample(1:nrow(newdata), 2835,replace=FALSE),] 

## first 60% is Training, 20% to validation and 20% to test 
library(e1071)

#################Naivebayes Method


### cross validation
Naive<-function(p){
  data<-data.frame(data$sex, data$height, data$Sweight, data$Rings)
  trainData <- data[1:(85+100*p),,drop = FALSE]  
  validData <- data[(86+100*p):2256,,drop = FALSE] 
  ### final test data(for test)
  model <- naiveBayes(trainData[,2:3], trainData[,1])   
  predicted <- predict(model, validData[,-1]);predicted
  ########### compare thr result with prediction
  valid<-data[(86+100*p):2256,1]
  
  cnfmatlog<-table(predicted,validData[,1]) #confusion matrix
  accu<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matrix
  
  return(accu)
}
# Cross Validation:

power.naive=NULL
for (i in 1:10){power.naive[i]=Naive(i)}
naive<-mean(power.naive) 



##############Logistic Regression


Logit<-function(p){
  trainData <- data[1:(85+100*p),,drop = FALSE]  
  validData <- data[(86+100*p):2256,,drop = FALSE]  ## 2257-2835 for validation data
  modellog <- glm(sex~.,trainData,family = binomial(logit))   
  predictedlog <- predict(modellog, validData[,-1],type='response')
  
  Max<-function(x){
    result <- predictedlog
    result[predictedlog>x]= "M"
    result[predictedlog<=x]= "F"
  
  cnfmatlog<-table(result,validData[,1]) #confusion matrix
  acculog<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matrix
  return(acculog)
  }
  s<-seq(0, 1 ,by=0.01)
  ans=NULL
  for (i in 1:length(s)){ans[i]=Max(s[i])}
  result<-max(ans)   
  return(result)
}

power.logit=NULL
for (i in 1:10){power.logit[i]=Logit(i)}
logit<-mean(power.logit) 

##### From Cross validation method, we found out the avergaed power for Logistic method is 0.562

##########################Cross Validation for Trees

Tree<-function(p){
  trainData <- data[1:(85+100*p),,drop = FALSE]  
  validData <- data[(86+100*p):2256,,drop = FALSE]  ## 2256-2835 for test data
  fit1 <- rpart(sex ~ ., data = trainData )
  pfit<- prune(fit1, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  
  # read by column
  table<-table(predict(pfit, validData, type = "class"), validData[, "sex"])
  power<- (table[1,1]+table[2,2])/ (table[1,1]+table[2,2]+table[1,2]+ table[2,1])
  
  return(power)

}

power.tree=NULL
for (i in 1:10){power.tree[i]=Tree(i)}
tree<-mean(power.tree) 
#################### K-NN k=7
KNN<-function(p){
  trainData <- data[1:(85+100*p),,drop = FALSE]  
  validData <- data[(86+100*p):2256,,drop = FALSE]  ## 2256-2835 for test data
  train.knn<-trainData
  train.knn<-train.knn[order(train.knn$sex),]
  f=length(subset(train.knn,sex=='F')[,1]) # M=930 ,F=755
  m=length(subset(train.knn,sex=='M')[,1]) 
  train.knn<-train.knn[,2:9]
  valid.knn<-validData[,2:9]

  cl <- factor(c( rep("F", f),rep("M",m)))
  knn<-knn(train.knn, valid.knn, cl, k=7, prob=FALSE);knn

  ## check the accuracy
  cnfmatlog<-table(knn, validData[,1]) #confusion matrix
  accu<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matri
  return(accu)

}

power.knn=NULL
for (i in 1:10){power.knn[i]=KNN(i)}
knn<-mean(power.knn) 


  ##########################################################
  ##########################Comparison######################
  #########################################################
cbind(Naive.power=naive, Logistic.power=logit, Tree.power=tree, Knn.power=knn)

