###### project for data mining----4 methods-----60%train, 20%valid, 205 test 

attach(Datamining)
pairs(Datamining)

set.seed(80)
newdata = subset(Datamining, sex != "I", drop=TRUE)
newdata$sex = factor(as.character(newdata$sex)) #dropping I from levels

##  randomize sequence
data<-newdata[sample(1:nrow(newdata), 2835,replace=FALSE),] 

## delect highly correlated variable and delete infants for Naive
data.naive<-data.frame(data$sex, data$height, data$Sweight, data$Rings)

# correlation plot 
pairs(data.naive)

## first 60% is Training, 20% to validation and 20% to test 
library(e1071)

#### NaiveBayes
trainData <- data.naive[1:1685,,drop = FALSE]  
validData <- data.naive[1686:2256,,drop = FALSE]  ## 2256-2835 for test data
### final test data(for test)
testData<-data.naive[2257:2835,,drop=FALSE]
model <- naiveBayes(trainData[,2:4], trainData[,1])   
model
summary(model)
predicted <- predict(model, validData[,-1]);predicted


########### compare the result with prediction

cnfmatlog<-table(predicted,validData[,1]) #confusion matrix
power.naive<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matri
power.naive

### Cart
library(rpart)
#length+Diameter+height+Wweight+Sweight+Vweight+Shellweight+Rings
trainData <- data[1:1685,,drop = FALSE]  
validData <- data[1686:2256,,drop = FALSE]  ## 2256-2835 for test data
fit <- rpart(sex ~ .,method="class", data=trainData)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
plot(fit, uniform=TRUE,
     main="Classification Tree for Abalone")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
post(fit, file = "C:/Users/cdelong/Desktop/newtree.ps",
     title = "Classification Tree for Abalone")

## Prune Tree
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
table(pfit)




# read by column
table<-table(predict(fit, validData, type = "class"), validData[, "sex"]);table
table.p<-table(predict(pfit, validData, type = "class"), validData[, "sex"]);table.p

power.tree<-(table[1,1]+table[2,2])/ (table[1,1]+table[2,2]+table[1,2]+ table[2,1]);power.tree

prune.tree<-307/(264+307);prune.tree

############## KNN
trainData <- data[1:1685,,drop = FALSE]  
validData <- data[1686:2256,,drop = FALSE]  ## 2256-2835 for test data

train.knn<-trainData
train.knn<-train.knn[order(train.knn$sex),]


length(subset(train.knn,sex=='F')[,1]) # M=930 ,F=755

train.knn<-train.knn[,2:9]
valid.knn<-validData[,2:9]

cl <- factor(c( rep("F", 755),rep("M",930)))
knn<-knn(train.knn, valid.knn, cl, k=7  , prob=FALSE);knn


## check the accuracy
cnfmatlog<-table(knn, validData[,1]) #confusion matrix
power.knn<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matri
power.knn

cbind(Naive=power.naive, Tree=power.tree, Knn=power.knn)
########## Final result 

cnfmatlog<-table(predicted,validData[,1]) #confusion matrix
power.naive<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matri
power.naive

### Cart
library(rpart)
#length+Diameter+height+Wweight+Sweight+Vweight+Shellweight+Rings
trainData <- data[1:1685,,drop = FALSE]  

testData<-data[2257:2835,, drop=FALSE]  ### test data set
fit <- rpart(sex ~ .,method="class", data=trainData)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
plot(fit, uniform=TRUE,
     main="Classification Tree for Abalone")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
post(fit, file = "C:/Users/cdelong/Desktop/newtree.ps",
     title = "Classification Tree for Abalone")

# read by column
table<-table(predict(fit, validData, type = "class"), validData[, "sex"]);table
power.tree<-(table[1,1]+table[2,2])/ (table[1,1]+table[2,2]+table[1,2]+ table[2,1]);power.tree


##############Logistic Regreesion 

##project for data mining ----Logistic regression
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

trainData <- data[1:1685,,drop = FALSE]  
validData <- data[1686:2256,,drop = FALSE]  ## 2257-2835 for validation data

testData<-data[2257:2835,, drop=FALSE]  ### test data set
modellog <- glm(sex~.,trainData,family = binomial(logit))   
modellog
predictedlog <- predict(modellog, validData[,-1],type='response');predictedlog

Max<-function(x){
  result <- predictedlog
  result[predictedlog>x]= "M"
  result[predictedlog<=x]= "F"
  
  
  cnfmatlog<-table(result,validData[,1]) #confusion matrix
  acculog<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matrix
  return(acculog)
}


s<-seq(0, 1 ,by=0.1)
ans=NULL
for (i in 1:length(s)){ans[i]=Max(s[i])}
max(ans)
s[which(ans==max(ans))]    ##0.5
############################3Final result################################
# test testDAta

trainData <- data[1:1685,,drop = FALSE]  


testData<-data[2257:2835,, drop=FALSE]  ### test data set
modellog <- glm(sex~.,trainData,family = binomial(logit))   
modellog
predictedlog <- predict(modellog, testData[,-1]);predictedlog


result <- predictedlog
result[predictedlog>0.02]= "M"
result[predictedlog<=0.02]= "F"


cnfmatlog<-table(result,testData[,1]) #confusion matrix
acculog<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matrix
acculog




