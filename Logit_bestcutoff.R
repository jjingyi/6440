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
predictedlog

length(predictedlog)
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
### x is 0.5
x<-0.5
result <- predictedlog
result[predictedlog>x]= "M"
result[predictedlog<=x]= "F"


cnfmatlog<-table(result,validData[,1]) ;cnfmatlog
acculog<-sum(diag(cnfmatlog))/sum(cnfmatlog)



############################3Final result################################
# test testDAta

trainData <- data[1:1685,,drop = FALSE]  


testData<-data[2257:2835,, drop=FALSE]  ### test data set
modellog <- glm(sex~.,trainData,family = binomial(logit))   
modellog
predictedlog <- predict(modellog, testData[,-1]);predictedlog


  result <- predictedlog
  result[predictedlog>0.5]= "M"
  result[predictedlog<=0.5]= "F"
  
  
  cnfmatlog<-table(result,testData[,1]) #confusion matrix
  acculog<-sum(diag(cnfmatlog))/sum(cnfmatlog)  # accuracy from confusion matrix
acculog





