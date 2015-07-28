##project for data mining 
attach(Datamining)



newdata = subset(Datamining, sex != "I", drop=TRUE);newdata
head(newdata)
set.seed(80)
data<-newdata[sample(1:nrow(newdata), 2835,replace=FALSE),] 
head(data)
## first 60% is Training, 20% to validation and 20% to test 
library(e1071)


trainData <- data[1:1685,,drop = FALSE]  
testData <- data[1686:2256,,drop = FALSE]  ## 2256-2835 for test data
model <- naiveBayes(trainData[,2:9], trainData[,1])   
model
predicted <- predict(model, testData[,-1]);predicted

test<-data[1686:2256,1]
accuracy<-data.frame(predict=predicted,test=test)

####

result=NULL
attach(accuracy)

for (i in 1:length(test)){
  if(predict[i]==test[i]){
  result[i]=1}
  else{
    result[i]=0}

}

sum(result)/length(test) ## Probability of Naive method


## logistic 


class.sex<-NULL
## Transform the sex to 0,1.  Female=0, Male=1
for (i in 1:4177){
  if (data$sex[i] == "M"){class.sex[i]<-1}
    
      if (data$sex[i] == "F"){class.sex[i]<-0}
}
class.sex

newdata1<-data.frame(class=class.sex, length=data$length, 
                    Diam=data$Diameter, height=data$height,
                    whole=data$Wweight, shu=data$Sweight,
                    Vis=data$Vweight, shell=data$Shellweight,
                    rings=data$Rings);head(newdata1)

## first 60% for training data
## 20% for valid
## 20% for test
train.reg<-newdata1[1:1685,]
valid.reg<-newdata1[1686:2256,]
test.reg<-newdata1[2257:2835,]

## logistic regression
glm.out = glm(class ~ rings * length * Diam*height*whole*shu*shell, 
              family=binomial(logit), data=train.reg);glm.out
summary(glm.out)

model<-lm(class~rings+length+Diam+height+whole+shu+shell,data=train.reg);model

summary(model)


############## KNN

train.knn<-data[1:1685,];head(train.reg)
train.knn<-train.knn[order(train.knn$sex),]

length(train.knn[,1])
length(subset(train.knn,sex=='F')[,1]) # M=930 ,F=755

train.knn<-train.knn[, 2:9]

valid.knn<-data[1686:2256,2:9]
test.knn<-data[2257:2835,]
test.knn<-test.knn[order(test.knn$sex),]
real.test<-test.knn[,1]
test.knn<-data[2257:2835,2:9]


length(real.test)

cl <- factor(c( rep("F", 755),rep("M",930)));cl
knn<-knn(train.knn, test.knn, cl, k=4  , prob=TRUE)
knn
length(knn)

for (i in 1:length(real.test)){
  if(knn[i]==real.test[i]){
    result[i]=1}
  else{
    result[i]=0}
  
}

sum(result)/length(real.test) ## Probability of K-NN method



