library(e1071)
library(ROCR)
library(corrplot)
############Naive Bayes#####################################
m.nb <- naiveBayes(LEAD_STATUS~., data = train)
pred.nb = predict(m.nb, validation[,-20],type="raw")

m.nb
plot(dm)
View(pred.nb)
model$nb_notRenewed = pred[,1]
pred[,1]
model$nb_Renewed = pred[,2]
a=table(pred, validation[,20]);a
(a[1,1]+a[2,2])/sum(a)
names(pred)

########graphing###############################################################
########ROC CURVE####################################################
score <- pred.nb[,c("Renewed")]
actual.class <- validation$LEAD_STATUS =='Renewed'
score <- (score + runif(length(score))) / 2
pred <- prediction(score, actual.class)
perf <- performance(pred, "prec", "rec")
plot(perf)

#######AUC Area Under Curve#######################################
perf <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
plot(perf)
legend(0.6,0.3,c(c(paste('AUC is', auc)),"\n"), border="white",cex=1.0,box.col = "white");

######find the best Cutoff######################
perf <- performance(pred, "cost", cost.fp=4, cost.fn=1)
plot(perf)

