library(caret)
library(pROC)
data(mtcars)
mtcars$am<-factor(mtcars$am,levels=c(0,1),labels=c("Automatic","Manual"))
print(head(mtcars,10))
str(mtcars)
summary(mtcars)
set.seed(123)
train_index<-sample(1:nrow(mtcars),size=0.8*nrow(mtcars))
train_data<-mtcars[train_index,]
test_data<-mtcars[-train_index,]
model<-glm(am~mpg+hp+wt,data=train_data,family=binomial)
print(summary(model))
probabilities<-predict(model,newdata=test_data,type="response")
predictions<-ifelse(probabilities>=0.5,"Manual","Automatic")
predictions<-factor(predictions,levels=levels(test_data$am))
confusion_matrix<-confusionMatrix(predictions,test_data$am)
print(confusion_matrix)
accuracy<-confusion_matrix$overall["Accuracy"]
precision<-confusion_matrix$byClass["Precision"]
recall<-confusion_matrix$byClass["Recall"]
f1<-confusion_matrix$byClass["F1"]
cat("Accuracy =",round(accuracy,4),"\n")
cat("Precision =",round(precision,4),"\n")
cat("Recall =",round(recall,4),"\n")
cat("F1-Score =",round(f1,4),"\n")
roc_curve<-roc(test_data$am,probabilities,levels=c("Automatic","Manual"),direction="<")
plot(roc_curve,main="ROC Curve")
auc_value<-auc(roc_curve)
cat("AUC =",round(auc_value,4),"\n")
print(coef(model))
print(exp(coef(model)))