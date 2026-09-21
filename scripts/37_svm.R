library(e1071)
library(caret)
data(iris)
print(head(iris,10))
str(iris)
summary(iris)
set.seed(123)
train_index<-sample(1:nrow(iris),size=0.8*nrow(iris))
train_data<-iris[train_index,]
test_data<-iris[-train_index,]
x_train<-scale(train_data[,1:4])
x_test<-scale(test_data[,1:4],
              center=attr(x_train,"scaled:center"),
              scale=attr(x_train,"scaled:scale"))
train_svm<-data.frame(x_train,Species=train_data$Species)
test_svm<-data.frame(x_test,Species=test_data$Species)
model<-svm(Species~.,data=train_svm,kernel="radial")
print(model)
predictions<-predict(model,test_svm)
confusion_matrix<-confusionMatrix(predictions,test_svm$Species)
print(confusion_matrix)
accuracy<-confusion_matrix$overall["Accuracy"]
precision<-mean(confusion_matrix$byClass[,"Precision"],na.rm=TRUE)
recall<-mean(confusion_matrix$byClass[,"Recall"],na.rm=TRUE)
f1<-mean(confusion_matrix$byClass[,"F1"],na.rm=TRUE)
cat("Accuracy =",round(accuracy,4),"\n")
cat("Precision =",round(precision,4),"\n")
cat("Recall =",round(recall,4),"\n")
cat("F1-Score =",round(f1,4),"\n")