library(randomForest)
library(caret)
data(iris)
print(head(iris,10))
str(iris)
summary(iris)
set.seed(123)
train_index<-sample(1:nrow(iris),size=0.8*nrow(iris))
train_data<-iris[train_index,]
test_data<-iris[-train_index,]
model<-randomForest(
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  data=train_data,
  ntree=100,
  importance=TRUE
)
print(model)
predictions<-predict(model,test_data)
confusion_matrix<-confusionMatrix(predictions,test_data$Species)
print(confusion_matrix)
accuracy<-confusion_matrix$overall["Accuracy"]
precision<-mean(confusion_matrix$byClass[,"Precision"],na.rm=TRUE)
recall<-mean(confusion_matrix$byClass[,"Recall"],na.rm=TRUE)
f1<-mean(confusion_matrix$byClass[,"F1"],na.rm=TRUE)
cat("Accuracy =",round(accuracy,4),"\n")
cat("Precision =",round(precision,4),"\n")
cat("Recall =",round(recall,4),"\n")
cat("F1-Score =",round(f1,4),"\n")
importance_values<-importance(model)
print(importance_values)
varImpPlot(model,main="Random Forest Variable Importance")
most_important<-rownames(importance_values)[which.max(importance_values[,"MeanDecreaseGini"])]
cat("Most Important Predictor =",most_important,"\n")