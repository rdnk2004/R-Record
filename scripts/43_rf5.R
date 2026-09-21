library(randomForest)
library(caret)
data(iris)
print(head(iris,10))
str(iris)
summary(iris)
set.seed(123)
folds<-createFolds(iris$Species,k=5,returnTrain=TRUE)
accuracy_values<-numeric(5)
predictions_all<-vector("list",5)
for(i in 1:5){
  train_data<-iris[folds[[i]],]
  test_data<-iris[-folds[[i]],]
  model<-randomForest(
    Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
    data=train_data,
    ntree=100,
    importance=TRUE)
  predictions<-predict(model,test_data)
  cm<-confusionMatrix(predictions,test_data$Species)
  accuracy_values[i]<-as.numeric(cm$overall["Accuracy"])
  predictions_all[[i]]<-list(
    Actual=test_data$Species,
    Predicted=predictions,
    Confusion_Matrix=cm$table
  )}
cat("5-Fold Accuracy:\n")
print(accuracy_values)
cat("Mean Accuracy =",round(mean(accuracy_values),4),"\n")
cat("Standard Deviation =",round(sd(accuracy_values),4),"\n")
final_model<-randomForest(
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  data=iris,
  ntree=100,
  importance=TRUE)
final_predictions<-predict(final_model,iris)
final_cm<-confusionMatrix(final_predictions,iris$Species)
print(final_cm)
cat("Final Accuracy =",round(as.numeric(final_cm$overall["Accuracy"]),4),"\n")
importance_values<-importance(final_model)
print(importance_values)
varImpPlot(final_model,main="Random Forest Variable Importance")
most_important<-rownames(importance_values)[which.max(importance_values[,"MeanDecreaseGini"])]
cat("Most Important Predictor =",most_important,"\n")