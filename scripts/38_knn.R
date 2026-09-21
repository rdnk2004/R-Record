library(class)
library(caret)
data(iris)
print(head(iris,10))
str(iris)
summary(iris)
set.seed(123)
train_index<-sample(1:nrow(iris),size=0.8*nrow(iris))
train_data<-iris[train_index,]
test_data<-iris[-train_index,]
train_x<-scale(train_data[,1:4])
test_x<-scale(test_data[,1:4],
              center=attr(train_x,"scaled:center"),
              scale=attr(train_x,"scaled:scale"))
train_y<-train_data$Species
test_y<-test_data$Species
k_values<-c(3,5,7)
accuracy_values<-numeric(length(k_values))
for(i in 1:length(k_values)){
  pred<-knn(train=train_x,test=test_x,cl=train_y,k=k_values[i])
  accuracy_values[i]<-mean(pred==test_y)
  cat("K =",k_values[i],"Accuracy =",round(accuracy_values[i],4),"\n")
}
results<-data.frame(
  K=k_values,
  Accuracy=accuracy_values
)
print(results)
best_k<-k_values[which.max(accuracy_values)]
cat("Best K =",best_k,"\n")
best_predictions<-knn(
  train=train_x,
  test=test_x,
  cl=train_y,
  k=best_k
)
confusion_matrix<-confusionMatrix(best_predictions,test_y)
print(confusion_matrix)
accuracy<-confusion_matrix$overall["Accuracy"]
precision<-mean(confusion_matrix$byClass[,"Precision"],na.rm=TRUE)
recall<-mean(confusion_matrix$byClass[,"Recall"],na.rm=TRUE)
f1<-mean(confusion_matrix$byClass[,"F1"],na.rm=TRUE)
cat("Best Model Accuracy =",round(accuracy,4),"\n")
cat("Precision =",round(precision,4),"\n")
cat("Recall =",round(recall,4),"\n")
cat("F1-Score =",round(f1,4),"\n")