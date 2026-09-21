data(mtcars)
set.seed(123)
n<-nrow(mtcars)
train_index<-sample(1:n,size=0.8*n)
train_data<-mtcars[train_index,]
test_data<-mtcars[-train_index,]
print(train_data)
print(test_data)
model<-lm(mpg~hp+wt+disp,data=train_data)
print(summary(model))
predictions<-predict(model,newdata=test_data)
actual<-test_data$mpg
rmse<-sqrt(mean((actual-predictions)^2))
mae<-mean(abs(actual-predictions))
r2<-1-sum((actual-predictions)^2)/sum((actual-mean(actual))^2)
results<-data.frame(
  Actual=actual,
  Predicted=predictions
)
print(results)
cat("RMSE =",round(rmse,4),"\n")
cat("MAE =",round(mae,4),"\n")
cat("R-squared =",round(r2,4),"\n")
plot(actual,predictions,
     main="Actual vs Predicted MPG",
     xlab="Actual MPG",
     ylab="Predicted MPG")
abline(0,1)