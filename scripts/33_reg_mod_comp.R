data(mtcars)
model1<-lm(mpg~hp,data=mtcars)
model2<-lm(mpg~hp+wt+disp,data=mtcars)
summary(model1)
summary(model2)
pred1<-predict(model1,mtcars)
pred2<-predict(model2,mtcars)
rmse1<-sqrt(mean((mtcars$mpg-pred1)^2))
rmse2<-sqrt(mean((mtcars$mpg-pred2)^2))
mae1<-mean(abs(mtcars$mpg-pred1))
mae2<-mean(abs(mtcars$mpg-pred2))
r2_1<-summary(model1)$r.squared
r2_2<-summary(model2)$r.squared
adj_r2_1<-summary(model1)$adj.r.squared
adj_r2_2<-summary(model2)$adj.r.squared
comparison<-data.frame(
  Model=c("Model 1","Model 2"),
  R_Squared=c(r2_1,r2_2),
  Adjusted_R_Squared=c(adj_r2_1,adj_r2_2),
  RMSE=c(rmse1,rmse2),
  MAE=c(mae1,mae2)
)
print(comparison)
cat("Model 1 R-squared =",round(r2_1,4),"\n")
cat("Model 1 Adjusted R-squared =",round(adj_r2_1,4),"\n")
cat("Model 1 RMSE =",round(rmse1,4),"\n")
cat("Model 1 MAE =",round(mae1,4),"\n\n")
cat("Model 2 R-squared =",round(r2_2,4),"\n")
cat("Model 2 Adjusted R-squared =",round(adj_r2_2,4),"\n")
cat("Model 2 RMSE =",round(rmse2,4),"\n")
cat("Model 2 MAE =",round(mae2,4),"\n")
