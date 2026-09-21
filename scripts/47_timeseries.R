library(forecast)
library(ggplot2)
data("AirPassengers")
ts_data<-AirPassengers
print(ts_data)
summary(ts_data)
plot(ts_data,main="Monthly Air Passengers",xlab="Year",ylab="Passengers")
decomp<-decompose(ts_data)
plot(decomp)
train<-window(ts_data,end=c(1958,12))
test<-window(ts_data,start=c(1959,1))
naive_model<-snaive(train,h=length(test))
holt_model<-holt(train,h=length(test))
arima_model<-auto.arima(train)
arima_forecast<-forecast(arima_model,h=length(test))
naive_pred<-naive_model$mean
holt_pred<-holt_model$mean
arima_pred<-arima_forecast$mean
mae<-function(actual,pred) mean(abs(actual-pred))
rmse<-function(actual,pred) sqrt(mean((actual-pred)^2))
mape<-function(actual,pred) mean(abs((actual-pred)/actual))*100
results<-data.frame(
  Model=c("Seasonal Naive","Holt","ARIMA"),
  MAE=c(mae(test,naive_pred),mae(test,holt_pred),mae(test,arima_pred)),
  RMSE=c(rmse(test,naive_pred),rmse(test,holt_pred),rmse(test,arima_pred)),
  MAPE=c(mape(test,naive_pred),mape(test,holt_pred),mape(test,arima_pred)))
print(results)
comparison<-ts.union(Actual=test,Seasonal_Naive=naive_pred,Holt=holt_pred,ARIMA=arima_pred)
plot(comparison,main="Actual vs Forecasted Passengers")
final_model<-auto.arima(ts_data)
future_forecast<-forecast(final_model,h=12)
print(future_forecast)
plot(future_forecast,main="12-Month Passenger Forecast",xlab="Year",ylab="Passengers")
print(future_forecast$mean)
cat("Average Forecast =",round(mean(future_forecast$mean),2),"\n")
cat("Maximum Forecast =",round(max(future_forecast$mean),2),"\n")
cat("Minimum Forecast =",round(min(future_forecast$mean),2),"\n")