rm(list=ls())

library(forecast)
library(ggplot2)
library(gridExtra)

#Reading the data
data <- read.csv("data.csv")
data.ts <- ts(data$price, start = c(2013,1), end = c(2022,12), freq = 12)
ts <- data.frame(date = time(data.ts), price = as.numeric(data.ts))
#Plotting time-series for crude-oil between 2013-2022
dev.off()
ggplot(ts, aes(x = date, y = price)) +
  geom_line() +
  xlab("Date") +
  ylab("Price") +
  ggtitle("Crude Oil Prices from 2013 to 2022")
#Paritioning the data in training and validation
data.ts.a <- window(data.ts, start = c(2013,1), end = c(2020,12)) 
data.ts.b <- window(data.ts, start = c(2021,1), end = c(2022,12))


##Regression Models##
#Regression Model with trend & season (most accurate)
data.ts.lms <- tslm(data.ts.a ~ trend + season)
data.ts.lms.pred <- forecast(data.ts.lms, h = 24)
lms.pred <- data.frame(date = time(data.ts.lms.pred$mean), price = data.ts.lms.pred$mean)
#Regression Model with only trend
data.ts.lm <- tslm(data.ts.a ~ trend)
data.ts.lm.pred <- forecast(data.ts.lm, h = 24)
lm.pred <- data.frame(date = time(data.ts.lm.pred$mean), price = data.ts.lm.pred$mean)

#Time-series plot with both regression models
lms <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = lms.pred, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1)+
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("Trend & Season Linear Regression Models for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
lm <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = lm.pred, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("Trend Linear Regression Models for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
grid.arrange(lms,lm, ncol = 2)
#Accuracies
accuracy(data.ts.lms.pred$mean,data.ts.b); accuracy(data.ts.lm.pred$mean,data.ts.b)


##Naive forecast##
#Naive Forecast
data.naive <- naive(data.ts.a, h=24)
naive.pred <- data.frame(date = time(data.naive$mean), price = data.naive$mean)
#Seasonal Naive Forecast
data.snaive <- snaive(data.ts.a, h=24)
snaive.pred <- data.frame(date = time(data.snaive$mean), price = data.snaive$mean)

#Time-series plot with Naive forecasting models
naive <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = naive.pred, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1)+
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("Naive Forecasting for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
snaive <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = snaive.pred, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1)+
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("Seasonal Naive Forecasting for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
grid.arrange(naive, snaive, ncol = 2)
#Accuracies
accuracy(data.naive$mean,data.ts.b); accuracy(data.snaive$mean,data.ts.b)


##ARIMA & Exponential Smoothing
#ARIMA model
data.ts.arima <- auto.arima(data.ts.a)
data.ts.arima.pred <- forecast(data.ts.arima, h = length(data.ts.b));
arima <- data.frame(date = time(data.ts.arima.pred$mean), price = data.ts.arima.pred$mean)
#Exponential Smoothing
data.ts.e <- ets(data.ts.a, model = "AAA")
data.ts.e.pred <- forecast(data.ts.e, h = length(data.ts.b)); 
es <- data.frame(date = time(data.ts.e.pred$mean), price = data.ts.e.pred$mean)

#Time-series plot with ARIMA & Exponential Smoothing forecasting models
ari <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = arima, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1)+
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("ARIMA model for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
exp <- ggplot() +
  geom_line(data = ts, aes(x = date, y = price), color = "blue") +
  geom_line(data = es, aes(x = date, y = price), color = "red", linetype = "dashed", linewidth = 1) +
  xlab("Date") +
  ylab("Price") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black")+
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  ggtitle("Exponential Smoothing model for Crude Oil Price Prediction") +
  annotate("text", x = 2017, y = max(ts$price), label = "Training Data")+
  annotate("text", x = 2022, y = max(ts$price), label = "Validation Data")+
  theme_bw()
grid.arrange(ari, exp, ncol = 2)
#Accuracies
accuracy(data.ts.arima.pred$mean,data.ts.b); accuracy(data.ts.e.pred$mean,data.ts.b)


