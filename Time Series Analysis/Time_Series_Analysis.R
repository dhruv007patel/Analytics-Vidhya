library(tidyr)
library(dplyr)
library(forecast)
library(TTR)
library(Metrics)
library(dplyr)
library(zoo)
library(ggplot2)
library(readr)
library(xts)
library(smooth)
library(caret)

train <- read_csv('train.csv')
test <-read_csv('test.csv')

time_index <- seq(from = as.POSIXct("2012-08-25 00:00"), to = as.POSIXct("2014-09-25 23:00"), by = "hour")
train_hr <- xts(train$Count, order.by = time_index)
head(train_hr)
tail(train_hr)


plot(train_hr)

x <- msts(train$Count, seasonal.periods=c(24,24*7,365.25*24), start=2012+34/52)
plot(x)

stl1 <- stl(x,s.window = "periodic")
plot(stl1)

x1 <- ts(train$Count, frequency=24*1)
x2 <- ts(train$Count, frequency=24*7)
x3 <- ts(train$Count, frequency=24*365)


CES_auto2 <- auto.ces(x2)
f2 <- forecast(CES_auto2,h=nrow(test))
test_op2 <- data.frame(test$ID, f2$mean)
colnames(test_op2) <- c("ID","Count")

