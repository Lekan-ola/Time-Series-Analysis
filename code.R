
# A Time Series Analysis of Infact Mortality in Nigeria
# by Olalekan Igboroodowo


# load packages

library(tidyverse)
library(tseries)
library(forecast)
library(MLmetrics)
library(nnfor)


# bringing in the data now

mortality <- read.csv("infant mortality data.csv")


#exploring dataset

head(mortality)
str(mortality)
dim(mortality)

summary(mortality)


#convert to time series object

mort_ts <- ts(mortality$infant.mortality, start = c(1964), frequency = 1)



# quick plot/ TREND 

autoplot(mort_ts) +
  ggtitle("Timeplot of Infant Mortality Rate in Nigeria") +
  labs(y = "Infant Mortality rate")



#test for normality

hist(mort_ts, main = "Histogram of Infant Mortality Rate",
     xlab = "Mortality Rate")


shapiro.test(mort_ts)

# the tests above indicate non normality, so we take the difference of the data


## difference the data to remove seasonality/trend

mort_diff <- diff(mort_ts)

#plot it

autoplot(mort_diff) +
  ggtitle("First difference of Infant Mortality Rate in Nigeria")+
  labs(y = "Infant Mortality rate")




#test for stationarity

# correlogram of differenced data

#Autocorrelation function plot
acf(mort_diff)

#Partial Autocorrelation function plot
pacf(mort_diff)


# formal test
#unit root test


adf.test(mort_diff), k = 2)


# Time series modelling and forecasting

# In order to adequately measure our models , we separate our data into training 
# and validation data

training <- window(mort_ts, start = c(1964), end = c(2009))
validation<- window(mort_ts, start = c(2010))


# arima model

arima_train <- auto.arima(training, stepwise = FALSE, approximation = FALSE, trace = TRUE )

summary(arima_train)

#forecasting with arima
arima_forecast <- arima_train %>% 
  forecast(h=length(validation))

#plot
arima_forecast%>%
  autoplot() + autolayer(validation)

# accuracy
MAPE(arima_forecast$mean, validation) * 100 

accuracy(arima_forecast, validation)


#2

# Neural Network models

#Feed-forward neural networks

#set seed to ensure reproducibility

set.seed(632)

nn_model <- nnetar(training)
nn_forecast <- forecast(nn_model, h = 10)

#visualize
nn_forecast%>%
  autoplot() + autolayer(validation)

#check accuracy
MAPE(nn_forecast$mean, validation) * 100   # 1.579876
accuracy(nn_forecast, validation)


# The Neural Network model gives the most accurate forecasts

# use the winning model to the predict for the combined data

nn_model_full <- nnetar(mort_ts)
nn_forecast_full <- forecast(nn_model_full, h = 10)

accuracy(nn_forecast_full)

nn_forecast_full%>%
  autoplot() + autolayer(validation)


#Thank you.