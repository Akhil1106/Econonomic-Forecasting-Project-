data <- read.csv("Desktop/ECON/Project /Retailsales.csv")
View(data)

autoplot(data)
rlang::last_error()

ggplot(data)
summary(data)

#We can tell that this is monthly data and that it begins in 1992. 
#Let's turn the value column into a time series object with the function ts() and then we will print it out.
sales <- ts(data$value)
sales

#Since we know that this is monthly data and it begins in 1992 let's update the frequency and start arguments.

sales <- ts(data$value, frequency = 12, start = 1992)
sales

autoplot(sales, main = "Furniture and Home Furnishing Sales: 1992-2022")

ggseasonplot(sales, main = "Seasonal Plot: Furniture Sales")

ggtsdisplay(sales)
#Simple Exponential Smoothing
autoplot(ses(sales), PI = FALSE)
summary(ses(sales), PI = FALSE)

#Holt's Linear Trend

autoplot(holt(sales), PI = FALSE)
summary(holt(sales), PI = FALSE)

#Dampened Holt's Method

autoplot(holt(sales, damped = TRUE, h = 36), PI = FALSE)
summary(holt(sales, damped = TRUE, h = 36), PI = FALSE)

# holt's winter seasonality 

autoplot(sales) + 
  autolayer(hw(sales, seasonal = "multiplicative", PI=FALSE)) + 
  autolayer(hw(sales, seasonal = "additive", PI=FALSE), col="Red")


# ETS 

autoplot(forecast(ets(sales), h = 12, PI=FALSE))

autoplot(ets(sales))

summary(ets(sales))

autoplot(forecast(ets(sales), h = 12, PI=FALSE))
#ARIMA 

autoplot(forecast(Arima(sales, order=c(1,1,1)), h = 12), PI = FALSE)

summary(Arima(sales, order=c(1,1,1)), PI = FALSE)

autoplot(forecast(Arima(sales, order=c(2,1,1)), h = 12), PI = FALSE)

autoplot(forecast(Arima(sales, order=c(2,1,0)), h = 12), PI = FALSE)

mod1 = Arima(sales,order=c(1,2,0),lambda = 0)

summary(mod1)

new = Arima(sales,order=c(1,1,0),lambda = 0)
summary(new)

auto.arima(sales,lambda=0)

autoplot(forecast(auto.arima(sales), h = 12), PI = FALSE)

summary(forecast(auto.arima(sales), h = 12), PI = FALSE)

