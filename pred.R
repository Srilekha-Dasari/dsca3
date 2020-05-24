#give the file path and save the file in project folder
fpath2 <- file.path("galwaydata.csv")
#To import data in csv format
gdf1 <- read.csv(fpath2, header=TRUE, stringsAsFactors = FALSE, na.strings="" )

#displaying the structure of the data frame
str(gdf1)

temperature <- c(gdf1$temp)

class(temperature)
library(tseries)
#Creating time series object
myts <- ts(gdf1$temp, start=c(2004, 6),end = c(2017,12), frequency=12)
myts
#Show the cycle of ts object
cycle(myts)
#Plot the ts using the plot()
plot(myts,xlab = "year", ylab = "Temperature",
     main = "montly temperature between 2004 and 2017", col ="blue")

plot.ts(myts)
#information of timeseries object
start(myts)
end(myts)
frequency(myts)
class(myts)
print(summary(myts))
# View the records with NA
na_records <- myts[!complete.cases(myts)]
sum(na_records)
options(repr.plot.width=14, repr.plot.height=6)

# Show data using a plot() function
plot(myts,
     xlab="Year", 
     ylab = "Temperature",
     main="Temperature from 2004  to 2017", col = "blue")
# add a straight line to show the linearity
# between passenger numbers and time
abline(reg=lm(myts~time(myts)))
#calculate mean and Check for seasonality with trend
plot(aggregate(myts,FUN=mean))
#examine any seasonal effects within the data using a boxplot
boxplot(myts ~ cycle(myts),
        xlab="Date", 
        ylab = "Temperature" ,
        main ="Monthly Temperature Boxplot from 2004 to 2017",
        col="orange",
        border="brown")
seasonal_decomposition <- stl(myts, s.window = "periodic")
plot(seasonal_decomposition)



#adf test
adf.test(myts, alternative = "stationary")
kpss.test(myts)
diff_data <- diff(myts, lag = 12, differences =1)
adf.test(diff_data)
kpss.test(diff_data)

plot(diff_data)

#adf.test(myts, alternative = "stationary", k = 30)
library(forecast)
#auto correlation and choosing model order
#Acf displays correlation b/w series and its lags
Acf(myts)
#Pacf dispalys correlation b/w series and its lags that 
#explained by previous lags
Pacf(myts)
Acf(diff_data)
Pacf(diff_data)
library(forecast)
ndiffs(myts)
nsdiffs(myts)
plot(ma(myts,12),main="Simple Moving Averages k=3")

#building seasonal ARIMA model.
fit <- arima(myts, 
             c(1,1,6), 
             seasonal = list(order = c(1,1,5), 
                             period = 12))


fit

prediction <- predict(fit, n.ahead = 3 * 12)
prediction

#generate forecast
forecast_myts <- forecast(fit, level = c(95), h = 36)
forecast_myts
autoplot(forecast_myts)
plot(forecast(forecast_myts, 3), xlab = "Year", ylab = "Annual Temperatures")
auto_arima_model <- auto.arima(myts)
auto_arima_model
accuracy(auto_arima_model)
accuracy(fit)
#comparisions
plot(forecast(auto_arima_model, 3 * 12), xlab = "Year", ylab = "Annual Temperatures")
plot(forecast(fit, 3 * 12), xlab = "Year", ylab = "Annual Flow")
#Evaluating the models
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)
# Box.test() function tests that the autocorrelations are all zero.
# p > 0.05 so fail to reject the null hypothesis This ARIMA model appears to fit the data well
Box.test(auto_arima_model$residuals, type = "Ljung-Box")
#apply the same tests to the manually selected model.
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
#assess the predicted values versus actual values from the dataset
#need to split the data into a train/test split
library(TSstudio)

split_myts <- ts_split(ts.obj = myts, sample.out = 36)

training <- split_myts$train
testing <- split_myts$test

paste("length of myts time series:" , length(myts))

paste("length of training data:" , length(split_myts$train))

paste("length of testing data:" , length(split_myts$test))
training
testing
library(forecast)      
myts_model <- auto.arima(training)
myts_model
predict_auto_ARIMA <- forecast(myts_model, 3 * 12)
predict_auto_ARIMA
precict_manual_ARIMA <- forecast(fit, 3 * 12)
precict_manual_ARIMA
#one step forecast on test data
myts_test <- Arima(testing, model=myts_model)
accuracy(myts_test)

#repeating using manual arima model
myts_manual_ARIMA <- arima(training, 
                           c(1,1,6), 
                           seasonal = list(order = c(1,1,5), 
                                           period = 12))
myts_manual_ARIMA

myts_manual_test <- Arima(testing, model=myts_manual_ARIMA)
accuracy(myts_manual_test)
#compare aic manual  and auto arima aic 
#auto arima is better than manual
mytsprediction <- predict(myts_manual_ARIMA, n.ahead = 3 * 12)
mytsprediction$pred

#testing
# make actuals_predicted dataframe
# for auto ARIMA
actuals_predictions <- data.frame(cbind(actuals = testing, predicted = predict_auto_ARIMA))
head(actuals_predictions)

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
# make manual_predicted dataframe
# for ARIMA model
manual_predictions <- data.frame(cbind(manuals = testing, predicted = predict_manual_ARIMA))
head(manual_predictions)

correlation_accuracy2 <- cor(manual_predictions)
correlation_accuracy2




