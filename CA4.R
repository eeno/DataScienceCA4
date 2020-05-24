#improting data-----------------------------------------

#import plannning permission data
planning_permission <- read.csv("Planning_permission_clean.csv")
# check structure
str(planning_permission$DecisionDate)

#convert data ----------------------------------------------------------------

#convert decsion date to date fromat
planning_permission$DecisionDate <- as.Date(planning_permission$DecisionDate, format = "%Y-%m-%d")

#create a dataframe of du sate and the grabted indicator 
ts_planning_permission <- planning_permission[, c(9,17)]

#create a new planning permission dataset aggragating number of granted application based on month of decision date
#first need to convert decision date column to a column of just month and year
ts_planning_permission$DecisionDate <- format(ts_planning_permission$DecisionDate, "%Y-%m")


#aggregate the granted data based on month and year
ts_planning_permission <- aggregate(granted ~ DecisionDate,ts_planning_permission,  FUN = sum)

#creating a time series
ts_planning_permission <- ts(ts_planning_permission$granted,start = c(2000,1), frequency = 12)
ts_planning_permission

#checking cycle
cycle(ts_planning_permission)

#plotting time series
plot(ts_planning_permission)

#check the start of a time series
start(ts_planning_permission)
#check the end
end(ts_planning_permission)
#check frequncy
frequency(ts_planning_permission)
#check the class
class(ts_planning_permission)
#check summary
summary(ts_planning_permission)


#processinng tiome series---------------------------------------------------------------
#install 
install.packages("forecast")
library(forecast)


#checking for a trned in data
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(ts_planning_permission), max(ts_planning_permission))
plot(ts_planning_permission, main="Raw time series")
# ma() function used to smooth the Nile time series
plot(ma(ts_planning_permission, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(ts_planning_permission, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(ts_planning_permission, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)
# As k increases, plot becomes increasingly smooth




plot(ts_planning_permission,
     xlab="Date", 
     ylab = "Number of planning Permissions granted",
     main="Planning permissions granted from Jan 2000 - Dec 2019 ")
# Add a straight line shwing the linear relationship
# between granted planning permission applications  and time
abline(reg=lm(ts_planning_permission~time(ts_planning_permission)))


plot(aggregate(ts_planning_permission,FUN=mean))


#create a box plot ot check for season ality
boxplot(ts_planning_permission ~ cycle(ts_planning_permission),
        xlab="Date", 
        ylab = "Number of planning Permissions granted" ,
        main ="Planning permissions granted from Jan 2000 - Dec 2019 ")

#perform seaosn decomposition
seasonal_decomposition <- stl(ts_planning_permission, s.window="period")
#plot the decomposition
plot(seasonal_decomposition)


#Test if a time series is stationary
#by using the dickey fuller test
# p-value < 0.05 indicates the TS is stationary


library(tseries)
suggested_k <- trunc((length(ts_planning_permission)-1)^(1/3))
suggested_k


adf.test(ts_planning_permission, alternative = "stationary")




#Method 2 : Test stationarity of the time series (Autocorrelation)
library(forecast)
acf(ts_planning_permission)


pacf(ts_planning_permission)



# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_planning_permission, main = "OriginalPlanning permission dataset")
plot(diff_ts_planning, main = "Differenced planning permission dataset")
par(opar)

#check the difeferencing neede to make the time series stationary
nsdiffs(planning_permission)
ndiffs(diff_ts_planning)

#performing differncing
diff_ts_planning <- diff(ts_planning_permission, lag = 24, diffferences = 1)
nsdiffs(diff_ts_planning)

#plotting the difference and raw data
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_planning_permission, main = "Original Planning Permission dataset")
plot(diff_ts_planning, main = "Differenced Planning Permission dataset")
par(opar)

#checking the staionarity of the differenced time series
acf(diff_ts_planning)

pacf(diff_ts_planning)

ts_planning_stl <- stl(ts_planning_permission,"periodic")  # decompose the TS
ts_planning_sa <- seasadj(ts_planning_stl)  # de-seasonalize
plot(ts_planning_permission, type="l")  # original series
plot(ts_planning_sa, type="l")  # seasonal adjusted
seasonplot(ts_planning_sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Planning permisison") # seasonal frequency set as 12 for monthly data.

#fitting ARIMA model----------------------------------------

diff_ts_planning <- diff(ts_planning_permission, lag = 18, diffferences = 1)


acf(ts_planning_permission)

acf(diff_ts_planning)

pacf(ts_planning_permission)
pacf(diff_ts_planning)


#splitting the data ---------------------------------------------------
install.packages("TSstudio")
library(TSstudio)
#use the last 3 years of data for testing
#use the TSstudio package
#t_s split function makes it easy to do ths
splitts_planning_permission <- ts_split(ts.obj = ts_planning_permission, sample.out = 36)

training <- splitts_planning_permission$train
testing <- splitts_planning_permission$test

paste("length of air passengers time series:" , length(ts_planning_permission))

paste("length of training data:" , length(splitts_planning_permission$train))

paste("length of testing data:" , length(splitts_planning_permission$test))



#arima elements arima(pdq)(PDQ)12
#for this the PACF for non difference data gives 0 lags befir it bounds to confidnece interval (p)
#Difference is 1
#q = 4
#P = 0
#D = 1
#Q = 4

fit <- arima(training,
             c(0,1,4),
             seasonal = list(order = c(0,1,4), 
             period = 12))

fit
#checking MAPE of manual model
accuracy(fit)

#checkin normality of the residuals
qqnorm(fit$residuals)
qqline(fit$residuals)

#Ljung-box test on the manual data
Box.test(fit$residuals, type = "Ljung-Box")

predict_manual_arima <- forecast(fit ,  36)

#plot the manual predicted data
plot(forecast(fit ,  36))

#creating the auto arima model
auto_arima_model <- auto.arima(training)
auto_arima_model

accuracy(auto_arima_model)
#checkin normality of the residuals on auto
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)


#Ljung-box test on the manual data on auto data
Box.test(auto_arima_model$residuals, type = "Ljung-Box")
 #plotting redicitons of the auto model
predict_auto_arima <-  forecast(auto_arima_model ,  36)
plot(forecast(auto_arima_model ,  36))






# make actuals_predicted dataframe
# for auto ARIMA
actuals_predictions_manual <- data.frame(cbind(actuals = testing, predicted = predict_manual_arima))
head(actuals_predictions_manual)

#check correlation accuracy of manual model
correlation_accuracy_manual <- cor(actuals_predictions_manual)
correlation_accuracy


#check correlation accuracy of auto model
actuals_predictions_auto <- data.frame(cbind(actuals = testing, predicted = predict_auto_arima))
head(actuals_predictions_auto)

correlation_accuracy_auto <- cor(actuals_predictions_auto)



prediction <- predict(fit, n.ahead = 3 * 12)

prediction_auto <- predict(auto_arima_model, n.ahead = 3 * 12)


correlation_accuracy_auto




auto_arima_model <- auto.arima(ts_planning_permission)
auto_arima_model

#plotting the manaul data peformance and the auto model performance
opar <- par(no.readonly=TRUE)

plot(prediction$pred, 
     lty=1, 
     pch="o", 
     col = "Blue", 
     main = "Comparison of manual model and testing data")

# Plot second dataset
lines(testing, 
      col="red", 
      pch="*", 
      lty=2)

legend(1,19, legend=c("Prediction","Testing"), col=c("red","blue"),
       pch=c("o","*"),lty=c(1,2), ncol=1)

plot(prediction_auto$pred, 
     lty=1, 
     pch="o", 
     col = "Blue", 
     main = "Comparison of auto model and testing data")
# Plot second dataset
lines(testing, 
      col="red", 
      pch="*", 
      lty=2)
legend(1,19, legend=c("Prediction","Testing"), col=c("red","blue"),
       pch=c("o","*"),lty=c(1,2), ncol=1)

par(opar)




