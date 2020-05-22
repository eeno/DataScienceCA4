

#import plannning permission data
planning_permission <- read.csv("Planning_permission_clean.csv")
# check structure
str(planning_permission$DecisionDate)

#import monthly cpi data
cpi <- read.csv("CPI2.csv")

#convert decsion date to date fromat
planning_permission$DecisionDate <- as.Date(planning_permission$DecisionDate, format = "%Y-%m-%d")

#create a dataframe of du sate and the grabted indicator 
ts_planning_permission <- planning_permission[, c(9,17)]

#create a new planning permission dataset aggragating number of granted application based on month of decision date
#first need to convert decision date column to a column of just month and year
ts_planning_permission$DecisionDate <- format(ts_planning_permission$DecisionDate, "%Y-%m")

names(ts_planning_permission)
#aggregate the granted data based on month and year
ts_planning_permission <- aggregate(granted ~ DecisionDate,ts_planning_permission,  FUN = sum)

ts_planning_permission <- ts(ts_planning_permission$granted,start = c(2000,1), frequency = 12)
ts_planning_permission

plot(ts_planning_permission)

start(ts_planning_permission)
end(ts_planning_permission)
frequency(ts_planning_permission)

class(ts_planning_permission)

summary(ts_planning_permission)


install.packages("forecast")
library(forecast)

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


#Test if a time series is stationary
library(tseries)
# p-value < 0.05 indicates the TS is stationary

adf.test(ts_planning_permission)


cycle(ts_planning_permission)

#How to determine if a time series is stationary

plot(ts_planning_permission,
     xlab="Date", 
     ylab = "Number of planning Permissions granted",
     main="Planning permissions granted from Jan 2000 - Dec 2019 ")
# Add a straight line shwing the linear relationship
# between passenger numbers and time
abline(reg=lm(ts_planning_permission~time(ts_planning_permission)))


plot(aggregate(ts_planning_permission,FUN=mean))


#Multiplicative or additive model?
boxplot(ts_planning_permission ~ cycle(ts_planning_permission),
        xlab="Date", 
        ylab = "Number of planning Permissions granted" ,
        main ="Planning permissions granted from Jan 2000 - Dec 2019 ")

seasonal_decomposition <- stl(ts_planning_permission, s.window="period")
plot(seasonal_decomposition)



#testing stationarity
#inn order to test the stationarity of the time series, let’s run the Augmented Dickey-Fuller Test using the adf.test() function 
#from the tseries package.
# The null hypothesis H0 : the time series is non stationary
# The alternative hypothesis HA : the time series is stationary


library(tseries)
suggested_k <- trunc((length(ts_planning_permission)-1)^(1/3))
suggested_k


adf.test(ts_planning_permission, alternative = "stationary")


adf.test(ts_planning_permission, alternative = "stationary", k = 12)



#Method 2 : Test stationarity of the time series (Autocorrelation)
library(forecast)
acf(ts_planning_permission)


pacf(ts_planning_permission)

library(forecast)
nsdiffs(ts_planning_permission)





log_planning_permission <- log(ts_planning_permission)


# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_planning_permission, main = "OriginalPlanning permission dataset")
plot(ts_planning_permission, main = "Differenced planning permission dataset")
par(opar)

nsdiffs(log_planning_permission)



diff_ts_planning <- diff(ts_planning_permission, lag = 6, diffferences = 2)

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_planning_permission, main = "Original Air Passengers dataset")
plot(diff_ts_planning, main = "Differenced Air Passengers dataset")
par(opar)


seasonal_decomposition <- stl(ts_planning_permission, s.window="period")
plot(seasonal_decomposition)

acf(diff_ts_planning)


pacf(diff_ts_planning)

ts_planning_stl <- stl(ts_planning_permission,"periodic")  # decompose the TS
ts_planning_sa <- seasadj(ts_planning_stl)  # de-seasonalize
plot(ts_planning_permission, type="l")  # original series
plot(ts_planning_sa, type="l")  # seasonal adjusted
seasonplot(ts_planning_sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Airpassengers") # seasonal frequency set as 12 for monthly data.

#arima elements arima(pdq)(PDQ)12
#for this the PACF for non difference data gives 3 lags befir it bounds to confidnece interval (p)
#Difference is 1
#q = 1
#P = 2
#D = 1
#Q = 2
fit <- arima(ts_planning_permission,
             c(3,1,1),
             seasonal = list(order = c(2,1,2), 
             period = 12))

fit

# #Replace - with M to match the "month and year" column in POI2 and CPI2
# test$year_month <- gsub("-","M",test$year_month)
# test$year_month
# 
# test <- merge(test,cpi, by = "year_month", all =  TRUE) 
# 
# scatter.smooth(x = test$CPI..2011., y = test$granted,
#               main = "Distance ~ speed",
#               xlab = "Consumer Price index",
#               ylab = "Number of granted applications")

auto_arima_model <- auto.arima(ts_planning_permission)
auto_arima_model
