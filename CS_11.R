# Case-Study Title: Online retail E-Commerce platform demand prediction (TimeSeries forecasting)
# Data Analysis methodology: CRISP-DM
# Dataset: American Retail company's Sales Timeseries data from 03/01/2014 to 30/12/2017
# Case Goal: Prediction of total monthly revenue in 2017 based-on previous data of the company (sales prediction: cutomers demand prediction) to do annual budget planning


### Required Libraries ----
install.packages('fpp2')
install.packages('forecast')
library('fpp2')
library('forecast')


### Read Data from File ----
sales_data <- read.csv('CS_11.csv', header = T)
dim(sales_data)  # 9994 records, 14 columns


### Step 1: Business Understanding ----
 # know business process and issues
 # know the context of the problem
 # know the order of numbers in the business


### Step 2: Data Understanding ----
### Step 2.1: Data Inspection (Data Understanding from Free Perspective) ----
## Dataset variables definition
colnames(sales_data)

#Row.ID: 	row number
#Order.ID:	order unique number
#Order.Date: 	when customer order the goods (when the income did happen)
#Ship.Date:	when we send customer's order to him
#Ship.Mode:	how we sent customer's order to him
#Segment:	type of customer (Consumer, Home Office, Corporate)
#Product.ID:	product unique number
#Category:	product main category
#Sub.Category:	product sub-category
#Product.Name:	product name & brand & detailed info
#Sales:		total sale amount -> outcome variable
#Quantity:	number of sale
#Discount:	Off percent
#Profit:	how much did company earn? (pure benefit or loss amount)


### Step 2.2: Data Exploring (Data Understanding from Statistical Perspective) ----
## Overview of Dataframe
class(sales_data)
head(sales_data)
tail(sales_data)
str(sales_data)
summary(sales_data)


### Step 3: Data PreProcessing ----
#Date
class(sales_data$Order.Date)
sales_data$Order.Date <- as.Date(sales_data$Order.Date, '%m/%d/%Y')  # change Character to Date

#Extract Month (for monthly analysis)
sales_data$Month <- format(sales_data$Order.Date, '%m')

#Extract Year (for yearly analysis)
sales_data$Year <- format(sales_data$Order.Date, '%Y')

#Product Category
sales_data$Category <- factor(sales_data$Category)

summary(sales_data)

#Time-Series for Monthly Sales (monthly sales per each product category)
ts1 <- tapply(sales_data$Sales, list(sales_data$Month, sales_data$Year, sales_data$Category), sum)  # separate based-on 3 categorical variable
ts1  # 3D array

monthly_sales <- data.frame(year = rep(2014:2017, each = 12), month = rep(1:12, 4))
monthly_sales  # change array to dataframe

monthly_sales$furniture <- as.vector(ts1[,,1])  # Furniture Sales column
monthly_sales$officesupply <- as.vector(ts1[,,2])  # Office Supplies Sales column
monthly_sales$technology <- as.vector(ts1[,,3])  # Technology Sales column

monthly_sales  # monthly sales of each category per year

#Total Monthly Sales
monthly_sales$total <- apply(monthly_sales[,3:5], 1, sum)
head(monthly_sales)
tail(monthly_sales)

#Convert dataframe to timeseries (put a timestamp on each record)
monthly_sales <- ts(monthly_sales, start = 2014, frequency = 12)
monthly_sales  # rownames is timestamp
class(monthly_sales)


### Step 4: Descriptive Analysis ----
## TimeSeries Visualization
#Total Monthly Sales over time
forecast::autoplot(monthly_sales[,'total']) +
	ggtitle('Total Monthly Sales') +
	xlab('Year') +
	ylab('Dollars')  # Total Monthly Sales of company from 2014 to 2017
#we have seasonality and trend pattern

#Total Monthly Sales and its Categories
autoplot(monthly_sales[,3:6]) +
	ggtitle('Monthly Total Sales and its Categories') +
	xlab('Year') +
	ylab('Dollars')

#result: it seems that the seasonality is very chromatic in this dataset 

#Seasonal Plot for Total Sales
forecast::ggseasonplot(monthly_sales[,'total'], year.labels = T, year.labels.left = T) +
	ylab('Dollars') +
	ggtitle('Seasonal Plot - Total Sales')  # line-chart plot: Timeseries of each year

forecast::ggseasonplot(monthly_sales[,'total'], polar = T) +
	ylab('Dollars') +
	ggtitle('Polar Seasonal Plot - Total Sales')  # Polar plot

#result: we see high seasonality patterns in this data by our eyes!

#Seasonal Plot for Furniture Sales
ggseasonplot(monthly_sales[,'furniture'], year.labels = T, year.labels.left = T) +
	ylab('Dollars') +
	ggtitle('Seasonal Plot - Furniture Sales')

ggseasonplot(monthly_sales[,'furniture'], polar = T) +
	ylab('Dollars') + 
	ggtitle('Polar Seasonal Plot - Furniture Sales')  # Polar plot

ggsubseriesplot(monthly_sales[,'furniture']) +
	ylab('Dollars') +
	ggtitle('Seasonal Subseries Plot - Furniture Sales')
#min average is for Feb and max average is for Dec and Nov

#Auto-correlation function
ggAcf(monthly_sales[,'furniture'], lag.max = 36)

2 / sqrt(nrow(monthly_sales))  # significant auto-correlation boundaries of chart

#high significant auto-correlation with Lag_12 and Lag_24 -> this plot tells us we have seasonality in this dataset
#Lag_12 has the most auto-correlation with Yt
#Naive Prediction: if you want to predict current month's Sales, consider the past year's same month sales

#Partial Auto-correlation
ggPacf(monthly_sales[,'furniture'], lag.max = 36)
#Lag_12 has the most significant direct and pure effect on Yt

## TimeSeries Decomposition
#Extract Trend
forecast::ma(monthly_sales[,'furniture'], 3, centre = T)  # calculate Moving Average

#Plot Trend line
#3-MA plot
autoplot(monthly_sales[,'furniture'], series = 'Data') +
	autolayer(ma(monthly_sales[,'furniture'], 3), series = '3-MA') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Trend') +
	scale_color_manual(values = c('Data' = 'grey50', '3-MA' = 'red'))

#5-MA plot
autoplot(monthly_sales[,'furniture'], series = 'Data') +
	autolayer(ma(monthly_sales[,'furniture'], 5), series = '5-MA') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Trend') +
	scale_color_manual(values = c('Data' = 'grey50', '5-MA' = 'red'))

#7-MA plot
autoplot(monthly_sales[,'furniture'], series = 'Data') +
	autolayer(ma(monthly_sales[,'furniture'], 7), series = '7-MA') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Trend') +
	scale_color_manual(values = c('Data' = 'grey50', '7-MA' = 'red'))

#6-MA plot
autoplot(monthly_sales[,'furniture'], series = 'Data') +
	autolayer(ma(monthly_sales[,'furniture'], 6), series = '6-MA') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Trend') +
	scale_color_manual(values = c('Data' = 'grey50', '6-MA' = 'red'))

#12-MA plot
autoplot(monthly_sales[,'furniture'], series = 'Data') +
	autolayer(ma(monthly_sales[,'furniture'], 12), series = '12-MA') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Trend') +
	scale_color_manual(values = c('Data' = 'grey50', '12-MA' = 'red'))  # pure Trend

#result: our data has Trend

# Additive Decomposition approach
ts_decomp_add <- decompose(monthly_sales[,'furniture'], type = 'additive')
ts_decomp_add$x  # input data
ts_decomp_add$seasonal  # seasonal component
ts_decomp_add$trend  # trend component (12-MA)
ts_decomp_add$random  # remain component (random part)
ts_decomp_add

autoplot(ts_decomp_add)  # plot decomposed components
#trend is very weak and seasonality is very strong in this time-series.

# Multiplicative Decomposition approach
ts_decomp_multi <- decompose(monthly_sales[,'furniture'], type = 'multiplicative')
autoplot(ts_decomp_multi)  # plot decomposed components

# STL Decomposition approach
ts_decomp_stl <- stl(monthly_sales[,'furniture'], t.window = 13, s.window = 'periodic')
ts_decomp_stl

autoplot(ts_decomp_stl)  # plot decomposed components
#trend is very weak and seasonality is very strong in this time-series -> the most variance of this Timeseries is explained by Seasonality component


### Step 4: Modeling ----
dim(monthly_sales)  # we have data for 48 months
#train model on Jan 2014 to Dec 2016: from month 1 to 36
#test and evaluate model performance on Jan 2017 to Dec 2017: from month 37 to 48

# Model 1: Average Method
hist(monthly_sales[1:36, 'furniture'])  # histogram of train data -> data is not from Normal distribution

#with assumption that data is normally distributed
meanf(monthly_sales[1:36, 'furniture'], 1)  # predict 1 next data
#point estimate for next time-range (1 Jan 2017) is 14628.13 
#confidence interval 80%: [3061.2 - 26195.05] 
#confidence interval 95%: [-3349.133 - 32605.38]

#without assumption that data is normally distributed
meanf(monthly_sales[1:36, 'furniture'], 1, bootstrap = T, npaths = 500, level = c(80, 95))
#confidence interval 80%: 80% of generated data falled between 6242.525 and 30880.83
#confidence interval 95%: 95% of generated data falled between 1839.658 and 36678.71

meanf(monthly_sales[1:36, 'furniture'], 12)  # all of next 12 months will be same to next first month -> mean of data

#Expanding Window
ave_method <- numeric(length = 12)  # vector to save prediction outputs
for(i in 1:12){
	ave_method[i] <- meanf(monthly_sales[1:(35+i), 'furniture'], 1)$mean  # extract point estimate column
}
ave_method
ave_method <- ts(ave_method, start = 2017, frequency = 12)  # change vector to timeseries
ave_method

#Errors
err_ave_method <- (monthly_sales[37:48, 'furniture'] - ave_method)
methods_comp <- data.frame('Method' = 'AveMethod',
				'mean' = mean(abs(err_ave_method)),
				'median' = median(abs(err_ave_method)),
				'sd' = sd(abs(err_ave_method)),
				'IQR' = IQR(abs(err_ave_method)),
				'Min' = min(abs(err_ave_method)),
				'Max' = max(abs(err_ave_method)))
View(methods_comp)

#Plot Actual vs. Prediction
autoplot(monthly_sales[, 'furniture'], series = 'Actual') +
	autolayer(ave_method, series = 'Prediction') +
	xlab('Year') + ylab('Dollars') +
	ggtitle('Monthly Furniture Total Sales and Average Method Prediction') +
	scale_colour_manual(values = c('Data' = 'grey50', 'Prediction' = 'red'))

#idea: we can transform data to make it Normally-Distributed

# Model 2: Average Method Adjusted Bias
lambda <- forecast::BoxCox.lambda(monthly_sales[,'furniture'])  # gives us the optimum lambda
meanf(monthly_sales[1:36, 'furniture'], 1, lambda = lambda)

#Expanding Window
ave_method_adjbias <- numeric(length = 12)
for(i in 1:12){
	ave_method_adjbias[i] <- meanf(monthly_sales[1: (35 + i), 'furniture'], 1, lambda = lambda)$mean
}
ave_method_adjbias
ave_method_adjbias <- ts(ave_method_adjbias, start = 2017, frequency = 12)
ave_method_adjbias

#Errors
err_ave_method_adjbias <- (monthly_sales[37:48, 'furniture'] - ave_method_adjbias)
methods_comp <- rbind(methods_comp, data.frame("Method" = "AveMethodAdjBias",
                                               "mean" = mean(abs(err_ave_method_adjbias)),
                                               "median" = median(abs(err_ave_method_adjbias)),
                                               "sd" = sd(abs(err_ave_method_adjbias)),
                                               "IQR" = IQR(abs(err_ave_method_adjbias)),
                                               "Min" = min(abs(err_ave_method_adjbias)),
                                               "Max" = max(abs(err_ave_method_adjbias))))
View(methods_comp)

#Plot Actual vs. Prediction
autoplot(monthly_sales[, "furniture"], series = "Data") +
 	autolayer(ave_method_adjbias, series = "Prediction") +
 	xlab("Year") + ylab("Dollars") +
	ggtitle("Monthly Furniture Total Sales and Ave Method Adjusted Bias Prediction") + 
	scale_colour_manual(values = c("Data" = "grey50", "Prediction" = "red"))

# Model 3: Naive Method
naive(monthly_sales[1:36, 'furniture'], 1)
#point-estimate for Jan 2017 is equals to Dec 2016 (it's previous datapoint)

#Expanding Window
naive_method <- numeric(length = 12)
for(i in 1:12){
	naive_method[i] <- naive(monthly_sales[1: (35 + i), 'furniture'], 1)$mean
}
naive_method
naive_method <- ts(naive_method, start = 2017, frequency = 12)
naive_method

#Errors
err_naive_method <- (monthly_sales[37:48, 'furniture'] - naive_method)
methods_comp <- rbind(methods_comp, data.frame("Method" = "NaiveMethod",
                                               "mean" = mean(abs(err_naive_method)),
                                               "median" = median(abs(err_naive_method)),
                                               "sd" = sd(abs(err_naive_method)),
                                               "IQR" = IQR(abs(err_naive_method)),
                                               "Min" = min(abs(err_naive_method)),
                                               "Max" = max(abs(err_naive_method))))
View(methods_comp)

#Plot Actual vs. Prediction
autoplot(monthly_sales[, "furniture"], series = "Data") +
	autolayer(naive_method, series = "Prediction") +
	xlab("Year") + ylab("Dollars") +
	ggtitle("Monthly Furniture Total Sales and Naive Method Prediction") + 
	scale_colour_manual(values = c("Data" = "grey50", "Prediction" = "red"))

# Model 4: Simple Linear Regression
#split train data
monthly_sales_train <- window(monthly_sales, start = 2014, end = c(2016, 12))  # cut data
monthly_sales_train

reg_model <- tslm(monthly_sales_train[, 3] ~ trend + season)  # timeseries simple linear regression model based-on Trend and Seasonality components
reg_model
summary(reg_model)  # Adjusted R-squared = 0.8868

#forecasting next 12-months Sales by this model
reg_model_pred <- forecast(reg_model, h = 12)
reg_model_pred  # point-estimate with confidence intervals
reg_model_pred$mean  # point-estimate column

#Errors
err_reg_method <- (monthly_sales[37:48, "furniture"] - reg_model_pred$mean)
methods_comp <- rbind(methods_comp, data.frame("Method" = "RegMethod",
                                               "mean" = mean(abs(err_reg_method)),
                                               "median" = median(abs(err_reg_method)),
                                               "sd" = sd(abs(err_reg_method)),
                                               "IQR" = IQR(abs(err_reg_method)),
                                               "Min" = min(abs(err_reg_method)),
                                               "Max" = max(abs(err_reg_method))))
View(methods_comp)

#Plot Actual vs. Prediction
autoplot(monthly_sales[, "furniture"], series = "Data") +
	autolayer(reg_model_pred$mean, series = "Prediction") +
	xlab("Year") + ylab("Dollars") +
	ggtitle("Monthly Furniture Total Sales and Regression Method Prediction") + 
	scale_colour_manual(values = c("Data" = "grey50", "Prediction" = "red"))

# Model 5: Exponential Smoothing approaches (Holt-Winters Method)
#HW - Additive Model
hw_model_1 <- ets(monthly_sales_train[, 3], model = 'AAA')
summary(hw_model_1)

checkresiduals(hw_model_1)  # plot training residuals -> to check how much it is similar to White-Noise (how much model is good?)
#Ljung-Box test
#H0: The data are independently distributed (the residuals are independent from each other) -> the Residuals have not any AutoCorrelation with each other 
#if p-value < 0.05 reject H0

#HW - Multiplicative Model
hw_model_2 <- ets(monthly_sales_train[,3], model = 'MAM')
summary(hw_model_2)
checkresiduals(hw_model_2)

#Optimal Model (software proposal)
hw_model_3 <- ets(monthly_sales_train[,3], model = 'ZZZ')
summary(hw_model_3)
checkresiduals(hw_model_3)

#use Additive Model for prediction:
hw_model_pred <- forecast(hw_model_1, h = 12)  # predict for next 12-months
hw_model_pred
hw_model_pred$mean  # point-estimate column

#Errors
err_hw_method <- (monthly_sales[37:48, "furniture"] - hw_model_pred$mean)
methods_comp <- rbind(methods_comp, data.frame("Method" = "HWMethod",
                                               "mean" = mean(abs(err_hw_method)),
                                               "median" = median(abs(err_hw_method)),
                                               "sd" = sd(abs(err_hw_method)),
                                               "IQR" = IQR(abs(err_hw_method)),
                                               "Min" = min(abs(err_hw_method)),
                                               "Max" = max(abs(err_hw_method))))
View(methods_comp)

#Plot Actual vs. Prediction
autoplot(monthly_sales[, "furniture"], series = "Data") +
	autolayer(hw_model_pred$mean, series = "Prediction") +
	xlab("Year") + ylab("Dollars") +
	ggtitle("Monthly Furniture Total Sales and Holt-Winters Method Prediction") + 
	scale_colour_manual(values = c("Data" = "grey50", "Prediction" = "red"))

# Model 6: ARIMA Method
ggAcf(monthly_sales[, 'furniture'], lag.max = 36)  # Auto-correlation
ggPacf(monthly_sales[, 'furniture'], lag.max = 36)  # Partial-Auto-correlation
#Yt has high correlation with Lag-12 -> we have significant Seasonality in this Timeseries data

#ARIMA(p,d,q) models without seasonality
#ARIMA(0, 1, 2)
arima_model_1 <- arima(monthly_sales_train[,3], order = c(0, 1, 2))
arima_model_1

#ARIMA(1, 1, 0)
arima_model_2 <- arima(monthly_sales_train[, 3], order = c(1, 1, 0))
arima_model_2

#ARIMA(1, 1, 2)
arima_model_3 <- arima(monthly_sales_train[, 3], order = c(1, 1, 2))
arima_model_3

#ARIMA(p,d,q)(P,D,Q)[m] models with seasonality
arima_model_4 <- auto.arima(monthly_sales_train[,3], seasonal = T)  # Optimal Model (software proposal)
arima_model_4

#choose model with min(AIC)
sarima_pred <- forecast(arima_model_4, h = 12)
sarima_pred$mean  # predictions

#Errors
err_sarima <- (monthly_sales[37:48, "furniture"] - sarima_pred$mean)
methods_comp <- rbind(methods_comp, data.frame("Method" = "SARIMA",
                                                 "mean" = mean(abs(err_sarima)),
                                                 "median" = median(abs(err_sarima)),
                                                 "sd" = sd(abs(err_sarima)),
                                                 "IQR" = IQR(abs(err_sarima)),
                                                 "Min" = min(abs(err_sarima)),
                                                 "Max" = max(abs(err_sarima))))
View(methods_comp) 
#best method in model comparison is Regression

#Plot Actual vs. Prediction
autoplot(monthly_sales[, "furniture"], series = "Data") +
	autolayer(sarima_pred$mean, series = "Prediction") +
	xlab("Year") + ylab("Dollars") +
	ggtitle("Monthly Furniture Total Sales and SARIMA Method Prediction") + 
	scale_colour_manual(values = c("Data" = "grey50", "Prediction" = "red"))





