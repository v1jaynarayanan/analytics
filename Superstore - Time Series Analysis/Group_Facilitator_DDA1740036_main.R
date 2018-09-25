#############################################################################################
############################### Retail Giant Sales Forecasting ##############################
###############################         Group Case Study       ##############################
#############################################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tseries)
library(forecast)

#############################################################################################
####    Authors : Vijayanand Narayanan, Arunachalam Meenakshisundaram,                   ####
####              Akash Ashokan and Dharamarajan Thiyagarajan                            ####
#############################################################################################

#############################################################################################
###################################  Business Brief  ########################################
#############################################################################################
# 1. Global Mart is an online retail giant that takes orders and deliveries worldwide  
# 2. The company caters to 7 market segments and 3 categories (Consumer, Home and Corporate)
# 3. Forecast sales and demand for the 2 most profitable segments for the next 6 months
#############################################################################################

#############################################################################################
#####################################  Objectives  ##########################################
#############################################################################################
# 1. Understand all markets and segments in the provided data
# 2. Aggregate Sales, Quantity and Profit for all segments
# 3. Identify top 2 consistently profitable segments using Coefficient of Variation of profit
# 4. Forecast Sales and Demand for next 6 months using Classical Decomposition and Auto ARIMA
#############################################################################################

# Set working directory
setwd("~/Downloads/Upgrad-IIIT/Course/Predictive Analytics II/Time Series/Group Assignment")

# Read dataset
superstore <- read.csv("Global superstore.csv", stringsAsFactors = FALSE, header = TRUE)

# View dataset
View(superstore)

#############################################################################################
####################################  Data Cleaning  ########################################
#############################################################################################

# Are there any duplicates?
sum(duplicated(superstore))
# There are no duplicate orders

# Are there any missing values
colSums(is.na(superstore))
# There are 41296 missing values
# All missing values are in Postal.Code column

unique(superstore$Country[which(is.na(superstore$Postal.Code == T))])
# It appears that Postal.Code is empty for all non US countries 
# But, this is not a concern as this column is not a siginificant attribute for the time series 
# model to forecast sales and demand

# Structure of dataset
str(superstore)

# Change Order.Date and Ship.Date from chr to date format
superstore$Order.Date <- as.Date(superstore$Order.Date, "%d-%m-%Y")
superstore$Ship.Date <- as.Date(superstore$Ship.Date, "%d-%m-%Y")

#############################################################################################
##################################  Data Preparation  #######################################
#############################################################################################
# Determine levels for Market
levels(as.factor(superstore$Market))
# Market has 7 unique values

# Determine levels for Segment
levels(as.factor(superstore$Segment))
# Segment has 3 unique values

# Create a new attribute to store year and month
superstore$YearMonth <- format(as.Date(superstore$Order.Date), "%Y-%m")

# Aggregate Sales, Quantity & Profit over the Order Date to arrive at monthly values for these attributes
superstore_monthly_agg <- aggregate(superstore[, c("Sales","Quantity","Profit")], by = list(superstore$Market, superstore$Segment, superstore$YearMonth), FUN = "sum")
# Rename columns 
colnames(superstore_monthly_agg) = c("Market","Segment","OrderDate","Sales", "Quantity", "Profit")

# Aggregate Profit for each market segment over all dates to to identify top 2 most profitable segments
superstore_profit_agg <- aggregate(superstore[, "Profit"], by = list(superstore$Market, superstore$Segment), FUN = "sum")
# Rename columns 
colnames(superstore_profit_agg) = c("Market","Segment","Profit")

# Aggregate CV of Profit for each market segment over all dates to to identify top 2 most consistently profitable segments
summary_segments <- superstore[, c("Profit","Sales","Market","Segment","Quantity")] %>% group_by(Market,Segment) %>% dplyr::summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(summary_segments) = c("Market","Segment","Sales","Profit", "CV")

# Top 2 Profitable Segments
head(superstore_profit_agg[order(superstore_profit_agg$Profit, decreasing = TRUE),], 2)
# APAC Consumer and EU Consumer are the two most profitable segments

# Consistently Profitable Segments based on CV of Profit
head(summary_segments[order(summary_segments$CV, decreasing = FALSE),], 7)
# APAC Consumer and EU Consumer have low CV values

# Plots
plot1 <- ggplot(summary_segments, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity") + labs(title="Most Profitable Segment")
plot2 <- ggplot(summary_segments, aes(Segment, CV, fill=Market)) + geom_bar(position = "dodge",stat = "identity") + labs(title="Most Consistently Profitable Segment")

grid.arrange(plot1, plot2, ncol=1, nrow = 2)

# Plot CV, Profit for all Market Segments
ggplot(summary_segments, aes(CV, Profit, colour = paste(Market, Segment))) + geom_point() + geom_text(aes(label=paste(Market, Segment)),hjust=0, vjust=-1) + labs(title = "Profit and CV for All Market Segments", x= "CV", y="Profit", colour="Market Segment")

# From the plots it can be seen that the top 2 segments based on maximum profit and least
# Coefficient of Variation of Profit are :
# 1. APAC Consumer and 
# 2. EU Consumer

#############################################################################################
####################################  Model Building  #######################################
#############################################################################################
# Common function to filter out segment data
filterOutSegment <- function(data, market, segment) {
  filtered_segment <- filter(data, Market == market & Segment == segment)
  return(filtered_segment)
}

# Filter out top 2 segments APAC Consumer and EU Consumer from the dataset
apac_segment <- filterOutSegment(superstore_monthly_agg,"APAC", "Consumer")
eu_segment <-  filterOutSegment(superstore_monthly_agg, "EU", "Consumer")

#############################################################################################
#############################  Apac Consumer Sales Forecast  ################################
#############################################################################################

# Create Time Series for sales on all data for this segment
apac_sales_total_ts <- ts(apac_segment$Sales)
plot(apac_sales_total_ts, main = "Sales Time Series for APAC Consumer", xlab = "Time", ylab="Total Sales")
lines(lowess(apac_sales_total_ts), col="red")
legend(x="topleft", y=10, legend=c("Sales", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# There is a clear increasing global trend with some seasonality

# Seperate out last 6 months data from sample so that we can build a model and use it to
# predict these values and then cross-check them later
apac_sales_indata <- apac_segment[1:(nrow(apac_segment)-6), ]

# Create Time Series data for Sales
apac_sales_ts <- ts(apac_sales_indata$Sales)

# Plot Sales Time Series for 1 to 42 months
plot(apac_sales_ts, main = "Sales Time Series for APAC Consumer", xlab = "Time", ylab="Total Sales")

# Smoothen series using moving average method with a window size of 3
w <-1
apac_sales_smoothedseries <- stats::filter(apac_sales_ts,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series
diff <- apac_sales_smoothedseries[w+2] - apac_sales_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  apac_sales_smoothedseries[i] <- apac_sales_smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(apac_sales_ts)
diff <- apac_sales_smoothedseries[n-w] - apac_sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  apac_sales_smoothedseries[i] <- apac_sales_smoothedseries[i-1] + diff
}

# Plot the smoothed sales time series
plot(apac_sales_ts, main = "Sales Time Series With Smoothing for APAC Consumer", xlab = "Time", ylab="Total Sales")
lines(apac_sales_smoothedseries, col="red", lwd=2)
legend(x="topleft", y=10, legend=c("Sales", "Smoothed Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# The Time Series for Sales appear to be smoothed well

# Convert the sales time series to a dataframe
apac_sales_timevals_in <- c(1:nrow(apac_sales_indata))
apac_sales_smootheddf <- as.data.frame(cbind(apac_sales_timevals_in, as.vector(apac_sales_smoothedseries)))
colnames(apac_sales_smootheddf) <- c('Month', 'Sales')

# Using Linear Regression to model Sales
apac_sales_lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,1) + cos(0.05*Month) * poly(Month,1) +
              tan(0.02*Month), Month, data=apac_sales_smootheddf)

# Summary of model and accuracy
summary(apac_sales_lmfit)
accuracy(apac_sales_lmfit)

# Predict Sales using the LR model
apac_sales_global_pred <- predict(apac_sales_lmfit, Month=apac_sales_timevals_in)
summary(apac_sales_global_pred)
lines(apac_sales_timevals_in, apac_sales_global_pred, col='blue', lwd=2)
legend(x="topleft", y=10, legend=c("Sales", "Smoothed Line", "Predicted"), col=c("black", "red", "blue"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# The predicted values from linear regression are quite close to the actual values 
# and captures trend and seasonal behaviour well

# Now, let us look at the locally predictable series
# Subtract the predicted global trend and seasonality from the original time series
apac_sales_local_pred <- apac_sales_ts - apac_sales_global_pred
# What remains is a stationary time series. This should now contain local predictable auto regressive
# part (if any) and white noise

plot(apac_sales_local_pred, col='black', type = "l", main = "Stationary Sales Time Series for APAC Consumer", xlab = "Time", ylab="Total Sales")
# The graph is flat and has no long-term predictability such as trend or seasonality.

# Let us check for weak stationarity i.e is there any local predictable behaviour still left in the time series?
acf(apac_sales_local_pred, main="ACF Plot for APAC Consumer Sales Time Series")
acf(apac_sales_local_pred, type="partial", main="PACF Plot for APAC Consumer Sales Time Series")

apac_sales_autoarima_ts <- auto.arima(apac_sales_local_pred)
tsdiag(apac_sales_autoarima_ts)
apac_sales_autoarima_ts
# ARIMA(0,0,0) with zero mean. This implies that there is no AR(p) or MA(q) series in the local component

# Let us test the residual for white noise
resi_apac_sales <- apac_sales_local_pred - fitted(apac_sales_autoarima_ts)

# White noise test
qqnorm(resi_apac_sales)
# Plot of white noise shows a linear graph

# Dickey-Fuller Test
adf.test(resi_apac_sales,alternative = "stationary")
# Augmented Dickey-Fuller Test shows a p-value of 0.01 which is < 0.05
# Residual time series is stationary and just white noise

# KPSS Test
kpss.test(resi_apac_sales)
# KPSS test shows a p-value of 0.1 with is > 0.05
# Residual time series is stationary and just white noise

# Model Evaluation of Classical Decomposition for Sales
# Forecast Sales using for the last 6 months i.e months 43 to 48
apac_sales_outdata <- apac_segment[43:48, ]
apac_sales_timevals_out <- c(43:48)

apac_sales_global_pred_out <- predict(apac_sales_lmfit,data.frame(Month=apac_sales_timevals_out))

#Now, let's compare our prediction with the actual values, using MAPE
apac_sales_MAPE_class_dec <- accuracy(apac_sales_global_pred_out,apac_sales_outdata$Sales)[5]
apac_sales_MAPE_class_dec
# MAPE is 20.83

# Let us plot the predictions along with original values, to get a visual feel of the fit
apac_sales_class_dec_pred <- c(ts(apac_sales_global_pred),ts(apac_sales_global_pred_out))
plot(apac_sales_total_ts, col = "black", main = "Sales Forecast for APAC Segment using Classical Decomposition", xlab ="Time", ylab ="Total Sales")
lines(apac_sales_class_dec_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Sales", "Predicted Sales"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

# Using Auto Arima model
apac_sales_autoarima <- auto.arima(apac_sales_ts)
apac_sales_autoarima
# The auto arima model was ARIMA(0,1,1)  
# This means that 1 stage differencing was performed and the resulting timeseries was 
# modeled as MA(1).
tsdiag(apac_sales_autoarima)
plot(apac_sales_autoarima$x, col="black", main = "Sales Time Series for APAC Consumer using ARIMA", xlab = "Time", ylab="Total Sales")
lines(fitted(apac_sales_autoarima), col="red")

# Let us check if the residual series Auto Arima model is white noise
resi_apac_sales_auto_arima <- apac_sales_ts - fitted(apac_sales_autoarima)

adf.test(resi_apac_sales_auto_arima,alternative = "stationary")
kpss.test(resi_apac_sales_auto_arima)
# Residual time series after Auto Arima is indeed stationary which means it is pure white noise

# Auto Arima Model Evaluation for Sales
# Forecast Sales using for the last 6 months i.e months 43 to 48
apac_sales_auto_arima_forecast <- predict(apac_sales_autoarima, n.ahead = 6)

#Now, let's compare our prediction with the actual values, using MAPE
apac_sales_MAPE_autoarima <- accuracy(apac_sales_auto_arima_forecast$pred,apac_sales_outdata$Sales)[5]
apac_sales_MAPE_autoarima
# MAPE is 27.68, which is higher than the Classical Decomposition model

# Let us plot the predictions of Auto Arima along with original values, to get a visual feel of the fit
apac_sales_auto_arima_pred <- c(fitted(apac_sales_autoarima),ts(apac_sales_auto_arima_forecast$pred))
plot(apac_sales_total_ts, col = "black", main = "Sales Forecast for APAC Segment using Auto Arima", xlab ="Time", ylab ="Total Sales")
lines(apac_sales_auto_arima_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Sales", "Predicted Sales"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

#############################################################################################
############################  Apac Consumer Demand Forecast  ################################
#############################################################################################

# Create Time Series for Demand on all data for this segment
apac_demand_total_ts <- ts(apac_segment$Quantity)
plot(apac_demand_total_ts, main = "Demand Time Series for APAC Consumer", xlab = "Time", ylab="Total Demand")
lines(lowess(apac_demand_total_ts), col="red")
legend(x="topleft", y=10, legend=c("Demand", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# There is a clear increasing global trend with some seasonality

# Seperate out last 6 months data from sample so that we can build a model and use it to
# predict these values and then cross-check them later
apac_demand_indata <- apac_segment[1:(nrow(apac_segment)-6), ]

# Create Time Series data for Demand
apac_demand_ts <- ts(apac_demand_indata$Quantity)

# Plot Demand Time Series
plot(apac_demand_ts, main = "Demand Time Series for APAC Consumer", xlab = "Time", ylab="Total Demand")

# Smoothen series using moving average method with a window size of 3
w <-1
apac_demand_smoothedseries <- stats::filter(apac_demand_ts,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series
diff <- apac_demand_smoothedseries[w+2] - apac_demand_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  apac_demand_smoothedseries[i] <- apac_demand_smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(apac_demand_ts)
diff <- apac_demand_smoothedseries[n-w] - apac_demand_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  apac_demand_smoothedseries[i] <- apac_demand_smoothedseries[i-1] + diff
}

# Plot the smoothed Demand time series
plot(apac_demand_ts, main = "Demand Time Series With Smoothing for APAC Consumer", xlab = "Time", ylab="Total Demand")
lines(apac_demand_smoothedseries, col="red", lwd=2)
legend(x="topleft", y=10, legend=c("Demand", "Smoothed Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# The Time Series for Demand appear to be smoothed well

# Convert the demand time series to a dataframe
apac_demand_timevals_in <- c(1:nrow(apac_demand_indata))
apac_demand_smootheddf <- as.data.frame(cbind(apac_demand_timevals_in, as.vector(apac_demand_smoothedseries)))
colnames(apac_demand_smootheddf) <- c('Month', 'Demand')

# Using Linear Regression to model Demand
apac_demand_lmfit <- lm(Demand ~ sin(0.5*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1) +
                          tan(0.02*Month), Month, data=apac_demand_smootheddf)

# Summary of model and accuracy
summary(apac_demand_lmfit)
accuracy(apac_demand_lmfit)

# Predict Demand using the LR model
apac_demand_global_pred <- predict(apac_demand_lmfit, Month=apac_demand_timevals_in)
summary(apac_demand_global_pred)
lines(apac_demand_timevals_in, apac_demand_global_pred, col='blue', lwd=2)
legend(x="topleft", y=10, legend=c("Demand", "Smoothed Line", "Predicted"), col=c("black", "red", "blue"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# The predicted values from linear regression are quite close to the actual values 
# and captures trend and seasonal behaviour reasonably well

# Now, let us look at the locally predictable series
# Subtract the predicted global trend and seasonality from the original time series
apac_demand_local_pred <- apac_demand_ts - apac_demand_global_pred
# What remains is a stationary time series. This should now contain local predictable auto regressive
# part (if any) and white noise

plot(apac_demand_local_pred, col='black', type = "l", main = "Stationary Demand Time Series for APAC Consumer", xlab = "Time", ylab="Total Demand")
# The graph is flat and has no long-term predictability such as trend or seasonality.

# Let us check for weak stationarity i.e is there any local predictable behaviour still left in the time series?
acf(apac_demand_local_pred, main="ACF Plot for APAC Consumer Demand Time Series")
acf(apac_demand_local_pred, type="partial", main="PACF Plot for APAC Consumer Demand Time Series")

apac_demand_autoarima_ts <- auto.arima(apac_demand_local_pred)
tsdiag(apac_demand_autoarima_ts)
apac_demand_autoarima_ts
# ARIMA(0,0,0) with zero mean. This implies that there is no AR(p) or MA(q) series in the local component

# Let us test the residual for white noise
resi_apac_demand <- apac_demand_local_pred - fitted(apac_demand_autoarima_ts)

# White noise test
qqnorm(resi_apac_demand)
# Plot of white noise shows a linear graph

# Dickey-Fuller Test
adf.test(resi_apac_demand,alternative = "stationary")
# Augmented Dickey-Fuller Test shows a p-value of 0.01 which is < 0.05
# Residual time series is stationary and just white noise

# KPSS Test
kpss.test(resi_apac_demand)
# KPSS test shows a p-value of 0.1 with is > 0.05
# Residual time series is stationary and just white noise

# Model Evaluation of Classical Decomposition for Demand
# Forecast Demand using for the last 6 months i.e months 43 to 48
apac_demand_outdata <- apac_segment[43:48, ]
apac_demand_timevals_out <- c(43:48)

apac_demand_global_pred_out <- predict(apac_demand_lmfit,data.frame(Month=apac_demand_timevals_out))

#Now, let's compare our prediction with the actual values, using MAPE
apac_demand_MAPE_class_dec <- accuracy(apac_demand_global_pred_out,apac_demand_outdata$Quantity)[5]
apac_demand_MAPE_class_dec
# MAPE is 18.79

# Let us plot the predictions along with original values, to get a visual feel of the fit
apac_demand_class_dec_pred <- c(ts(apac_demand_global_pred),ts(apac_demand_global_pred_out))
plot(apac_demand_total_ts, col = "black", main = "Demand Forecast for APAC Segment using Classical Decomposition", xlab ="Time", ylab ="Total Demand")
lines(apac_demand_class_dec_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Demand", "Predicted Demand"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

# Using Auto Arima model
apac_demand_autoarima <- auto.arima(apac_demand_ts)
apac_demand_autoarima
# The auto arima model was ARIMA(0,1,0)  
# This means that 1 stage differencing was performed

tsdiag(apac_demand_autoarima)
plot(apac_demand_autoarima$x, col="black", main = "Demand Time Series for APAC Consumer using ARIMA", xlab = "Time", ylab="Total Demand")
lines(fitted(apac_demand_autoarima), col="red")

# Let us check if the residual series Auto Arima model is white noise
resi_apac_demand_auto_arima <- apac_demand_ts - fitted(apac_demand_autoarima)

adf.test(resi_apac_demand_auto_arima,alternative = "stationary")
kpss.test(resi_apac_demand_auto_arima)
# Residual time series after Auto Arima is indeed stationary which means it is pure white noise

# Auto Arima Model Evaluation for Demand
# Forecast Demand using for the last 6 months i.e months 43 to 48
apac_demand_auto_arima_forecast <- predict(apac_demand_autoarima, n.ahead = 6)

#Now, let's compare our prediction with the actual values, using MAPE
apac_demand_MAPE_autoarima <- accuracy(apac_demand_auto_arima_forecast$pred,apac_demand_outdata$Quantity)[5]
apac_demand_MAPE_autoarima
# MAPE is 26.24, which is higher than the Classical Decomposition model

# Let us plot the predictions of Auto Arima along with original values, to get a visual feel of the fit
apac_demand_auto_arima_pred <- c(fitted(apac_demand_autoarima),ts(apac_demand_auto_arima_forecast$pred))
plot(apac_demand_total_ts, col = "black", main = "Demand Forecast for APAC Segment using Auto Arima", xlab ="Time", ylab ="Total Demand")
lines(apac_demand_auto_arima_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Demand", "Predicted Demand"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

#############################################################################################
#############################   EU Consumer Sales Forecast  #################################
#############################################################################################

# Create Time Series for sales on all data for this segment
eu_sales_total_ts <- ts(eu_segment$Sales)
plot(eu_sales_total_ts, main = "Sales Time Series for EU Consumer", xlab = "Time", ylab="Total Sales")
lines(lowess(eu_sales_total_ts), col="red")
legend(x="topleft", y=10, legend=c("Sales", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# There is a clear increasing global trend with some seasonality

# Seperate out last 6 months data from sample so that we can build a model and use it to
# predict these values and then cross-check them later
eu_sales_indata <- eu_segment[1:(nrow(eu_segment)-6), ]

# Create Time Series data for Sales
eu_sales_ts <- ts(eu_sales_indata$Sales)

# Plot Sales Time Series for 1 to 42 months
plot(eu_sales_ts, main = "Sales Time Series for EU Consumer", xlab = "Time", ylab="Total Sales")

# Smoothen series using moving average method with a window size of 3
w <-1
eu_sales_smoothedseries <- stats::filter(eu_sales_ts,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series
diff <- eu_sales_smoothedseries[w+2] - eu_sales_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  eu_sales_smoothedseries[i] <- eu_sales_smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(eu_sales_ts)
diff <- eu_sales_smoothedseries[n-w] - eu_sales_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  eu_sales_smoothedseries[i] <- eu_sales_smoothedseries[i-1] + diff
}

# Plot the smoothed sales time series
plot(eu_sales_ts, main = "Sales Time Series With Smoothing for EU Consumer", xlab = "Time", ylab="Total Sales")
lines(eu_sales_smoothedseries, col="red", lwd=2)
legend(x="topleft", y=10, legend=c("Sales", "Smoothed Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.55)
# The Time Series for Sales appear to be smoothed well

# Convert the sales time series to a dataframe
eu_sales_timevals_in <- c(1:nrow(eu_sales_indata))
eu_sales_smootheddf <- as.data.frame(cbind(eu_sales_timevals_in, as.vector(eu_sales_smoothedseries)))
colnames(eu_sales_smootheddf) <- c('Month', 'Sales')

# Using Linear Regression to model Sales
eu_sales_lmfit <- lm(Sales ~ sin(0.4*Month) * poly(Month,1) + cos(0.09*Month) * poly(Month,1), Month, data=eu_sales_smootheddf)

# Summary of model and accuracy
summary(eu_sales_lmfit)
accuracy(eu_sales_lmfit)

# Predict Sales using the LR model
eu_sales_global_pred <- predict(eu_sales_lmfit, Month=eu_sales_timevals_in)
summary(eu_sales_global_pred)
lines(eu_sales_timevals_in, eu_sales_global_pred, col='blue', lwd=2)
legend(x="topleft", y=10, legend=c("Sales", "Smoothed Line", "Predicted"), col=c("black", "red", "blue"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.55)
# The predicted values from linear regression are quite close to the actual values 
# and captures trend and seasonal behaviour well

# Now, let us look at the locally predictable series
# Subtract the predicted global trend and seasonality from the original time series
eu_sales_local_pred <- eu_sales_ts - eu_sales_global_pred
# What remains is a stationary time series. This should now contain local predictable auto regressive
# part (if any) and white noise

plot(eu_sales_local_pred, col='red', type = "l", main = "Stationary Sales Time Series for EU Consumer", xlab = "Time", ylab="Total Sales")
# The graph is flat and has no long-term predictability such as trend or seasonality.

# Let us check for weak stationarity i.e is there any local predictable behaviour still left in the time series?
acf(eu_sales_local_pred, main="ACF Plot for EU Consumer Sales Time Series")
acf(eu_sales_local_pred, type="partial", main="PACF Plot for EU Consumer Sales Time Series")

eu_sales_autoarima_ts <- auto.arima(eu_sales_local_pred)
tsdiag(eu_sales_autoarima_ts)
eu_sales_autoarima_ts
# ARIMA(0,0,0) with zero mean. This implies that there is no AR(p) or MA(q) series in the local component

# Let us test the residual for white noise
resi_eu_sales <- eu_sales_local_pred - fitted(eu_sales_autoarima_ts)

# White noise test
qqnorm(resi_eu_sales)
# Plot of white noise shows a linear graph

# Dickey-Fuller Test
adf.test(resi_eu_sales,alternative = "stationary")
# Augmented Dickey-Fuller Test shows a p-value of 0.01 which is < 0.05
# Residual time series is stationary and just white noise

# KPSS Test
kpss.test(resi_eu_sales)
# KPSS test shows a p-value of 0.1 with is > 0.05
# Residual time series is stationary and just white noise

# Model Evaluation of Classical Decomposition for Sales
# Forecast Sales using for the last 6 months i.e months 43 to 48
eu_sales_outdata <- eu_segment[43:48, ]
eu_sales_timevals_out <- c(43:48)

eu_sales_global_pred_out <- predict(eu_sales_lmfit,data.frame(Month=eu_sales_timevals_out))

#Now, let's compare our prediction with the actual values, using MAPE
eu_sales_MAPE_class_dec <- accuracy(eu_sales_global_pred_out,eu_sales_outdata$Sales)[5]
eu_sales_MAPE_class_dec
# MAPE is 20.73

# Let us plot the predictions along with original values, to get a visual feel of the fit
eu_sales_class_dec_pred <- c(ts(eu_sales_global_pred),ts(eu_sales_global_pred_out))
plot(eu_sales_total_ts, col = "black", main = "Sales Forecast for EU Segment using Classical Decomposition", xlab ="Time", ylab ="Total Sales")
lines(eu_sales_class_dec_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Sales", "Predicted Sales"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.70)

# Using Auto Arima model
eu_sales_autoarima <- auto.arima(eu_sales_ts)
eu_sales_autoarima
# The auto arima model was ARIMA(2,1,0)  
# This means that 1 stage differencing was performed and the resulting timeseries was 
# modeled as AR(2).
tsdiag(eu_sales_autoarima)
plot(eu_sales_autoarima$x, col="black", main = "Sales Time Series for EU Consumer using ARIMA", xlab = "Time", ylab="Total Sales")
lines(fitted(eu_sales_autoarima), col="red")

# Let us check if the residual series Auto Arima model is white noise
resi_eu_sales_auto_arima <- eu_sales_ts - fitted(eu_sales_autoarima)

adf.test(resi_eu_sales_auto_arima,alternative = "stationary")
kpss.test(resi_eu_sales_auto_arima)
# Residual time series after Auto Arima is indeed stationary which means it is pure white noise

# Auto Arima Model Evaluation for Sales
# Forecast Sales using for the last 6 months i.e months 43 to 48
eu_sales_auto_arima_forecast <- predict(eu_sales_autoarima, n.ahead = 6)

#Now, let's compare our prediction with the actual values, using MAPE
eu_sales_MAPE_autoarima <- accuracy(eu_sales_auto_arima_forecast$pred,eu_sales_outdata$Sales)[5]
eu_sales_MAPE_autoarima
# MAPE is 28.92, which is higher than the Classical Decomposition model

# Let us plot the predictions of Auto Arima along with original values, to get a visual feel of the fit
eu_sales_auto_arima_pred <- c(fitted(eu_sales_autoarima),ts(eu_sales_auto_arima_forecast$pred))
plot(eu_sales_total_ts, col = "black", main = "Sales Forecast for EU Segment using Auto Arima", xlab ="Time", ylab ="Total Sales")
lines(eu_sales_auto_arima_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Sales", "Predicted Sales"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.70)

#############################################################################################
############################   EU Consumer Demand Forecast  #################################
#############################################################################################

# Create Time Series for Demand on all data for this segment
eu_demand_total_ts <- ts(eu_segment$Quantity)
plot(eu_demand_total_ts, main = "Demand Time Series for EU Consumer", xlab = "Time", ylab="Total Demand")
lines(lowess(eu_demand_total_ts), col="red")
legend(x="topleft", y=10, legend=c("Demand", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
# There is a clear increasing global trend with some seasonality

# Seperate out last 6 months data from sample so that we can build a model and use it to
# predict these values and then cross-check them later
eu_demand_indata <- eu_segment[1:(nrow(eu_segment)-6), ]

# Create Time Series data for Demand
eu_demand_ts <- ts(eu_demand_indata$Quantity)

# Plot Demand Time Series for 1 to 42 months
plot(eu_demand_ts, main = "Demand Time Series for EU Consumer", xlab = "Time", ylab="Total Demand")

# Smoothen series using moving average method with a window size of 3
w <-1
eu_demand_smoothedseries <- stats::filter(eu_demand_ts,filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series
diff <- eu_demand_smoothedseries[w+2] - eu_demand_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  eu_demand_smoothedseries[i] <- eu_demand_smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(eu_demand_ts)
diff <- eu_demand_smoothedseries[n-w] - eu_demand_smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  eu_demand_smoothedseries[i] <- eu_demand_smoothedseries[i-1] + diff
}

# Plot the smoothed Demand time series
plot(eu_demand_ts, main = "Demand Time Series With Smoothing for EU Consumer", xlab = "Time", ylab="Total Demand")
lines(eu_demand_smoothedseries, col="red", lwd=2)
legend(x="topleft", y=10, legend=c("Demand", "Smoothed Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.70)
# The Time Series for Demand appear to be smoothed well

# Convert the demand time series to a dataframe
eu_demand_timevals_in <- c(1:nrow(eu_demand_indata))
eu_demand_smootheddf <- as.data.frame(cbind(eu_demand_timevals_in, as.vector(eu_demand_smoothedseries)))
colnames(eu_demand_smootheddf) <- c('Month', 'Demand')

# Using Linear Regression to model Demand
eu_demand_lmfit <- lm(Demand ~ sin(0.5*Month) * poly(Month,1) + cos(0.09*Month) * poly(Month,1) +
                        tan(0.02*Month), Month, data=eu_demand_smootheddf)

# Summary of model and accuracy
summary(eu_demand_lmfit)
accuracy(eu_demand_lmfit)

# Predict Demand using the LR model
eu_demand_global_pred <- predict(eu_demand_lmfit, Month=eu_demand_timevals_in)
summary(eu_demand_global_pred)
lines(eu_demand_timevals_in, eu_demand_global_pred, col='blue', lwd=2)
legend(x="topleft", y=10, legend=c("Demand", "Smoothed Line", "Predicted"), col=c("black", "red", "blue"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.70)
# The predicted values from linear regression are quite close to the actual values 
# and captures trend and seasonal behaviour reasonably well

# Now, let us look at the locally predictable series
# Subtract the predicted global trend and seasonality from the original time series
eu_demand_local_pred <- eu_demand_ts - eu_demand_global_pred
# What remains is a stationary time series. This should now contain local predictable auto regressive
# part (if any) and white noise

plot(eu_demand_local_pred, col='red', type = "l", main = "Stationary Demand Time Series for EU Consumer", xlab = "Time", ylab="Total Demand")
# The graph is flat and has no long-term predictability such as trend or seasonality.

# Let us check for weak stationarity i.e is there any local predictable behaviour still left in the time series?
acf(eu_demand_local_pred, main="ACF Plot for EU Consumer Demand Time Series")
acf(eu_demand_local_pred, type="partial", main="PACF Plot for APAC Consumer Demand Time Series")

eu_demand_autoarima_ts <- auto.arima(eu_demand_local_pred)
tsdiag(eu_demand_autoarima_ts)
eu_demand_autoarima_ts
# ARIMA(0,0,0) with zero mean. This implies that there is no AR(p) or MA(q) series in the local component

# Let us test the residual for white noise
resi_eu_demand <- eu_demand_local_pred - fitted(eu_demand_autoarima_ts)

# White noise test
qqnorm(resi_eu_demand)
# Plot of white noise shows a linear graph

# Dickey-Fuller Test
adf.test(resi_eu_demand,alternative = "stationary")
# Augmented Dickey-Fuller Test shows a p-value of 0.02 which is < 0.05
# Residual time series is stationary and just white noise

# KPSS Test
kpss.test(resi_eu_demand)
# KPSS test shows a p-value of 0.1 with is > 0.05
# Residual time series is stationary and just white noise

# Model Evaluation of Classical Decomposition for Demand
# Forecast Demand using for the last 6 months i.e months 43 to 48
eu_demand_outdata <- eu_segment[43:48, ]
eu_demand_timevals_out <- c(43:48)

eu_demand_global_pred_out <- predict(eu_demand_lmfit,data.frame(Month=eu_demand_timevals_out))

#Now, let's compare our prediction with the actual values, using MAPE
eu_demand_MAPE_class_dec <- accuracy(eu_demand_global_pred_out,eu_demand_outdata$Quantity)[5]
eu_demand_MAPE_class_dec
# MAPE is 21.98

# Let us plot the predictions along with original values, to get a visual feel of the fit
eu_demand_class_dec_pred <- c(ts(eu_demand_global_pred),ts(eu_demand_global_pred_out))
plot(eu_demand_total_ts, col = "black", main = "Demand Forecast for EU Segment using Classical Decomposition", xlab ="Time", ylab ="Total Demand")
lines(eu_demand_class_dec_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Demand", "Predicted Demand"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

# Using Auto Arima model
eu_demand_autoarima <- auto.arima(eu_demand_ts)
eu_demand_autoarima
# The auto arima model was ARIMA(2,1,0)  
# This means that 1 stage differencing was performed and the resulting timeseries was 
# modeled as AR(2).
tsdiag(eu_demand_autoarima)
plot(eu_demand_autoarima$x, col="black", main = "Demand Time Series for EU Consumer using ARIMA", xlab = "Time", ylab="Total Demand")
lines(fitted(eu_demand_autoarima), col="red")

# Let us check if the residual series Auto Arima model is white noise
resi_eu_demand_auto_arima <- eu_demand_ts - fitted(eu_demand_autoarima)

adf.test(resi_eu_demand_auto_arima,alternative = "stationary")
kpss.test(resi_eu_demand_auto_arima)
# Residual time series after Auto Arima is indeed stationary which means it is pure white noise

# Auto Arima Model Evaluation for Demand
# Forecast Demand using for the last 6 months i.e months 43 to 48
eu_demand_auto_arima_forecast <- predict(eu_demand_autoarima, n.ahead = 6)

#Now, let's compare our prediction with the actual values, using MAPE
eu_demand_MAPE_autoarima <- accuracy(eu_demand_auto_arima_forecast$pred,eu_demand_outdata$Quantity)[5]
eu_demand_MAPE_autoarima
# MAPE is 30.13, which is higher than the Classical Decomposition model

# Let us plot the predictions of Auto Arima along with original values, to get a visual feel of the fit
eu_demand_auto_arima_pred <- c(fitted(eu_demand_autoarima),ts(eu_demand_auto_arima_forecast$pred))
plot(eu_demand_total_ts, col = "black", main = "Demand Forecast EU Segment using Auto Arima", xlab ="Time", ylab ="Total Demand")
lines(eu_demand_auto_arima_pred, col = "red")
legend(x="topleft", y=10, legend=c("Original Demand", "Predicted Demand"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)

# On comparing the MAPE values for EU Consumer and APAC Consumer Sales and Demand, the Classical Decomposition model performes 
# better that the Auto ARIMA model

#############################################################################################
#########################  Future Forecast of Sales and Demand  #############################
#############################################################################################
# Use Classical Decomposition to predict Sales and Demand for next 6 months in future for APAC segment

# Common function of predict future values
forecastFutureValues <- function(market, segment, sales_lmfit, demand_lmfit) {
  future_order_dates <- c("2015-01","2015-02","2015-03","2015-04","2015-05","2015-06")
  
  # Predict Sales
  future_sales_forecast <- predict(sales_lmfit, data.frame(Month=c(49:54)))
  apac_future_forecast <- data.frame(Market=market, Segment=segment, OrderDate=future_order_dates, Sales=c(future_sales_forecast))
  
  # Predict Demand
  future_demand_forecast <- predict(demand_lmfit, data.frame(Month=c(49:54)))
  apac_future_forecast <- cbind(apac_future_forecast, data.frame(Quantity=c(future_demand_forecast), Profit=""))
  
  return(apac_future_forecast)
}

# APAC Sales and Demand Forecast
apac_future_forecast <- forecastFutureValues("APAC", "Consumer", apac_sales_lmfit, apac_demand_lmfit)

# Add forecasted values to original data
apac_segment <- rbind(apac_segment, apac_future_forecast)
apac_segment

# Common function to plot Sales and Demand graphs
plotSalesAndDemandGraphs <- function(salesTimeSeries, sales_title, demandTimeSeries, demand_title) {
  # Sales
  plot(salesTimeSeries, main = sales_title, xlab = "Time", ylab="Total Sales")
  lines(lowess(salesTimeSeries), col="red")
  legend(x="topleft", y=10, legend=c("Sales", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
  
  # Demand
  plot(demandTimeSeries, main = demand_title, xlab = "Time", ylab="Total Demand")
  lines(lowess(demandTimeSeries), col="red")
  legend(x="topleft", y=10, legend=c("Demand", "Trend Line"), col=c("black", "red"), lty = c(1,1), lwd=c(2.5,2.5), pch=c(".",".", ".", "."),  cex = 0.75)
}

# Construct time series with future values for APAC Consumer
apac_future_sales_total_ts <- ts(apac_segment$Sales)
apac_future_demand_total_ts <- ts(apac_segment$Quantity)

# Plot Graphs
plotSalesAndDemandGraphs(apac_future_sales_total_ts, "Forecasted Sales Time Series for APAC Consumer",
                         apac_future_demand_total_ts, "Forecasted Demand Time Series for APAC Consumer")

# EU Sales and Demand Forecast
eu_future_forecast <- forecastFutureValues("EU", "Consumer", eu_sales_lmfit, eu_demand_lmfit)

# Add forecasted values to original data
eu_segment <- rbind(eu_segment, eu_future_forecast)
eu_segment

# Construct time series with future values for EU Consumer
eu_future_sales_total_ts <- ts(eu_segment$Sales)
eu_future_demand_total_ts <- ts(eu_segment$Quantity)
# Plot Graphs
plotSalesAndDemandGraphs(eu_future_sales_total_ts, "Forecasted Sales Time Series for EU Consumer",
                         eu_future_demand_total_ts, "Forecasted Demand Time Series for EU Consumer")

####
