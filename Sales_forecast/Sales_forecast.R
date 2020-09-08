library(readxl)

#read the csv file
sales_project_7 <- read_excel("C:/Users/KISHOR/Desktop/Sales_forecast/sales_project 7.xlsx")
View(sales_project_7)

sapply(sales_project_7,class)
#convert  Order type from character to factor
sales_project_7$`Order Type` <- as.factor(sales_project_7$`Order Type`)
levels(sales_project_7$`Order Type`)

summary(sales_project_7$`Order Date`)

sales_project_7 <- sales_project_7[,-c(4,5)]#drop columns  order type and state

sale1 <- sales_project_7

#sales_project_7 <- sales_project_7[sales_project_7$`Order Type`!="payment",]
#sales_project_7 <- sales_project_7[sales_project_7$`Order Type`!="refund",]

library(lubridate)
sales_project_7$`Order Date` <- as.Date(sales_project_7$`Order Date`, "%Y%m%d")
sapply(sales_project_7,class)

which(is.na(sales_project_7$`Order Date`))
#found NAs
sales_project_7 <- sales_project_7[-c(180142,180144),]

library(dplyr)
#data sorted
sales_new <- dplyr::arrange(sales_project_7,sales_project_7$`Order Date`)

sales_new[is.na(sales_new)] <- 0 #NAs to zero

sales_new <- sales_new[,-3]

library(data.table)
sales_new <- data.table(sales_new)
sales_new <- sales_new[, mean(Sales), by = `Order Date`]

colnames(sales_new) <- c("Order Date","Sales")

#
#sales_new$day <- weekdays(as.Date(sales_new$`Order Date`))

#plot(DT1,type="l")#no seasonality or trend

library(ggplot2)
library(forecast)
library(tseries)

ggplot(sales_new,aes(`Order Date`,Sales))+geom_line()+scale_x_date('month')+ylab("Sales")+xlab("")
sales_ts = ts(sales_new[,c('Sales')])
#removing outliers as there are sales amount that are beynd 50000
sales_new$clean_sales = tsclean(sales_ts)
ggplot() +geom_line(data = sales_new, aes(x = `Order Date`, y = clean_sales)) + ylab('Cleaned Sales Data')

#plotting the moving average for weekly and monthly using cleaned data
sales_new$sale_ma = ma(sales_new$clean_sales,order = 7)
sales_new$sale_ma30 = ma(sales_new$clean_sales,order = 30)

#plotted MAs with actual data
ggplot() +
  geom_line(data = sales_new, aes(x = `Order Date`, y = clean_sales, colour = "Sales")) +
  geom_line(data = sales_new, aes(x = `Order Date`, y = sale_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = sales_new, aes(x = `Order Date`, y = sale_ma30, colour = "Monthly Moving Average"))  +
  ylab('Sales')

#deseasonlize data
count_ma = ts(na.omit(sales_new$sale_ma),frequency = 30)
decomp = stl(count_ma,s.window = "periodic",)
deseasonal_sales = seasadj(decomp)
plot(decomp)

#decomp1 = stl(count_ma,s.window = "periodic",allow.multiplicative.trend=TRUE)

#checking stationary with Dickey_Fuller test
adf.test(count_ma, alternative = "stationary")#p-value =0.3355(non -stationary)

Acf(count_ma,main='')#geometric(many lags)
Pacf(count_ma,main='')#lags at 1,2,3,8,15

#differencing is needded to make the data stationary
#first taking d=1
count_d1 = diff(deseasonal_sales,differences = 1)
plot(count_d1)
adf.test(count_d1,alternative = "stationary")#p-value = 0.01 (<0.05)
#so data has now become stationary
#plotting acf and pacf on the differenced series

Acf(count_d1,main='Acf for differenced series')
#lags at 1..7
Pacf(count_d1,main='Pacf for differenced series')
#lags at 1,2,..,7,14
#so we need to check for values ranging 1 to 7 of p and q

#splitting train and test data of deaseasonalized sales data
# train_sale <- deseasonal_sales[1:320]
# test_sale <- deseasonal_sales[321:419]

# fit1 <- auto.arima(train_sale,seasonal = FALSE)
# fit1
# tsdisplay(residuals(fit1),lag.max = 40,main='(2,1,2)Model residuals')
# #still there are lags at 7 specifically in both the acf and pacf plots
# #also the AIC = 4276.81 and log likelihood=-2133.31 which is not good
# #so we should try with (2,1,7) or (7,1,2)

# fit2 <- arima(train_sale,order = c(2,1,7))
# fit2
# #log likelihood = -2057.3,  aic = 4134.61
# tsdisplay(residuals(fit2),lag.max = 40,main='(2,1,7)Model residuals')
# #no lags now

# fit3 <- arima(train_sale,order = c(1,1,7))
# fit3
# #log likelihood = -2057.34,  aic = 4132.68(lower than above)
# tsdisplay(residuals(fit3),lag.max = 40,main='(1,1,7)Model residuals')


hold <- window(ts(deseasonal_sales),start = 390)
fit_no_holdout <- arima(ts(deseasonal_sales[-c(390:419)]),order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=29)
plot(fcast_no_holdout,main="")
lines(ts(deseasonal_sales))
#the forecast doesnt match the actual data

#trying auto arima in the whole data including seasonality
fit_w_seasonality = auto.arima(deseasonal_sales, seasonal=TRUE)
fit_w_seasonality
#log likelihood=-2743.34 AIC=5506.69   AICc=5507.23   BIC=5547.04

#forecasting for the next 30 days
seas_fcast <- forecast(fit_w_seasonality,h=30)
plot(seas_fcast,main = "")
accuracy(fcast_no_holdout,hold)
#375.6383 13.959980
#ARIMA(3,1,4)(2,0,0)[30]

tsdisplay(residuals(fit_w_seasonality),lag.max = 20,main = "Seasonal model residuals")
#its found that  the acf and pacf plots are showing  lags at many places

#SIMPLE EXPONENTIAL SMOOTHING
library(tidyverse)
sales_ts = ts(sales_new[,c('Sales')])

#cleaning data
sales_new$clean_sales = tsclean(sales_ts)
sales_ses <- sales_new[,-c(1,2)]
sale.train <- window(sales_ses,end = 395)
sale.test <- window(sales_ses,start = 396)
ses.sale <- ses(sale.train,alpha = .2,h=30)
autoplot(ses.sale)

#differencing the data
sale.diff <- diff(sale.train)
autoplot(ts(sale.diff))
ses.sale.diff <- ses(sale.diff,alpha= .2,h=30)
autoplot(ses.sale.diff)
#created differenced test data

sale.diff.test <- diff(sale.test)
accuracy(ses.sale.diff,sale.diff.test)
#1126.444,120.1252
#797.5764 , 121.1201
#494.0383 , 770.8394

#identify optimal alpha value
alpha <- seq(.01,.99,by=.01)
RMSE <- NA
for(i in seq_along(alpha)){
     fit <- ses(sale.diff,alpha=alpha[i],h=30)
     RMSE[i] <- accuracy(fit,sale.diff.test)[2,2]
   }
   
#convert to a data frame and idenitify min alpha value
alpha.fit <- data.frame(alpha,RMSE)
alpha.min <- filter(alpha.fit,RMSE==min(RMSE))
#alpha = 0.04,RMSE=  1126.046
#0.04 ,797.0132
#0.01 ,464.3777

# plot RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")
  
#refit model with alpha = .01
ses.sale.opt <- ses(sale.diff, alpha = .01, h = 30)

# performance eval
accuracy(ses.sale.opt, sale.diff.test)
#1126.046,106.4788
#464.3777,151.2718

# plotting results
p1 <- autoplot(ses.sale.opt) 
p2 <- autoplot(ts(sale.diff.test)) +
  autolayer(ses.sale.opt, alpha = .4) +
  ggtitle("Predicted vs. actuals for the test data set")
gridExtra::grid.arrange(p1, p2, nrow = 1)
#so its seen SES doesnt perform well

#HOLT’S METHOD
holt.sale <- holt(sale.train,h=30)
autoplot(holt.sale)
holt.sale$model

#Smoothing parameters:
#alpha = 0.1928 , 0.2169 , 0.3159
#beta  = 1e-04 
#AIC     AICc      BIC 
#6464.118 6464.309 6482.960 
#6329.324 6329.515 6348.166 
#7886.640 7886.794 7906.534
accuracy(holt.sale,sale.test)
#381.6096 13.25246

# identify optimal beta parameter
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(sale.train, beta = beta[i], h = 30)
  RMSE[i] <- accuracy(fit, sale.test)[2,2]
}

# convert to a data frame and idenitify min beta value
beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))
#beta min = 0.278 ,0.279, 0.174

# plot RMSE vs. beta
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line() +
  geom_point(data = beta.min, aes(beta, RMSE), size = 2, color = "blue")
  
# new model with optimal beta
holt.sale.opt <- holt(sale.train, h = 30, beta = 0.174)

# accuracy of first model
accuracy(holt.sale, sale.test)
#2204.517,31.71329
#381.6096, 13.25246

# accuracy of new optimal model
accuracy(holt.sale.opt, sale.test)
#1797.488,30.67075
# 1996.238,31.50727
#339.4273, 11.32062

# on whole data
model <- holt(sales_ses,h = 30, beta = 0.174)
plot(model)
p1 <- autoplot(holt.sale) +
  ggtitle("Original Holt's Model")
p2 <- autoplot(holt.sale.opt) +
  ggtitle("Optimal Holt's Model")
gridExtra::grid.arrange(p1, p2, nrow = 1)

#HOLT WINTER’S METHOD
autoplot(decompose(sales_new))
sale.hw <- ets(sale.train,model="ZZZ")

#model formed M,N,N
autoplot(forecast(sale.hw))
summary(sale.hw)
checkresiduals(sale.hw)
sale.f1 <- forecast(sale.hw,h=30)

# check accuracy
accuracy(sale.f1, sale.test)
#1860.623,21.07005
#1644.195,20.55384
#481.4886 18.03020

#plotting
autoplot(sale.f1)

#FINAL MODEL 
#Final model holts(optimal)
#since it has low RMSE and MAPE value
#339.4273, 11.32062

# model on whole  cleaned data
model <- holt(sales_ses,h = 30, beta = 0.174)
fore_sale <- data.frame(model)
plot(model)

#create dates for next 30 days
startDate <- as.Date("2019-06-11")
xm <- seq(startDate,by = "1 day", length.out = 30)
xm
fore_sale <- cbind(xm,fore_sale)
