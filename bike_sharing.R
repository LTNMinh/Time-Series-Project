# Import Package
library(ggplot2)      # Data visualization
library(readr)        # CSV file I/O, e.g. the read_csv function
library(forecast)
library(tseries)
library(astsa)
library(GGally)
library(fastDummies)
library(lmtest)
library(stargazer)
library(Metrics)
library(seasonal)

rm(list = ls(all = TRUE))
graphics.off()

# Get data Setworking Dir First! 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

daily_data <- read.csv(file="./Bike-Sharing-Dataset/day.csv",
                       header=TRUE, sep=",", 
                       na.strings=c(""))

# View data
# View(daily_data)

# --> Data sets started from 2011-01-01 to 2012-12-31
daily_data$Date = as.Date(daily_data$dteday) # create toDate with column dteday

# Plot Time Series
ggplot(daily_data, aes(Date, cnt)) + 
  geom_line(size=0.5) + 
  scale_x_date('Month', breaks=('year')) +
  theme_bw() +
  ylab("Daily Bike Checkouts") + xlab("") 


# Function to check outlier with box-plot
date_range <- daily_data$Date
measure <- daily_data$cnt

# create new columns for the months and years, and 
# and a year_month column for x-axis labels
daily_data$month <- format(date_range, format="%b")
daily_data$year <- as.POSIXlt(date_range)$year + 1900
daily_data$year_month <- paste(daily_data$year, daily_data$month)
daily_data$sort_order <- daily_data$year *100 + as.POSIXlt(date_range)$mon

#plot it
ggplot(daily_data) + 
      geom_boxplot(aes(x=reorder(year_month, 
                                 sort_order), 
                       y=measure)) + 
      ylab("Daily Bike Checkouts") + xlab("") +  
      theme_bw() +
          theme(text = element_text(size=10), 
                  axis.text.x = element_text(angle=90, hjust=1)) 

# Delete Outlier 
count_ts = ts(daily_data[, c('cnt')],frequency = 30)
daily_data$cnt_ma = ma(tsclean(count_ts), order=7)
daily_data$clean_cnt = as.numeric(tsclean(count_ts))


#------------------------ Linear Regression --------------------#
# Correlation Exploring 
keep <- c('instant','Date','season','holiday','workingday','weathersit'
          ,'temp','atemp','hum','windspeed','cnt','clean_cnt')
temp <- daily_data[keep]
ggpairs(temp,columns = c('temp','atemp','hum','windspeed','clean_cnt'))

# ANOVA testing 
temp$season <- as.factor(temp$season)
aov1 = aov(atemp ~ season ,data = temp )
summary(aov1)
aov2 = aov(hum ~ season ,data = temp )
summary(aov2)
aov3 = aov(windspeed ~ season ,data = temp )
summary(aov3)

temp$weathersit <- as.factor(temp$weathersit)
aov4 = aov(atemp ~ weathersit ,data = temp )
summary(aov4)
aov5 = aov(hum ~ weathersit ,data = temp )
summary(aov5)
aov6 = aov(windspeed ~ weathersit ,data = temp )
summary(aov6)


# Create Dummies Variable 
temp <- dummy_cols(temp,select_columns = c('season'))
temp <- dummy_cols(temp,select_columns = c('weathersit'))
temp$weathersit_3 <- NULL
temp$season_4 <- NULL

# Slipt Train, Test set. 
train_count <- ts(count_ts[-(701:731)],frequency = 30)
test_count <- count_ts[(701:731)]

train <- temp[-(701:731),]
test  <- temp[(701:731),]

# Linear Model with dummies Variables 
lm.cnt <- lm(
  clean_cnt ~ instant + season_1 + season_2 + season_3 + 
        weathersit_1 + weathersit_2 + holiday + workingday +
        atemp + hum + windspeed,
  data=train)
summary(lm.cnt)

# Test 
bptest(lm.cnt)  # Variance test
checkresiduals(lm.cnt) # Residual test
# Checking Nornal Residual 
qqnorm(resid(lm.cnt), main = "Normal Q-Q Plot,Residual", col = "darkgrey")
qqline(resid(lm.cnt), col = "dodgerblue", lwd = 2)


# Violate the Error Assumption 
# Fitting data test set 
cnt_predict <- predict(lm.cnt,test)
# Plot two timeseries 
ggplot() + 
  geom_line(data = test,aes(x=Date,y=cnt_predict,   
                            colour = "Predict")) + 
  geom_line(data = train,aes(x=Date, y = clean_cnt, 
                             colour = "Data")) + 
  geom_line(data = test,aes(x=Date,y=clean_cnt  ,  
                            colour = "Data")) + 
  scale_color_grey(start = 0.7, end = 0.1) + 
  theme_bw() +
  xlab("Month") + ylab("Daily Count") +
  ggtitle("Data and Prediction")


ggplot() + 
  geom_line(data = train,aes(x=Date, y = clean_cnt, 
                             colour = "Data")) + 
  geom_line(data = train,aes(x=Date, y = fitted(lm.cnt),
                              colour = "Fitted")) +
  geom_line(data = test,aes(x=Date,y=clean_cnt  ,  
                            colour = "Data")) + 
  scale_color_grey(start = 0.7, end = 0.1) + 
  xlab("Month") + ylab("Daily Count") + 
  theme_bw() +
  ggtitle("Data vs. Fitted ")



#------------------------- Holtwinter Forecast ---------------------#
daily_data$cnt_ma
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
train_count_ma <-  ts(frequency = 30,count_ma[-(700:725)])
hwfit <-  HoltWinters(train_count_ma)
hwforcast <- forecast(hwfit,31)




data_ma <- count_ma
autoplot(data_ma) +
  autolayer(hwforcast,colour = 'grey') + 
  autolayer(data_ma) +
  theme_bw() +
  scale_color_grey(start = 0.2, end = 0) +
  scale_x_continuous(breaks= c(1,13,24.25),
                     labels= c("2011-Jan","2012-Jan","2012-Dec")) +
  xlab("Month") + ylab("Daily Count")

data <- count_ts
autoplot(data) +
  autolayer(hwforcast,colour = 'grey') + 
  autolayer(data) +
  theme_bw() +
  scale_color_grey(start = 0.2, end = 0) +
  scale_x_continuous(breaks= c(1,13,24.25),
                     labels= c("2011-Jan","2012-Jan","2012-Dec")) +
  xlab("Month") + ylab("Daily Count")

# Testing Residual 
Box.test(hwforcast$residual, lag=20, type="Ljung-Box")


#-------------------ARIMA model----------------------------------#

# Calculate seasonal component of the data using stl()

# count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
# train_count_ma_log <- log(train_count_ma)
# plot(train_count_ma_log)

decomp = stl(train_count_ma, s.window="periodic") # STL calculates the seasonal component of the series using smoothing, and adjusts the original series by subtracting seasonality
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
acf2(deseasonal_cnt)


# Check stationarity 
adf.test(train_count_ma, alternative = "stationary")
# Autocorrelations and Choosing Model Order

acf(train_count_ma, main='ACF plot with seasonal component') # ACF plots display correlation between a series and its lags
pacf(train_count_ma, main='PACF plot  with seasonal component') # PACF display correlation between a variable and its lags that is not explained by previous lags
acf2(train_count_ma)
# Plotting the differenced series, 
#  we see an oscillating pattern around 0 with no visible strong trend

count_d1 = diff(train_count_ma, differences = 1)
plot(count_d1, main="Bicycle usage after decomposion, difference 1 and moving average 30")
adf.test(count_d1, alternative = "stationary")



# Spikes at particular lags of the differenced series 
# can help inform the choice of p or q
Acf(count_d1, main='ACF for Differenced Series')
# Following lags are significant for MA(q)
# Lag 1, Lag 2, Lag 3, Lag 7, Lag 8
# The order q of MA terms can be 1,2,3,7
Pacf(count_d1, main='PACF for Differenced Series')
acf2(count_d1)
# Following lags are significant for AR(p)
# Lag 1, Lag 2, Lag 6, Lag 7, Lag 8, Lag 13, Lag 14
# The order p of AR terms can be 1,2,3,7



# Manual calculation with arima d=1 # difference
d = 1
for(p in 1:4){
  for(q in 1:4){ if(p+d+q<=6){
    model<-arima(x=deseasonal_cnt, order = c((p-1),d,(q-1))) 
    pval<-Box.test(model$residuals, lag=log(length(model$residuals))) 
    sse<-sum(model$residuals^2)
    cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
  } }
}

# We choose model with smallest AIC 
# (2 1 0) AIC= 9422.642 SSE= 18871035 p-VALUE= 0.2980812 
arima(x=deseasonal_cnt, order = c(2,1,0))
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(2,1,0) Model Residuals')

# There is a clear pattern present in ACF/PACF 
# Model residuals plots repeating at lag 7. 
# our model may be better off with a different specification, 
# such as p = 7 or q = 7

# Make p+d+q<=12 means that the model will be very complex
for(p in 1:8){
  for(q in 1:8){
    if(p+d+q<=12){
      model<-arima(x=deseasonal_cnt, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

# ARIMA(1 1 7) model has the smallest AIC value
# (1 1 7) AIC= 8688.101  SSE= 9857393  p-VALUE= 0.2897584 
model.arima <- arima(x=deseasonal_cnt, order = c(1,1,7))
arima.forcast <- forecast(model.arima, h=31)

data <- count_ma
autoplot(data) +
  autolayer(arima.forcast,colour = 'grey') + 
  autolayer(data) +
  theme_bw() +
  scale_color_grey(start = 0.2, end = 0) +
  scale_x_continuous(breaks= c(1,13,24.25),
                     labels= c("2011-Jan","2012-Jan","2012-Dec")) +
  xlab("Month") + ylab("Daily Count")

#-------------------Compare Model on Testing Set-----------------#

# Holt 
mse(test_count,hwforcast$mean)
mae(test_count,hwforcast$mean)
mape(test_count,hwforcast$mean)
# Linear 
mse(test$clean_cnt,cnt_predict)
mae(test$clean_cnt,cnt_predict)
mape(test$clean_cnt,cnt_predict)
#ARIMA
mse(test_count,arima.forcast$mean)
mae(test_count,arima.forcast$mean)
mape(test_count,arima.forcast$mean)

#------Test-----#
myts <-  ts(train,frequency=30)
timeseriesln <- tslm(clean_cnt ~ instant + season_1 + season_2 + season_3 + 
                       weathersit_1 + weathersit_2 + holiday + workingday +
                       atemp + hum + windspeed,
                     data=myts)
checkresiduals(timeseriesln)
fcast <- forecast(timeseriesln,test)  

data <- count_ts
autoplot(data) + autolayer(fcast,color='grey') + autolayer(data) + theme_bw() +
  scale_color_grey(start = 0.2, end = 0) +
  scale_x_continuous(breaks= c(1,13,24.25),
                     labels= c("2011-Jan","2012-Jan","2012-Dec")) +
  xlab("Month") + ylab("Daily Count")
  acf2(timeseriesln$residuals) 

