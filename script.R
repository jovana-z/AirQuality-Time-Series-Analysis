setwd("C:/Users/username/Desktop/madrid")
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(gridExtra)


#### reading the data into the environment
list <- list.files()
for(i in list){
  assign(substr(i,1,11), read.csv(file = i, stringsAsF=actors=FALSE)[ ,c("date", "BEN", "CO", "EBE", "NMHC","NO_2", "O_3","PM10","SO_2","TCH","TOL","station")])
}

madrid <- rbind(madrid_2001, madrid_2002, madrid_2003, madrid_2004, 
                madrid_2005, madrid_2006, madrid_2007, madrid_2008, 
                madrid_2009, madrid_2010, madrid_2011, madrid_2012, 
                madrid_2013, madrid_2014, madrid_2015, madrid_2016, 
                madrid_2017, madrid_2018)

madrid$date <- as.Date(madrid$date, format="%Y-%m-%d") #adding date column

madrid_mean <- madrid %>% group_by(date) %>%
  summarise_all(mean, na.rm=TRUE)

## splitting up the date to get month and day columns so we can look at those too
madrid <- madrid %>% mutate(year = lubridate::year(date), 
                            month = lubridate::month(date), 
                            day = lubridate::day(date))

# Here we're creating new variables that are averaged values of the pollutants (daily)
madrid_mean <- dplyr::mutate(madrid_mean, total = 
                               rowSums(madrid_mean[,-c(1,12,13,14,15)], 
                                       na.rm = TRUE))


####### first plot of daily total average of pollutants
p1 <- ggplot(madrid_mean, aes(x = date, y = total)) + geom_line() + 
  scale_x_date(breaks = seq(as.Date("2001-01-01"), as.Date("2018-07-01"), by="12 months"), date_labels = "%Y")
p1
#### this looks like an unstationary time series with variance and mean


#### same thing as above but grouping by monthly data. also create time variable that looks at measurement on
#### 15th of every month over the course of 2001 to 2018

madrid_mean_monthly <- madrid_mean %>% group_by(month, year) %>%
  summarise_all(mean, na.rm=TRUE)

p2 <- ggplot(madrid_mean_monthly, aes(x = date, y = total, group = 1)) + 
  geom_line(color = 'deeppink') + 
  geom_point(aes(x = date, y = total), size = 0.6, color = 'black') + ggtitle("Average Monthly Measure of Pollutants in Madrid") + 
  xlab('Year') + ylab('Total Pollution') +
  theme_light()
p2

plot(madrid_season, type='l', ylim=c(125,160))
Month=c('J','F','M','A','M','J','J','A','S','O','N','D')
points(madrid_season, pch=as.vector(Month))


## let's see if there's seasonality 

madrid_season <- madrid_mean_monthly %>% 
  select(month,total) %>%
  group_by(month) %>%
  summarise_all(mean, na.rm=TRUE)

plot(madrid_season, type='l', ylim=c(125,160))
Month=c('J','F','M','A','M','J','J','A','S','O','N','D')
points(madrid_season, pch=as.vector(Month))

# looking at the different pollutants monthly avgs, on the 1st of each month
# for all the years

p4<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=BEN)) + 
  geom_line() + labs(x = "Month")
p5<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=CO)) + 
  geom_line() + labs(x = "Month")
p6<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=EBE)) + 
  geom_line() + labs(x = "Month")
p7<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=NMHC)) + 
  geom_line() + labs(x = "Month")
p8<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=NO_2)) + 
  geom_line() + labs(x = "Month")
p9<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=O_3)) + 
  geom_line() + labs(x = "Month")
p10<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=PM10)) + 
  geom_line() + labs(x = "Month")
p11<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=SO_2)) + 
  geom_line() + labs(x = "Month")
p12<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=TCH)) + 
  geom_line() + labs(x = "Month")
p13<-ggplot(madrid_mean_monthly, aes(x=as.Date(date, format="%Y - %m"),y=TOL)) + 
  geom_line() + labs(x = "Month")

grid.arrange(p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) + theme_classic()

#### overall there looks to be seasonality in our data as well!
#### i.e. air quality gets worse in the summer months
#### so we need to extract this seasonality!!!

######################
madrid_mean_monthly$date <- as.Date(madrid_mean_monthly$date)
madrid_mean_monthly <- dplyr::arrange(madrid_mean_monthly, date)

madrid_ts <- ts(madrid_mean_monthly[,16], frequency=12, start=c(2001,1))

acf(madrid_ts, lag.max=48)

'''Its clear from Figure that our time series has a seasonal component because of the annual recurring
nature of the increase values in the ACF. 
This means we need to transform the data to remove the seasonality, and also the non-constant mean'''

library(TSA)
library(MASS)

BoxCox.ar(madrid_ts, lambda=seq(-0.4,-0.3,0.01))

madrid_ts_bc <- madrid_ts**(-0.36)
madrid_ts_bc %>%
  ggtsdisplay(xlab="Year",
              main="Total Pollution Power Transformed with lambda=-0.36")
diff12 = diff(madrid_ts_bc, lag=12) 
diff12 %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally Differenced and Transformed Pollution Levels")

plot(decompose(diff12))

diff1 %>%
  ggtsdisplay(xlab="Year",
              main="Differenced and Power Transformed Pollution Levels")

fit <- auto.arima(madrid_ts)

qqnorm(fit$residuals)
qqline(fit$residuals)
shapiro.test(fit$residuals) 
Box.test(resid(fit))

'''The Ljung-Box test has a p-value greater than 0.05 which means we dont have enough evidence to reject the null hypothesis which is that the model does not exhibit a lack of fit. Similarily, the Shapiro Wilks test has a p-value greater 
than 0.05 which helps confirm normality. This further confirms our model is working well.'''

train <- head(madrid_ts, round(length(ts) * 0.9))
h <- length(madrd_ts) - length(train)
test <- tail(madrid_ts, h)

ts.train <- Arima(train, order=c(2,1,1))
ts.train %>%
  forecast(h=60) %>%
  autoplot() + autolayer(test)
