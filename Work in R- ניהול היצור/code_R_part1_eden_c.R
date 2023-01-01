#Q1
  a <- as.numeric(123)
  str(a)
  a <- as.character(123)
  str(a)
  a <- as.factor(123)
  str(a)
  b <- list(123)
  str(b)
  b1 <- list(1,"2",3)
  str(b1)
  c <- data.frame(b)
  str(c)
  str(b1)

#Q2
  DATA<-read.csv(file="Data316087337_NoaBarnov.csv",stringsAsFactors= FALSE)
  DATA.TS<-ts(DATA)
  str(DATA.TS)
  str(DATA)

#Q3
  library(forecast)
  library(ggplot2)
  library(ggpubr)
  qplot(data = DATA,y = Demand,x = 1:120,xlab = 'index', main = 'Data frame Plot')
  plot(DATA.TS, main = 'Time Series Plot')
  

#Q4
  time_series_4 <- ts(DATA.TS, end = 2022, frequency = 4) #Quarterly
  time_series_1 <- ts(DATA.TS, end = 2022, frequency = 1) #Yearly
  plot(time_series_4, main = 'Quarterly')
  plot(time_series_1, main = 'Yearly')
  

#Q5
  #creating time series by 12 month because this is our true frequency
    time_series_12 <- ts(DATA.TS, end = 2022, frequency = 12)
    time_series_12
  dat.lm1 <- tslm(time_series_12 ~ trend) 
  summary(dat.lm1) 
  #a+b
    dat.lm1$coefficients
  #c
    summary(dat.lm1) # we looked at the adjusted R squared
  #d
    plot(time_series_12, main ='Time Series Plot With A Regression Line')
    lines(dat.lm1$fitted, col="red")
  #e
    accuracy(dat.lm1)

#Q6
  autoplot(time_series_12, series = "Data_ma", ylab = 'Demands', xlab= 'Years') + autolayer(ma(time_series_12,order= 3), series = '3 months') + autolayer(ma(time_series_12,order= 6), series = '6 months') +ggtitle('Moving Average (3 months and 6 months)') 

#Q7
  #a,b,c
    fc_0.1 <- ses(DATA.TS, h=1, alpha = 0.1)
    fc_0.1$mean

    fc_0.5 <- ses(DATA.TS, h=1, alpha = 0.5)
    fc_0.5$mean
    
    fc_0.9 <- ses(DATA.TS, h=1, alpha = 0.9)
    fc_0.9$mean

  #d
    fc <- ses(DATA.TS, h=1)
    fc$model

  #f
    autoplot(DATA.TS, series = "data_ex", ylab = 'Demands', main ="The graph of the optimal alpha on the original data" ) + autolayer(fitted(fc),series = 'optimal alpha 0.1031') 

  #e
    round(accuracy(fc), 3)

#Q8
  #Holt-winters prediction in R
    #a
      DT <- DATA[1:115,]
    #b
      time_series_NEW <- ts(DT, start = c(2012,2), frequency = 12) #starting at February 2012 shown at time_series_12 at global environment
    #alpha,beta,gamma=0.9
      data.hw_0.9 <- HoltWinters(time_series_NEW, alpha = 0.9, beta = 0.9, gamma = 0.9)
      #prediction
        predict(data.hw_0.9,5)

    #d
      #alpha,beta,gamma=0.1
        data.HW_0.1 <- HoltWinters(time_series_NEW, alpha = 0.1, beta = 0.1, gamma = 0.1)
        #prediction 
          predict(data.HW_0.1,5)
          
    # without the alpha,beta,gamma
      data_c <- HoltWinters(time_series_NEW)
      predict(data_c,5) #the results changed to 805.7837

    #e
      forc0.9<-forecast(data.hw_0.9, h = 1)
      round(accuracy(forc0.9), 3)

      forc0.1<-forecast(data.HW_0.1, h = 1)
      round(accuracy(forc0.1), 3)
      
      forc<-forecast(data_c, h = 1)
      round(accuracy(forc), 3) #best RMSE

# Q9
  #a
    decomposedTimeSeries <- decompose(time_series_12)
    plot(decomposedTimeSeries)
  #b
    range_observed <- max(DATA.TS)-min(DATA.TS)
    range_observed
  #c
    #removing NA's because we want to export the max and min values from the data.
      random <- na.omit(decomposedTimeSeries$random) 
      max(random)-min(random)
  
  #d
    range_seasonal <- max(decomposedTimeSeries$seasonal)-min(decomposedTimeSeries$seasonal)
    range_seasonal
    range_observed-range_seasonal # the answer is positive so the range is getting smaller
  
  #e
    noise<-decomposedTimeSeries$random
    noise1<-c(noise)
    qqnorm(noise1); qqline(noise1, col = 2)
    
  
                 