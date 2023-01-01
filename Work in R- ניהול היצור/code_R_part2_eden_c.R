#uploading libraries
  library(forecast)
  library(ggplot2)
  library(stats)

#uploading data sets
  d_can<-read.csv(file="can_production.csv",stringsAsFactors= FALSE)
  d_nuts<-read.csv(file="nuts_sales.csv",stringsAsFactors= FALSE)
  d_shirts<-read.csv(file="shirts_sales.csv",stringsAsFactors= FALSE)

### CANS

# 1) Preparations
  # a) finding the frequency of the cans production
    findfrequency(d_can)
  # b) removing 'date' column from the data
    dcannew <- subset(x = d_can, drop = TRUE, select = 'Production') 
  # c) starting at January 1972
    d_can_12 <- ts(dcannew, start = c(1972, 1), frequency = 12)
  # d) De-composition
    plot(d_can_12, main = 'Monthly')
    decomposedTimeSeries <- decompose(d_can_12) 
    plot(decomposedTimeSeries)

# We will compare between 2 models - holt winters and linear regression due the decomposition (The data is seasonal and with trend)
    
# 2) Holt Winters model
    fit_cans <- HoltWinters(d_can_12)
    forc_cans <- forecast(fit_cans, h=25)
    plot(forc_cans)
    forc_cans$mean
    rmse_cans_holt <- round(accuracy(forc_cans),3)
    rmse_cans_holt[2] # RMSE for holt winter is 3.868

# 3) Regression model
    linear_regression_cans <-  tslm(d_can_12 ~ trend)
    forc_cans_reg <- forecast(linear_regression_cans, h=25)
    rmse_can_reg <- round(accuracy(forc_cans_reg),3)
    rmse_can_reg[2] #RMSE for linear regression is 15.59


# 4) Cans conclusion:
    rmse_cans_holt[2] < rmse_can_reg[2] # the answer is True 
    # The better forecast is the holt winter model because 
    # the the RMSE of this model is smaller than the RMSE of the linear regression model.
    
    #25 next values
    forc_cans$mean

### NUTS

# 1) Preparations
  # a) finding the frequency of the nuts production
    findfrequency(d_nuts)
  # b) removing 'date' column from the data
    dnutsnew <- subset(x = d_nuts, drop = TRUE, select = 'SalesNuts')
  # c) starting at January 2010
    d_nuts_12 <- ts(dnutsnew, start = c(2010, 1), frequency = 12)
  # d) De-composition
    plot(d_nuts_12, main = 'Monthly')
    decomposedTimeSeries_nuts <- decompose(d_nuts_12)
    plot(decomposedTimeSeries_nuts)
  
# We will compare between 2 models - holt winters and linear regression due the decomposition (The data is seasonal and with trend)

# 2) Holt Winters model
    fit_nuts <- HoltWinters(d_nuts_12)
    forc_nuts <- forecast(fit_nuts, h=25)
    plot(forc_nuts)
    forc_nuts$mean
    rmse_nuts_holt <- round(accuracy(forc_nuts),3)
    rmse_nuts_holt[2] # RMSE for holt winters model is 15.184

# 3) Regression model
    linear_regression_nuts <-  tslm(d_nuts_12 ~ trend)
    forc_nuts_reg <- forecast(linear_regression_nuts, h=25)
    rmse_nuts_reg <- round(accuracy(forc_nuts_reg),3)
    rmse_nuts_reg[2] #rmse for linear regression is 24.334

# 4) Nuts conclusion:
    rmse_nuts_holt[2] < rmse_nuts_reg[2] # the answer is True 
    # The better forecast is the holt winter model because 
    # the the RMSE of this model is smaller than the RMSE of the linear regression model.

    #25 next values
    forc_nuts$mean

### SHIRTS

# 1) Preparations
  # a) removing 'date' column from the data:
     dshirtssnew <- subset(x = d_shirts, drop = TRUE, select = 'SalesShirts')

  # b) starting at January 2010
    d_shirts_12 <- ts(dshirtssnew, start = c(2010, 1), frequency = 12)
    plot(d_shirts_12, main = 'Monthly')

  # c) first we will figure out where is our NA's in the data
    complete_rows <- complete.cases(dshirtssnew)
    incomplete_rows <- which(!complete_rows)
    incomplete_rows # we got NA in rows :33,45,53,55,57

  # d) creating 2 new data sets (we will make a copy of the original data to avoid shallow copy) (including the missing values that we will fill them by two separate methods - linear regression model and holt winters model)
    dshirtss_winters <- data.frame(dshirtssnew)
    dshirtss_regression <- data.frame(dshirtssnew)

# 2) Holt Winters model

  # a) filling the missing values with holt winters model

    #handling the missing values in row 33:
      data_32 <- dshirtss_winters[1:32,] # (in the next row we will have a NA)
      d_shirts_33 <- ts(data_32, start = c(2010, 1), frequency = 12)
      try_winters_33 <- HoltWinters(d_shirts_33)
      forc_33 <- predict(try_winters_33,h=1) 
      forc_33
      dshirtss_winters[33,] <- forc_33[[1]] #inserting the value to row number 33
    #handling the missing values in row 45:
      data_44 <- dshirtss_winters[1:44,]
      d_shirts_45 <- ts(data_44, start = c(2010, 1), frequency = 12)
      try_winters_45 <- HoltWinters(d_shirts_45)
      forc_45 <- predict(try_winters_45,1)
      forc_45
      dshirtss_winters[45,] <- forc_45[[1]]
    #handling the missing values in row 53:
      data_52 <- dshirtss_winters[1:52,]
      d_shirts_53 <- ts(data_52, start = c(2010, 1), frequency = 12)
      try_winters_53 <- HoltWinters(d_shirts_53)
      forc_53 <- predict(try_winters_53,1) 
      forc_53
      dshirtss_winters[53,] <- forc_53[[1]]
    #handling the missing values in row 55:
      data_54 <- dshirtss_winters[1:54,]
      d_shirts_55 <- ts(data_54, start = c(2010, 1), frequency = 12)
      try_winters_55 <- HoltWinters(d_shirts_55)
      forc_55<- predict(try_winters_55,1) 
      forc_55
      dshirtss_winters[55,] <- forc_55[[1]]
    #handling the missing values in row 57:
      data_56 <- dshirtss_winters[1:56,]
      d_shirts_57 <- ts(data_56, start = c(2010, 1), frequency = 12)
      try_winters_57 <- HoltWinters(d_shirts_57)
      forc_57<- predict(try_winters_57,1)
      dshirtss_winters[57,] <- forc_57[[1]]
    #lets check if we have missing values:
      complete_rows_after_changes <- complete.cases(dshirtss_winters)
      incomplete_rows <- which(!complete_rows_after_changes)
      incomplete_rows # we dont have any NA's

  # b) De-composition
    dshirtss_winters_12 <- ts(dshirtss_winters, start = c(2010, 1), frequency = 12)
    plot(dshirtss_winters_12, main = 'Monthly')
    decomposedTimeSeries_shirts <- decompose(dshirtss_winters_12)
    plot(decomposedTimeSeries_shirts)

  # c) Holt Winters model prediction
    fit_shirts <- HoltWinters(dshirtss_winters_12)
    forc_shirts <- forecast(fit_shirts, h=25)
    forc_shirts
    plot(forc_shirts)
    forc_shirts$mean
    rmse_shirts_holt <- round(accuracy(forc_shirts),3)
    rmse_shirts_holt[2] # RMSE for holt winters is 1.462

# 3) Regression model

  # a) filling the missing values with linear regression model

    #handling the missing values in row 33:
      data_32_regression <- dshirtss_regression[1:32,] # (in the next row we will have a NA)
      dshirtss_regression_32 <- ts(data_32_regression, start = c(2010, 1), frequency = 12)
      dshirtss_regression_32 <- tslm(dshirtss_regression_32 ~ trend)
      forc_33_reg <- forecast(dshirtss_regression_32, h=1)
      dshirtss_regression[33,] <- forc_33_reg[[2]] #inserting the value to row number 33
    #handling the missing values in row 45:
      data_44_regression <- dshirtss_regression[1:44,]
      dshirtss_regression_44 <- ts(data_44_regression, start = c(2010, 1), frequency = 12)
      dshirtss_regression_44 <- tslm(dshirtss_regression_44 ~ trend)
      forc_45_reg <- forecast(dshirtss_regression_44, h=1)
      dshirtss_regression[45,] <- forc_45_reg[[2]] #inserting the value to row number 45
    #handling the missing values in row 53:
      data_52_regression <- dshirtss_regression[1:52,]
      dshirtss_regression_52 <- ts(data_52_regression, start = c(2010, 1), frequency = 12)
      dshirtss_regression_52 <- tslm(dshirtss_regression_52 ~ trend)
      forc_53_reg <- forecast(dshirtss_regression_52, h=1)
      dshirtss_regression[53,] <- forc_45_reg[[2]] #inserting the value to row number 53
    #handling the missing values in row 55:
      data_54_regression <- dshirtss_regression[1:54,]
      dshirtss_regression_54 <- ts(data_54_regression, start = c(2010, 1), frequency = 12)
      dshirtss_regression_54 <- tslm(dshirtss_regression_54 ~ trend)
      forc_55_reg <- forecast(dshirtss_regression_54, h=1)
      dshirtss_regression[55,] <- forc_55_reg[[2]] #inserting the value to row number 55
    #handling the missing values in row 57:
      data_56_regression <- dshirtss_regression[1:56,]
      dshirtss_regression_56 <- ts(data_56_regression, start = c(2010, 1), frequency = 12)
      dshirtss_regression_56 <- tslm(dshirtss_regression_56 ~ trend)
      forc_57_reg <- forecast(dshirtss_regression_56, h=1)
      dshirtss_regression[57,] <- forc_57_reg[[2]] #inserting the value to row number 57
    #lets check if we have missing values:
      complete_rows_after_changes <- complete.cases(dshirtss_regression)
      incomplete_rows <- which(!complete_rows_after_changes)
      incomplete_rows # we dont have any NA's

  # b) De-composition
    dshirtss_regression_12 <- ts(dshirtss_regression, start = c(2010, 1), frequency = 12)
    plot(dshirtss_regression_12, main = 'Monthly')
    decomposedTimeSeries_shirts_regression <- decompose(dshirtss_regression_12)
    plot(decomposedTimeSeries_shirts_regression)

  # c) linear regression prediction
    linear_regression_shirts <- tslm(d_shirts_12 ~ trend)
    forc_shirts_reg <- forecast(linear_regression_shirts, h=25)
    rmse_shirts_reg <- round(accuracy(forc_shirts_reg),3)
    rmse_shirts_reg[2] #RMSE for linear regression is 10.282

# 4) Shirts conclusion:
    rmse_shirts_holt[2] < rmse_shirts_reg[2] #the answer is True
    #The better forecast is the holt winter model because 
    #the the RMSE of this model is smaller than the RMSE of the linear regression model.

    #25 next values
    forc_shirts$mean
    

