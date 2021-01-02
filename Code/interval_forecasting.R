library(forecast)
library(lubridate)
library(dplyr)
library(dummies)

##--------------------------------------------Reading input files
path=getwd()

if (file.exists(paste0(path,"/Input/horizon_base.rds")))
  file.remove(paste0(path,"/Input/horizon_base.rds"))

input <- read.csv(paste0(path,"/Input/incoming_volumes.csv"),stringsAsFactors = F)

holiday_list <- read.csv(paste0(path,"/Input/Holiday_list.csv"),stringsAsFactors = F)
holiday_list$date <- as.POSIXct(holiday_list$date,format="%m/%d/%Y")

forecast_param <- read.csv(paste0(path,"/Input/Forecast_parameters.csv"),stringsAsFactors = F)
forecast_param <- forecast_param[forecast_param$Run=="Yes",]
##---------------------------------------------Parameters---------------##
i=1
start = forecast_param$Start_hour[i]
end = forecast_param$End_hour[i]
interval=forecast_param$interval[i]
freq=60/forecast_param$interval[i]*(end-start)
include_weekends=forecast_param$Include_Weekends[i]
outlier_method=forecast_param$outlier_method[i]
Upper_perc=forecast_param$Upper_perc[i]
Lower_perc=forecast_param$Lower_perc[i]
hol_per_alter=forecast_param$holiday_per_alter[i]
treat_data=forecast_param$outlier_correction_req[i]
ot_hrs=forecast_param$ot_hrs[i]
aht=forecast_param$aht[i]
fte=forecast_param$fte[i]
  
days <- ifelse(include_weekends==FALSE,5,7)
test_points=forecast_param$no_of_test_days[i]*freq
horizon_points=forecast_param$no_of_horizon_days[i]*freq

train_points <- freq*days*4
##-------------------------------------------Data Transformation----
input <- input[,c("Received.Date","Received.Time","Volume","backlog")]
input$Received.Time <- as.POSIXct(input$Received.Time,format="%I:%M %p")
input$Received.Date <- as.POSIXct(input$Received.Date,format="%m/%d/%Y")

input$hour <- hour(input$Received.Time)
input$min <- minute(input$Received.Time)
input <- input[input$hour>=start&input$hour<end,]

#input <- input[input$Received.Date>=since,]
input$Received.Time <- as.POSIXct(paste0(input$Received.Date," ",input$hour,":",input$min))

#Temp filter
#input <- input[input$Received.Time<"2018-01-31 19:15:00",]
##


input$interval <- paste0(input$hour,":",input$min)
input$interval <- factor(input$interval,levels=unique(input$interval))

input$Weekday <- weekdays(input$Received.Date)

if(include_weekends==FALSE){
input <- input[!input$Weekday%in%c("Sunday","Saturday"),]
}

input$Volume[is.na(input$Volume)] <- 0

input <- data.frame(input)

input <- cbind(input,dummy(x = "Weekday",data = input,sep = "_"))
input <- input[,1:(ncol(input)-1)]

input <- left_join(input,holiday_list[,c("date","Holiday_ind")],
                   by=c("Received.Date"="date"))

input$Holiday_ind[is.na(input$Holiday_ind)] <- 0

input$Actual <- input$Volume

input%>%
  arrange(Received.Time)->input

input$forecast <- NA

input$interval <- as.character(input$interval)

#Changing format of intervals for better visual
input$interval <- sapply(input$interval,FUN = function(x){
  
  ifelse(nchar(substr(x,gregexpr(":",x)[[1]][1]+1,
                      nchar(x)))<2,paste0(x,"0"),x)
  
})

input$interval <- sapply(input$interval,FUN = function(x){
  
  ifelse(nchar(substr(x,1,gregexpr(":",x)[[1]][1]-1))<2,
         paste0("0",x),x)
  
})

##-------------------------------------------Defining Train and test sets--------------------##
if(test_points>0){
  
test_final <- data.frame()

#input$Volume <- edit(input$Volume)

for (counter in 1:test_points){

index_train <- nrow(input)-train_points-test_points+counter
index_test <- nrow(input)-test_points+counter

train <- input[index_train:(index_test-1),]
test <- input[index_test:nrow(input),]
test <- test[1,]

if(treat_data==TRUE){
train <- outlier(train)
}

train%>%
  arrange(Received.Time)->train

train$Volume <- round(train$Volume)

ts_ <- ts(train$Volume,frequency=1)
msts_ <- msts(ts_,c(freq,freq*days))

fit <- auto.arima(ts_, seasonal=F, xreg=cbind(fourier(msts_, K=c(6,5))))

fc <- forecast(fit, xreg=cbind(fourier(msts_,K=c(6,5),h = nrow(test))), h=nrow(test))

test$forecast <- round(fc$mean)
test$forecast <- ifelse(test$forecast<0,0,test$forecast)
test$forecast <- as.numeric(test$forecast)
test$forecast <- ifelse(test$Holiday_ind==1,test$forecast*hol_per_alter,test$forecast)

accuracy_func(test$forecast,test$Actual)

test_final <- rbind(test_final,test)

print(paste0("Complete - ",round(nrow(test_final)/test_points*100,2),"%"))

}

test_final$abs_error <- abs(test_final$Actual-test_final$forecast)

acc <- 1-sum(test_final$abs_error)/sum(test_final$Actual)

acc <- ifelse(acc<0,0,acc)

print(paste0("Accuracy - ",acc))

write.csv(test_final,"test.csv",row.names = F)

}
##-------------------------------------Forecast Horizon---------------------------------##
if(horizon_points>0){
  
    index_train <- nrow(input)-train_points
    
    train <- input[(index_train+1):nrow(input),]
    
    ##----------------------------------------Outlier Treatment
    if(treat_data==TRUE){
      train <- outlier(train)
    }
    
    train%>%
      arrange(Received.Time)->train
    
    train$Volume <- round(train$Volume)
    
    #--------------------------------#Building a horizon dataframe-----------------##
    
    horizon_df <- data.frame("Received.Time"=seq.POSIXt(from=train$Received.Time[nrow(train)],by=interval*60,length.out=1000))
    
    #horizon_df$Received.Date <- as.POSIXct(date(horizon_df$Received.Time))
    horizon_df$Received.Date <- as.POSIXct(substr(horizon_df$Received.Time,1,10))
    horizon_df$hour <- hour(horizon_df$Received.Time)
    horizon_df$min <- minute(horizon_df$Received.Time)
    horizon_df$interval <- paste0(horizon_df$hour,":",horizon_df$min)
    horizon_df$Weekday <- weekdays(horizon_df$Received.Time)
    
    horizon_df$interval <- as.character(horizon_df$interval)
    
    #Changing format of intervals for better visual
    
    horizon_df$interval <- sapply(horizon_df$interval,FUN = function(x){
      
      ifelse(nchar(substr(x,gregexpr(":",x)[[1]][1]+1,
                          nchar(x)))<2,paste0(x,"0"),x)
      
    })
    
    horizon_df$interval <- sapply(horizon_df$interval,FUN = function(x){
      
      ifelse(nchar(substr(x,1,gregexpr(":",x)[[1]][1]-1))<2,
             paste0("0",x),x)
      
    })
    
    #Removing weekends if parameter is FALSE
    if(include_weekends==FALSE){
      horizon_df <- horizon_df[!horizon_df$Weekday%in%c("Sunday","Saturday"),]
    }
    
    #Adding time components
    horizon_df <- cbind(horizon_df,dummy(x = "Weekday",data = horizon_df,sep = "_"))
    horizon_df <- horizon_df[,1:(ncol(horizon_df)-1)]
    
    #Adding holiday indicator
    horizon_df <- left_join(horizon_df,holiday_list[,c("date","Holiday_ind")],
                       by=c("Received.Date"="date"))
    
    horizon_df$Holiday_ind[is.na(horizon_df$Holiday_ind)] <- 0
    
    #Filtering data for the hour and min 
    horizon_df <- horizon_df[horizon_df$hour>=start&horizon_df$hour<end,]
    horizon_df$Received.Date <- as.Date(horizon_df$Received.Date)
    
    #removing the first row as it was a part of actuals
    horizon_df <- horizon_df[2:nrow(horizon_df),]
    no_of_dates <- unique(horizon_df$Received.Date)
    horizon_df <- horizon_df[horizon_df$Received.Date%in%no_of_dates[1],]
    
    ##------------------------------------------------------Training and Forecasting-----
    
    ts_ <- ts(train$Volume,frequency=1)
    msts_ <- msts(ts_,c(freq,freq*days))
    
    fit <- auto.arima(ts_, seasonal=F, xreg=cbind(fourier(msts_, K=c(6,5))))
    
    fc <- forecast(fit, xreg=cbind(fourier(msts_,K=c(6,5),h = nrow(horizon_df))), nrow(horizon_df))
    
    horizon_df$forecast <- round(fc$mean)
    horizon_df$forecast <- ifelse(horizon_df$forecast<0,0,horizon_df$forecast)
    horizon_df$forecast <- as.numeric(horizon_df$forecast)
    horizon_df$forecast <- ifelse(horizon_df$Holiday_ind==1,horizon_df$forecast*hol_per_alter,horizon_df$forecast)
    
    #Creating a horizon df to store base forecast, current forecast and ACtuals
    if(nrow(horizon_df)==freq){
      
      horizon_df_base <- horizon_df
      horizon_df_base$base_forecast <- horizon_df_base$forecast
      horizon_df_base$forecast <- NULL
      saveRDS(horizon_df_base,file=paste0(path,"/Input/horizon_base.rds"))
      
    }else{
      horizon_df_base <- readRDS(file = paste0(path,"/Input/horizon_base.rds"))
      horizon_df_base <- left_join(horizon_df_base,horizon_df[,c("Received.Time","forecast")],
                                   by=c("Received.Time"))
    }
    
    
  final_df <- horizon_df_base
  final_df <- left_join(final_df,input[,c("Received.Time","Actual","backlog")],
                        by="Received.Time")  
  
  #saveRDS(final_df,file=paste0(path,"/Input/final_df.rds"))
  
  #Print statements
  
  actuals_today <- sum(final_df$Actual,na.rm = T)
  base_forecast_today <- sum(final_df$base_forecast,na.rm = T)
  expec_eod_vol <- sum(final_df$Actual,na.rm = T)+sum(final_df$forecast,na.rm = T)

  print(actuals_today)
  print(base_forecast_today)
  print(expec_eod_vol)
  
  input_df <- input
  rm(input)
  
}

