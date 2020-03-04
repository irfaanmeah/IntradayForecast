path="C:\\Users\\md.irfaan.meah\\OneDrive - Accenture\\Interval_forecast\\"

source(paste0(path,"interval_functions.R"))
##--------------------------------------------Reading input files

input <- read.csv(paste0(path,"incoming_volumes.csv"),stringsAsFactors = F)

holiday_list <- read.csv(paste0(path,"Holiday_list.csv"),stringsAsFactors = F)
holiday_list$date <- as.POSIXct(holiday_list$date,format="%m/%d/%Y")

forecast_param <- read.csv(paste0(path,"Forecast_parameters.csv"),stringsAsFactors = F)
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

days <- ifelse(include_weekends==FALSE,5,7)
test_points=forecast_param$no_of_test_days[i]*freq
horizon_points=forecast_param$no_of_horizon_days[i]*freq

train_points <- freq*days*4
##-------------------------------------------Data Transformation----
input <- input[,c("Received.Date","Received.Time","Volume")]
input$Received.Time <- as.POSIXct(input$Received.Time,format="%I:%M %p")
input$Received.Date <- as.POSIXct(input$Received.Date,format="%m/%d/%Y")

input$hour <- hour(input$Received.Time)
input$min <- minute(input$Received.Time)
input <- input[input$hour>=start&input$hour<end,]

#input <- input[input$Received.Date>=since,]
input$Received.Time <- as.POSIXct(paste0(input$Received.Date," ",input$hour,":",input$min))

#Temp filter
input <- input[input$Received.Time<"2018-01-31 08:15:00",]
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

input$backlog <- sample(1:10,nrow(input),replace = T)

input_df <- input
rm(input)

input_df$interval <- as.character(input_df$interval)

for(i in 1:nrow(input_df)){
  
  input_df$interval[i] <- ifelse(nchar(substr(input_df$interval[i],gregexpr(":",input_df$interval[i])[[1]][1]+1,
                                              nchar(input_df$interval[i])))<2,paste0(input_df$interval[i],"0"),
                                 input_df$interval[i])
  
  input_df$interval[i] <- ifelse(nchar(substr(input_df$interval[i],1,gregexpr(":",input_df$interval[i])[[1]][1]-1))<2,
                                 paste0("0",input_df$interval[i]),
                                 input_df$interval[i])
  
}

input_df$Received.Date <- as.Date(input_df$Received.Date,tz = "")

