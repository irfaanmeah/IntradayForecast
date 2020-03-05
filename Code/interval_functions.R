outlier <- function(input1){
if (outlier_method=="IQR") {
  
  input1%>%
    group_by(Weekday,interval)%>%
    summarise(Median=median(Volume))->x
  
  input1%>%
    group_by(interval)%>%
    summarise(upper_quar=quantile(Volume,0.75),
              lower_quar=quantile(Volume,0.25),
              IQR=IQR(Volume)*1.5)->y
  
  x <- inner_join(x[,c("Weekday","interval","Median")],y,by=c("interval"))
  
  x%>%
    mutate(upper_thres=round(upper_quar)+IQR,
           lower_thres=round(lower_quar)-IQR)->threshold
  
  threshold$lower_thres[threshold$lower_thres<0] <- 0
  
  threshold <- threshold[,c('Weekday','interval','Median','upper_thres','lower_thres')]
  rm(x)
  
  input_bind <- input1[FALSE,]
  
  #input1$Actual <- input1$volume
  
  for (i in 1:nrow(threshold)){
    
    hour_min <- threshold[i,]
    
    input_grp <- input1[input1$Weekday==hour_min$Weekday&
                          input1$interval==hour_min$interval,]
    
    #input_grp$Interaction_Volume_Act <- ifelse(input_grp$Interaction_Volume_Act>hour_min$upper_thres[1]|input_grp$Interaction_Volume_Act<hour_min$lower_thres[1],
    #                                          hour_min$Median[1],input_grp$Interaction_Volume_Act)
    input_grp$Volume <- ifelse(input_grp$Volume<=hour_min$lower_thres[1]|input_grp$Volume>=hour_min$upper_thres[1],
                               hour_min$Median[1],input_grp$Volume)
    
    input_bind <- rbind(input_bind,input_grp)
    
  }
}else if (outlier_method=="percentile"){
  input1%>%
    group_by(Weekday,interval)%>%
    summarise(Median=median(Volume))->x
  
  input1%>%
    group_by(interval)%>%
    summarise(upper_quar=quantile(Volume,Upper_perc),
              lower_quar=quantile(Volume,Lower_perc))->y
  
  x <- inner_join(x[,c("Weekday","interval","Median")],y,by=c("interval"))
  
  x%>%
    mutate(upper_thres=round(upper_quar),
           lower_thres=round(lower_quar))->threshold
  
  threshold <- threshold[,c('Weekday','interval','Median','upper_thres','lower_thres')]
  rm(x)
  
  input_bind <- input1[FALSE,]
  
  for (i in 1:nrow(threshold)){
    
    hour_min <- threshold[i,]
    
    input_grp <- input1[input1$Weekday==hour_min$Weekday
                        &input1$interval==hour_min$interval,]
    
    input_grp$Volume <- ifelse(input_grp$Volume<=hour_min$lower_thres[1]|input_grp$Volume>=hour_min$upper_thres[1],
                               hour_min$Median[1],input_grp$Volume)
    
    input_bind <- rbind(input_bind,input_grp)
    
  }
  
}
  return(input_bind)
}

accuracy_func <- function(forecast,actual){
  
  MAPE <- ifelse(forecast==actual,0,ifelse(actual==0,abs(1-forecast)/1,
                                           abs(actual-forecast)/actual))
  MAPE <- mean(MAPE*100,na.rm = T)
  MAPE <- matrix(MAPE,1,1,dimnames = list(c(""),c("MAPE")))
  
  return(MAPE)
}
