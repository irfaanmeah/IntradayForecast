rm(list=ls())
library(plotly)
library(DT)
library(shinythemes)
library(data.table)
library(forecast)
library(lubridate)
library(dplyr)
library(dummies)
library(shinyalert)
library(waiter)

####--------------------------------------------Forecast Setup-------------------------------##
path1="C:/Users/md.irfaan.meah/OneDrive - Accenture/Interval_forecast/Intraday_app/Intraday_app_shared v2/"

setwd(path1)

source(paste0(path1,"Code/interval_functions.R"))
source(paste0(path1,"Code/interval_forecasting.R"))
####-------------------------------------------End Forecast Set up----------------------------##

#Choices for shiny

max_date <- as.Date(max(final_df$Received.Date),"")
print(max_date)
#print(unique(input_df$Received.Date))

input_df_all_choices <- unique(input_df$interval)
input_df_choice <- input_df[input_df$Received.Date==max_date,"interval"]

if(length(input_df_choice)!=freq){
    input_df_choice <- input_df_all_choices[!input_df_all_choices%in%input_df_choice]
}


#final_df <- readRDS(file = paste0(path1,"Input/final_df.rds"))

actuals_today <- sum(final_df$Actual,na.rm = T)
base_forecast_today <- sum(final_df$base_forecast,na.rm = T)
expec_eod_vol <- sum(final_df$Actual,na.rm = T)+sum(final_df$forecast,na.rm = T)

last_act_backlog <- final_df$backlog[!is.na(final_df$backlog)]
last_act_backlog <- last_act_backlog[length(last_act_backlog)]

remaining_forecast <- ifelse(sum(final_df$forecast,na.rm = T)==0,sum(final_df$base_forecast),
                             sum(final_df$forecast,na.rm = T))
ot_hrs=ot_hrs
aht=aht
fte=fte

df <- final_df

remain_time <- nrow(final_df[is.na(final_df$Actual),])*interval

total_volume <- sum(c(last_act_backlog,remaining_forecast))

time_avl <- remain_time+(ot_hrs*60)

exp_backlog <- round(total_volume-(time_avl/aht)*fte,digits = 0)

exp_backlog <-  ifelse(exp_backlog<0,0,exp_backlog)

###----------------------------------------------------Build the app

shinyApp(
  ui = fluidPage(
    useShinyalert(),
    use_waiter(),
    h1(id="title","Intraday Forecasting", align = "center",font="bold"),
    #titlePanel(h1(id="title","Intraday Forecasting",align = "center")),
    tags$head(tags$style(
      HTML('#title {
           color: white;
           font-family:"verdana black";
           background-color:#4040a1;
           font-size: 40px;
           font-style: bold;}'))),
    #theme = shinytheme("spacelab"),
    #shinythemes::themeSelector(),
    column(12,
           #mainPanel(
           #tabsetPanel(
           #tabPanel("Forecast Parameters"),
           
           #tabPanel("Intraday Forecast",
           
           fluidRow(column(2,
                           h3("Update Interval Volume"),
                           dateInput("date", label = h3("Date input"), value = max_date),
                           selectInput("interval_choices", label = h5(paste0("Interval - ",interval," min")), 
                                       choices = input_df_choice),
                           
                           numericInput("act_inc_vol", label = h5("Actual Incoming Volume"), value = "",min = 0),
                           
                           numericInput("act_backlog", label = h5("Actual Backlog"), value = 0,min=0),
                           
                           actionButton("save", label = "Save & Run",icon = icon("fas fa-location-arrow")),
                           tags$head(
                             tags$style(HTML('#save{background-color:#4040a1;
                                             color:white}'),
                                        HTML('#save_cp{background-color:#4040a1;
                                             color:white}'))
                           ),
                           
           ),
           br(),
           br(),
           br(),
           column(10,
                  plotlyOutput("plot1")),
           ),
           
           br(),
           fluidRow(column(2,
                           h3("Capacity Parameters"),
                           numericInput("aht", label = h5("AHT"), value = 5,min=0),
                           numericInput("fte", label = h5("# of Associates"), value = 10,min=0),
                           numericInput("ot_hrs", label = h5("Overtime Hours"), value = 0,min=0),
                           actionButton("save_cp", label = "Backlog Simulation",icon = icon("fas fa-location-arrow")),
           ),
           column(7,
                  DT::dataTableOutput("table1"),style = "height:350px;color:black;"),
           br(),
           br(),
           br(),
           column(3,
                  verbatimTextOutput("forecast_info"),
                  tags$head(tags$style("#forecast_info{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 background-color:#92a8d1;
                                 }"
                  )
                  ),
                  br(),
                  br(),
                  verbatimTextOutput("cap_info"),
                  tags$head(tags$style("#cap_info{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 background-color:#92a8d1;
                                 }"
                  )
                  ))),#overflow-y: scroll;
           
           
    ),
    
  ),
  
  server = function(input, output,session) {
    
    values <- reactiveValues(input_df=input_df,final_df=final_df,actuals_today=actuals_today,
                             base_forecast_today=base_forecast_today,expec_eod_vol=expec_eod_vol,
                             last_act_backlog=last_act_backlog,remaining_forecast=remaining_forecast,
                             exp_backlog=exp_backlog)
    
    output$inc_interval <- renderPrint({ input$interval_choices })
    
    output$forecast_info <- renderText({
      
      day_for <- paste0("Day Start Forecast: ",values$base_forecast_today)
      act <- paste0("Actuals so far: ",values$actuals_today)
      exp_eod <- paste0("Expected EOD Volume: ",values$expec_eod_vol)
      
      paste(day_for,
            act,
            exp_eod,sep="\n")
      
    })
    
    output$cap_info <- renderText({
      
      paste0("Expected EOD Backlog: ",values$exp_backlog)
      
    })
    
    output$table1 <- renderDataTable({
      
      df <- values$final_df
      df <- df[,c("Received.Date","interval","Actual","backlog")]
      
      setnames(df,c("Received.Date","interval","Actual","backlog"),c("Received Date","Interval","Actual Incoming Volume","Actual Backlog"))
      
      datatable(df,options = list(paging = FALSE,dom='t',scroller = TRUE,
                                  deferRender = TRUE,
                                  scrollY = 330,columnDefs=list(list(targets=1:ncol(df),class="dt-center",className= 'dt-body-center'))),
                fillContainer = T,filter="none",class = 'cell-border stripe')
      
    })
    
    output$plot1 <- renderPlotly({
      
      f <- list(
        family = "Droid Serif",
        size = 18,
        color = "black"
      )
      x <- list(
        title = "Interval",
        titlefont = f,
        tickangle=315,
        zeroline=FALSE,
        showline = FALSE)
      
      y <- list(
        title = "Volume",
        titlefont = f,
        zeroline=FALSE)
      
      # ay <- list(
      #     tickfont = list(color = "red"),
      #     overlaying = "y",
      #     side = "right",
      #     title = "Backlog",range=c(0,0.5*(max(final_df$base_forecast))),
      #     titlefont = f,
      #     showgrid = FALSE
      # )
      plot_ly(values$final_df, x = ~interval, y = ~Actual, name = 'Actuals', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~base_forecast, name = 'Day Start Forecast', mode = 'lines') %>%
        add_trace(y = ~forecast, name = 'Current forecast', mode = 'lines') %>%
        #add_trace(x=~interval,y=~backlog,name="Backlog",mode='lines',type='scatter',fill = 'tozeroy',yaxis='y2')%>%
        layout(xaxis = x, yaxis = y,legend=list(x = 0.9, y = 0.9))#,yaxis2 = ay
      
    })
    
    w=Waiter$new(color = transparent(.5),
                 html = spin_timer())
    
    observeEvent(input$save,{
      
      
      w$show()
      # show_waiter(
      #               
      #     spin_fading_circles()
      # )
      
      temp <- data.frame("Received.Date"=as.POSIXct(input$date,format="%Y-%m-%d",tz=""),
                         "interval"=input$interval_choices,
                         "Actual"=input$act_inc_vol,
                         "backlog"=input$act_backlog,check.names = FALSE)
      #print(temp)
      # temp <- data.frame("Received.Date"=as.POSIXct("2018-01-31",format="%Y-%m-%d",tz=""),"interval"="19:15",
      #                    "Actual"=16,
      #                    "backlog"=10,check.names = FALSE)

      input_df <-bind_rows(values$input_df,temp)
      
      input_df$Received.Time <- as.POSIXct(paste0(date(input_df$Received.Date)," ",input_df$interval))
      
      input_df$Volume <- input_df$Actual
      
      values$input_df <- input_df
      
      
      ##-------------------------------------Forecast Horizon---------------------------------##
      
      index_train <- nrow(input_df)-train_points
      
      train <- input_df[(index_train+1):nrow(input_df),]
      
      ##----------------------------------------Outlier Treatment
      if(treat_data==TRUE){
        train <- outlier(train)
      }
      
      train%>%
        arrange(Received.Time)->train
      
      train$Volume <- round(train$Volume)
      
      #--------------------------------#Building a horizon dataframe-----------------##
      
      horizon_df <- data.frame("Received.Time"=seq.POSIXt(from=train$Received.Time[nrow(train)],by=interval*60,length.out=1000))
      
      horizon_df$Received.Date <- as.POSIXct(date(horizon_df$Received.Time))
      horizon_df$hour <- hour(horizon_df$Received.Time)
      horizon_df$min <- minute(horizon_df$Received.Time)
      horizon_df$interval <- paste0(horizon_df$hour,":",horizon_df$min)
      horizon_df$Weekday <- weekdays(horizon_df$Received.Time)
      
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
      horizon_df$Received.Date <- as.Date(horizon_df$Received.Date,"")
      
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
        saveRDS(horizon_df_base,file=paste0(path1,"Input/horizon_base.rds"))
        
      }else{
        horizon_df_base <- readRDS(file = paste0(path1,"Input/horizon_base.rds"))
        horizon_df_base <- left_join(horizon_df_base,horizon_df[,c("Received.Time","forecast")],
                                     by=c("Received.Time"))
      }
      
      
      final_df <- horizon_df_base
      
      final_df <- left_join(final_df,input_df[,c("Received.Time","Actual","backlog")],
                            by="Received.Time")  
      
      #Print statements
      
      actuals_today <- sum(final_df$Actual,na.rm = T)
      base_forecast_today <- sum(final_df$base_forecast,na.rm = T)
      expec_eod_vol <- sum(final_df$Actual,na.rm = T)+sum(final_df$forecast,na.rm = T)
      remaining_forecast <- sum(final_df$forecast,na.rm = T)
      
      last_act_backlog <- final_df$backlog[!is.na(final_df$backlog)]
      last_act_backlog <- last_act_backlog[length(last_act_backlog)]
      
      print(actuals_today)
      print(base_forecast_today)
      print(expec_eod_vol)
      print(last_act_backlog)
      
      final_df$interval <- as.character(final_df$interval)
      
      #Changing format of intervals for better visual
      
      final_df$interval <- sapply(final_df$interval,FUN = function(x){
        
        ifelse(nchar(substr(x,gregexpr(":",x)[[1]][1]+1,
                            nchar(x)))<2,paste0(x,"0"),x)
        
      })
      
      final_df$interval <- sapply(final_df$interval,FUN = function(x){
        
        ifelse(nchar(substr(x,1,gregexpr(":",x)[[1]][1]-1))<2,
               paste0("0",x),x)
        
      })
      
      final_df$interval <- factor(final_df$interval,levels = unique(final_df$interval))
      
      ##-----------------Calc backlog
      
      df <- final_df
      
      remain_time <- nrow(final_df[is.na(final_df$Actual),])*interval
      
      total_volume <- sum(c(last_act_backlog,remaining_forecast),na.rm = T)
      
      time_avl <- remain_time+(ifelse(is.na(input$ot_hrs),0,input$ot_hrs)*60)
      
      exp_backlog <- round(total_volume-(time_avl/input$aht)*input$fte,0)
      
      values$exp_backlog <- ifelse(exp_backlog<0,0,exp_backlog)
      
      
      ##-------------------
      values$actuals_today <- actuals_today
      values$base_forecast_today <- base_forecast_today
      values$expec_eod_vol <- expec_eod_vol
      values$remaining_forecast <- remaining_forecast
      values$last_act_backlog <- last_act_backlog
      
      values$final_df <- final_df
      
      input1 <- input_df
      input1$Received.Date <- as.Date(input1$Received.Date,tz="")
      #print(input1[nrow(input1),])
      max_date <- as.Date(max(final_df$Received.Date),"")
      
      input_df_all_choices <- unique(input1$interval)
      input_df_choice <- input1[input1$Received.Date==max_date,"interval"]
      
      if(length(input_df_choice)!=freq){
        input_df_choice <- input_df_all_choices[!input_df_all_choices%in%input_df_choice]
      }
      #print(input_df_choice)
      #Changing values
      updateSelectInput(session, "interval_choices", 
                        choices = input_df_choice)
      
      updateNumericInput(session,"act_inc_vol",value = "",min = 0)
      updateNumericInput(session,"act_backlog", value = 0,min=0)
      updateDateInput(session,"date", value = max_date)
      #waiter_hide() 
      w$hide()
      shinyalert(title = "Forecast Complete !", type = "success")
    })
    
    observeEvent(input$save_cp,{
      
      df <- values$final_df
      
      remain_time <- nrow(df[is.na(df$Actual),])*interval
      
      remaining_forecast <- ifelse(sum(df$forecast,na.rm = T)==0,sum(df$base_forecast),
                                   sum(df$forecast,na.rm = T))
      
      total_volume <- sum(c(values$last_act_backlog,remaining_forecast),na.rm = T)
      
      time_avl <- remain_time+(ifelse(is.na(input$ot_hrs),0,input$ot_hrs)*60)
      
      exp_backlog <- round(total_volume-(time_avl/input$aht)*input$fte,0)
      
      values$exp_backlog <- ifelse(exp_backlog<0,0,exp_backlog)
      
      shinyalert(title = "CP Simulation Complete !", type = "success")
    })
    
  }
  
  
)