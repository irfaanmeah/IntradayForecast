# IntradayForecast

This is a Demo App showing an intraday forecast at a 15 mins time interval. It also considers backlog to determine expected End of Day (EOD) volumes and EOD Backlog. 
Added to this app is the ability to Simulate backlogs based on the No of associates, AHT (Average Handling time) in mins and Overtime hours.

Link - https://irfaanmeah11.shinyapps.io/App2/?_ga=2.263373004.1383046900.1609545645-2059591034.1609545645

## *Input*

* Incoming volumes at an interval level (15 mins)
* Holiday List to manipulate forecast (Increase or Decrease based on past performance)
* Forecast Parameters (Excel file to specify parameters that configure the model)

## *Model*

As and when new data is ingested, a multi-seasonal time series data is passed to an ARIMA model with fourier series as regressors.

## *Output*

### Plot
* The plot displays the interval at the X-axis and Volumes on the Y-axis
* The forecast to the start of day is shown. This is called as the Base forecast
* Current forecast is the forecast for the rest of the day based on the incoming volumes
* Actuals is the data being fed to the system through the tool

### Table
* Shows the Date, Interval, Actual incoming volume and Actual Backlog 

### SidePanel on the right
* Shows the summary of Incoming volumes, Actuals and Backlog
