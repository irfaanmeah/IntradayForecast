# IntradayForecast

This is a Demo App showing an intraday forecast at a 15 min time period. It also considers backlog to determine Expected End of Day (EOD) volumes and EOD Backlog. 
Added to this app is the ability to Simulate backlogs based on the No of associates, AHT (Average Handling time) and Overtime hours.

Input

* Incoming volumes at an interval level (15 mins)
* Holiday List to manipulate forecast (More or less based on past performance)
* Forecast Parameters (Excel file to specify parameters that configure the model)

Model

As and when new data is ingested, a multi-seasonal time series data is passed to an ARIMA model with fourier series as regressors.

Output
    Plot
    * The plot displays the interval at the X axis and Volumes on the Y axis
    * The forecast to the start of day is shown
    * Current forecast is the forecast for the rest of the Day
    * Actuals is when new actual data is input to the tool
    
    Table
    * Shows the Date, Interval, Actual incoming volume and Actual Backlog 
    
    SidePanel on the right
    * Shows the summary of Incoming volumes, Actuals and Backlog
