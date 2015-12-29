library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod); library(foreign);library(nnet);
library(kSamples);library(forecast)

connSQL <- odbcConnect('SQLServer')

RawVolumeData <- sqlQuery(connSQL,paste0("select businessunit, fiscal_period_name_long, a.calendar_date, skillcategory, skillgroup, skillname
	     , sum(anstime) as anstime, sum(abncalls) as abncalls, sum(acdcalls) as acdcalls
                                         , sum(acdtime) as acdtime, sum(holdtime) as holdtime, sum(handledtime) as handletime
                                         , sum(handledcalls) as handledcalls, sum(acwtime) as acwtime, sum(i_acwtime) as i_acwtime
                                         from falcon.dbo.tblAvayaIntradaySkills a
                                         join falcon.dbo.tlk_calendar b on a.calendar_date=b.calendar_date
                                         where businessunit = 'Staples Advantage'
                                         and a.calendar_date > getdate()-1123
                                         group by businessunit, fiscal_period_name_long, a.calendar_date, skillcategory, skillgroup, skillname
                                         order by a.calendar_date, skillcategory, skillgroup, skillname"))

AggVolumebyPeriod <- aggregate.data.frame(RawVolumeData$acdcalls
                                          ,list(RawVolumeData$fiscal_period_name_long)
                                          ,sum)
        AggVolumebyPeriod[1,2] <- 270000 #Replacing abnormal obs due to database launch
        AggVolumebyPeriod[nrow(AggVolumebyPeriod),2] <- 270000 #Replacing abnormal obs due to incomplete period
        colnames(AggVolumebyPeriod) <- c('Fiscal Period','ACDCalls')

TimeSeries <- ts(AggVolumebyPeriod$ACDCalls)

ARIMA <- auto.arima(TimeSeries, d=1)
        acf(residuals(ARIMA))
        Box.test(residuals(ARIMA),type = "Ljung-Box")
        summary(ARIMA)
        plot(residuals(ARIMA))

Prediction <- forecast.Arima(ARIMA,level = 80)
        plot(Prediction)
        summary(Prediction)

# auto.arima() only looks at 4 different types of ARIMA models that are not
# sufficient for forecasting purposes that are more predictable than random walks or 
# walks with drifts. Using the manual Arima() function
# allows for more useful ARIMA orders as it pertains to forecasting contact volume.
                
ARIMA2 <- Arima(TimeSeries,order = c(11,1,2),include.mean = FALSE
                ,include.drift = TRUE)
        acf(residuals(ARIMA2))
        Box.test(residuals(ARIMA2),type = "Ljung-Box")
        summary(ARIMA2)
        plot(residuals(ARIMA2))

Prediction2 <- forecast.Arima(ARIMA2, h = 12, level = 80)
        plot(Prediction2)
        summary(Prediction2)
        