library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod); library(foreign);library(nnet);
library(kSamples); library(MASS);library(glmnet);library(forecast)

connSQL <- odbcConnect('SQLserver')

mydata <- sqlQuery(connSQL,paste0("SELECT convert(date,a.RowLoadDate,101) as CalendarDate
	                          , b.ForecastGroup
                                  , sum(a.ACDCalls) as ACDCalls
                                  FROM NADCS_DW.dbo.Fact_Phone as a
                                  join NADCS_DW.dbo.Dim_PhoneSkill as b 
                                  on (a.PhoneSkill_Id=b.PhoneSkill_Id)
                                  WHERE a.RowLoadDate > '02/03/2013'
                                  GROUP BY a.RowLoadDate,b.ForecastGroup
                                  ORDER BY a.RowLoadDate,b.ForecastGroup"))

aggdata <- aggregate.data.frame(mydata$ACDCalls
                                , list(mydata$CalendarDate,mydata$ForecastGroup)
                                , sum)

FGs <- unique(aggdata$Group.2)

Premier <- filter(aggdata,aggdata[,2]=="SA Premier Voice")
        Premier <- Premier[2:nrow(Premier),]
PremierTS <- ts(Premier$x, frequency = 365
                , start = c(2013,66)
                , end = c(2015,348))
        PremDeComp <- stl(PremierTS,s.window = "period")
        plot.ts(PremierTS)
PremierFC <- auto.arima(PremierTS)      
        plot(PremierFC) #github
        
        test <- c("for git")
        