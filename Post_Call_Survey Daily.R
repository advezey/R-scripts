library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod); library(foreign);library(nnet);

connSQL <- odbcConnect('SQLServer')

PC_Surv <- sqlQuery(connSQL,paste0("SELECT b.CalendarDate, b.FiscalPeriodNameLong,b.FiscalWeekofPeriod 
                                   ,c.NetworkName,c.StaplesLocationName
                                   , a.Question_Id
                                   , a.Option_ID,a.AnsweredQuestions
                                   , a.ResponseValue
                                   FROM NADCS_DW.dbo.Fact_PhoneSurvey as a
                                   left join NADCS_DW.dbo.Dim_Date as b on (a.Date_ID = b.Date_ID)
                                   left join NADCS_DW.dbo.Dim_Associate as c on (a.Associate_Id = c.Associate_Id)
                                   WHERE b.CalendarDate > getdate() - 30 AND c.NetworkName != 'NULL'"))