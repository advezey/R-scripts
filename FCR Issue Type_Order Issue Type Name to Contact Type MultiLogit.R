library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod); library(foreign);library(nnet);
library(kSamples)

connSQL <- odbcConnect('SQLserver')
connTOAD <- odbcConnect('Sunsysh', uid ='CCFLAXV',  pwd = 'Gandalf8')

fcr <- sqlQuery(connSQL, "
                  select a.*, b.OrderIssueID,b.OrderIssuePartVItem, b.OrderIssueFollowup, b.OrderIssueDate, b.CSARNetworkID
		, e.OrderIssueTypeSort_SAUS as IssueType,c.OrderIssueContactID,c.RootCauseID, c.UserLANID as lanid
                , c.ContactDate, d.*, e.OrderIssueTypeName , f.ContactTypeName
                from FCR.dbo.FCR_Order a
                inner join FCR.dbo.FCR_OrderIssue b on a.OrderID=b.OrderID 
                inner join FCR.dbo.FCR_OrderIssueContact c on b.OrderIssueID=c.OrderIssueID 
                left join FCR.dbo.FCR_OrderIssueTypeRootCause d on c.RootCauseID=d.OrderIssueRootCauseID 
                left join FCR.dbo.FCR_OrderIssueType e on b.OrderIssueTypeID=e.OrderIssueTypeID 
                left join FCR.dbo.FCR_ContactType F on f.ContactTypeID=c.ContactTypeID
                where b.CSARNetworkID = 2
                AND c.ContactDate >= '2015-10-10'"
                , stringsAsFactors=F)
        fcr <- filter(fcr, fcr$IssueType != 'BOPIS Issue')
        fcr <- filter(fcr, fcr$IssueType != 'Gift Card Inquiry')

set.seed(77)
trainingdata <- fcr[sample(1:nrow(fcr),10000,replace = FALSE),c(9,18,19)]
trainingdata$IssueType <- as.factor(trainingdata$IssueType)
trainingdata$OrderIssueTypeName <- as.factor(trainingdata$OrderIssueTypeName)
trainingdata$ContactTypeName <- as.factor(trainingdata$ContactTypeName)
trainingdata$ContactTypeName2 <- relevel(trainingdata$ContactTypeName, ref = 'Phone')

tdmodel <- multinom(ContactTypeName2 ~ IssueType, data = trainingdata)
z <- summary(tdmodel)$coefficients/summary(tdmodel)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

summary(tdmodel)

probtest <- data.frame(IssueType = unique(trainingdata$IssueType))
probtest[,2:6]<-predict(tdmodel, newdata=probtest,"probs")
        colnames(probtest) <- c('IssueType','Phone','Chat','Email'
                                ,'Fax', 'Unknown')
        probtest[,2:6] <- format(probtest[,2:6], scientific=FALSE)

IssueTypeFreq <- table(trainingdata$IssueType)
IssueTypeProb <- aggregate.data.frame(fcr$IssueType
                                      , by = list(fcr$IssueType)
                                      , FUN = length)    
        IssueTypeProb$Percent_Of_Total <- IssueTypeProb$x/sum(IssueTypeProb$x)
      
chisq.test(IssueTypeFreq,p=IssueTypeProb$Percent_Of_Total)
