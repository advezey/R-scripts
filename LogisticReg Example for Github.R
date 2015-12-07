library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod)

# Example: A researcher is interested in how variables, such as 
# GRE (Graduate Record Exam scores), GPA (grade point average) and 
# prestige of the undergraduate institution, effect admission into graduate school. 
# The response variable, admit/don't admit, is a binary variable.
# Source: IDRE (Institute for Digital Research and Education)

mydata <- read.csv('http://www.ats.ucla.edu/stat/data/binary.csv', header = TRUE
                   , stringsAsFactors = FALSE)

mydata$rank <- factor(mydata$rank) 
#           Convert rank to a factor to indicate that 
#           rank should be treated as a categorical variable.

mylogit <- glm(admit~ gre+gpa+rank, data = mydata, family = "binomial") 
#           binomial distribution gives the discrete probability distribution
summary(mylogit) 
#           For every one unit change in gre, the log odds of admission (versus non-admission)
#                 increases by 0.002.
#           For a one unit increase in gpa, the log odds of being admitted to 
#                 graduate school increases by 0.804.
#           The indicator variables for rank have a slightly different interpretation.
#                 For example, having attended an undergraduate institution with rank of 2, 
#                 versus an institution with a rank of 1, changes the log odds of 
#                 admission by -0.675.
#           Below the table of coefficients are fit indices, 
#                 including the null and deviance residuals and the AIC (see below). 

with(mylogit, null.deviance-deviance) # Find the difference in deviance 
# for the two models (i.e., the test statistic)
with(mylogit, df.null - df.residual)  # Find the difference in degrees of freedom 
# between the two models
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual
                     , lower.tail = FALSE)) #find the p-value
#           The chi-square of 41.46 with 5 degrees of freedom and an associated p-value
#           of less than 0.001 tells us that our model as a whole fits significantly 
#           better than an empty model.
#          
confint(mylogit) # Confidence Intervals using profiled log-likelihood
confint.default(mylogit) # Confidence Intervals using standard errors
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#         We can test for an overall effect of rank using the wald.test function of the aod library.
#         The order in which the coefficients are given in the table of coefficients 
#         is the same as the order of the terms in the model. This is important because 
#         the  wald.test function refers to the coefficients by their order in the model. 
#         We use the wald.test function: b supplies the coefficients, while Sigma 
#         supplies the variance covariance matrix of the error terms, finally Terms 
#         tells R which terms in the model are to be tested, in this case, terms 
#         4, 5, and 6 are the three terms for the levels of rank.
#         The chi-squared test statistic of 20.9, with three degrees of freedom is 
#         associated with a p-value of 0.00011 indicating that the overall effect 
#         of  rank is statistically significant.

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
#         These objects must have the same names as the variables in your logistic 
#         regression above (e.g. in this example the mean for gre must be named gre). 
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")        
#         In the above output we see that the predicted probability of being 
#         accepted into a graduate program is 0.52 for students from the highest 
#         prestige undergraduate institutions (rank=1), and 0.18 for students 
#         from the lowest ranked institutions (rank=4), holding gre and 
#         gpa at their means.

# Test for Git