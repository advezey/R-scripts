library(corrgram);library(doBy);library(stringr);library(reshape2);
library(dplyr);library(ggplot2);library(RODBC);library(readxl);
library(moments); library(aod); library(foreign);library(nnet)

# Example 3. Entering high school students make program choices among general program,
# vocational program and academic program. Their choice might be modeled using their
# writing score and their social economic status.


ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")

# The data set contains variables on 200 students. The outcome variable is prog,
# program type. The predictor variables are social economic status, ses, a three-level
# categorical variable and writing score, write, a continuous variable. 


# Before running our model, we then choose the level of our outcome that we wish 
# to use as our baseline and specify this in the relevel function. Then, we run 
# our model using multinom. The multinom package does not include p-value calculation
# for the regression coefficients, so we calculate p-values using Wald tests
# (here z-tests).Note: the multinom function does not require the data
# to be reshaped as the mlogit package does.

ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)
z <- summary(test)$coefficients/summary(test)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2

summary(test)

# The model summary output has a block of coefficients and a block of standard 
# errors. Each of these blocks has one row of values corresponding to a model 
# equation. Focusing on the block of coefficients, we can look at the first row 
# comparing prog = "general" to our baseline prog = "academic" and the second row 
# comparing prog = "vocation" to our baseline prog = "academic".

# A one-unit increase in the variable write is associated with the decrease in the log odds of being in general program vs. academic program in the amount of .058 (b_13).
# A one-unit increase in the variable write is associated with the decrease in the log odds of being in vocation program vs. academic program. in the amount of .1136 (b_23).
# The log odds of being in general program vs. in academic program will decrease by 1.163 if moving from ses="low" to ses="high"(b_12).
# The log odds of being in general program vs. in academic program will decrease by 0.533 if moving from ses="low"to ses="middle"(b_11), although this coefficient is not significant.
# The log odds of being in vocation program vs. in academic program will decrease by 0.983 if moving from ses="low" to ses="high"(b_22).
# The log odds of being in vocation program vs. in academic program will increase by 0.291 if moving from ses="low" to ses="middle"(b_21), although this coefficient is not signficant.

# You can also use predicted probabilities to help you understand the model.
# You can calculate predicted probabilities for each of our outcome levels using 
# the fitted function. We can start by generating the predicted probabilities for 
# the observations in our dataset and viewing the first few rows

head(pp <- fitted(test))

# Next, if we want to examine the changes in predicted probability associated with
# one of our two variables, we can create small datasets varying one variable while
# holding the other constant. We will first do this holding write at its mean and
# examining the predicted probabilities for each level of ses.

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

# Another way to understand the model using the predicted probabilities is to look
# at the averaged predicted probabilities for different values of the continuous 
# predictor variable write within each level of ses. 

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41)
                     , write = rep(c(30:70),3))
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs"
                                  , se = TRUE))
## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)

# Things to consider:
# 
# The Independence of Irrelevant Alternatives (IIA) assumption: Roughly, the IIA 
# assumption means that adding or deleting alternative outcome categories does not
# affect the odds among the remaining outcomes. There are alternative modeling 
# methods, such as alternative-specific multinomial probit model, or nested logit
# model to relax the IIA assumption.
# 
# Diagnostics and model fit: Unlike logistic regression where there are many 
# statistics for performing model diagnostics, it is not as straightforward to do 
# diagnostics with multinomial logistic regression models. For the purpose of 
# detecting outliers or influential data points, one can run separate logit models 
# and use the diagnostics tools on each model. 

