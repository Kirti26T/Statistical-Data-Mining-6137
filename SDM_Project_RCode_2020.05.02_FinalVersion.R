####---------------Create ds-------------------------###
########################################################
rm(list =ls())
df <- read.csv("LOS_2020_04_29_Cleaned.csv")
#colnames(mydf)=tolower(make.names(colnames(mydf)))
str(df)


#------------------Data Cleaning---------------------#
########################################################
which(! complete.cases(df))                                       # Checking for missing values
df <- na.omit(df)

df$ADMISSION_DELAY <- as.factor(df$ADMISSION_DELAY) 
df$EMERGENCY_ROOM_FLAG <- as.factor(df$EMERGENCY_ROOM_FLAG)      
df$GENDER <- relevel(df$GENDER,    "FEMALE")                          # Set baseline for factor variables
df$ADMT_TYPE <- relevel(df$ADMT_TYPE,      "EMERGENCY")                       # Set baseline for factor variables
df$SVRTY_OF_ILLNESS_DESC <- relevel(df$SVRTY_OF_ILLNESS_DESC, "MINOR") 

colnames(df)=tolower(make.names(colnames(df)))
#unique(df$ADMISSION_DELAY)
str(df)
attach(df)

#------------------Data Visualization------------------#
########################################################

# Distribution of dependent variable “los_hours”
hist(df$los_hours)                                     # Not normally distributed
hist(log(df$los_hours))                                # Much better

# hist of Y variable
hist(df$los_hours, breaks=20, prob=T, main="Histogram of los_hours")           # Not normally distributed
den <- density(df$los_hours)                    
lines(den, col="red")

hist(log(df$los_hours), breaks=20, prob=T, main="Histogram of log(los_hours)")  # Much better
den <- density(log(df$los_hours))                    
lines(den, col="red")

# Distribution of dependent and independent continuous variables 
plot(log(los_hours) ~ age, data=df)
plot(log(los_hours) ~ log(age), data=df)

plot(log(los_hours) ~ physician_timeseen, data=df)
plot(log(los_hours) ~ log(physician_timeseen), data=df)

plot(log(los_hours) ~ svrty_of_illness, data=df)
plot(log(los_hours) ~ log(svrty_of_illness), data=df)

library(ggplot2)
ggplot(df, aes(x=age, y=log(los_hours))) +
  geom_point(color= "steelblue") +                 # Fit polynomial plot
  geom_smooth(method="lm", formula = y ~ poly(x, 2), color="red")


# Distributions between Y and X Factor variables
ggplot(df, aes(log(los_hours), fill=admission_delay)) +            # much better
  geom_density(alpha = 0.6) +
  ggtitle("los_hours Distributions by admission_delay") +
  theme_gray()

ggplot(df, aes(log(los_hours), fill=gender)) +            # much better
  geom_density(alpha = 0.6) +
  ggtitle("los_hours Distributions by gender") +
  theme_gray()


ggplot(df, aes(log(los_hours), fill=admt_type)) +            # much better
  geom_density(alpha = 0.6) +
  ggtitle("los_hours Distributions by admt_type") +
  theme_gray()

ggplot(df, aes(log(los_hours), fill=diagnosis)) +            # much better
  geom_density(alpha = 0.6) +
  ggtitle("los_hours Distributions by diagnosis") +
  theme_gray()

#-------------------------Models-----------------------#
########################################################

#MLE Regression Models
mle <- glm(log(los_hours)   ~ gender + age + svrty_of_illness + admt_type  + diagnosis + admission_delay + physician_timeseen, data=df, family=gaussian)

#poisson models
poisson = glm(los_hours     ~ gender + age + svrty_of_illness + admt_type  + diagnosis + admission_delay + physician_timeseen, data=df, family=poisson (link=log))
summary(poisson)
#There is some dispersion in the data (deviance > df), Residual deviance: 1917100  on 29475  degrees of freedom
#hence, we can try quasi-poisson and negative binomial regression

#quasipoisson timeseen_to_admit
qpoisson = glm(los_hours    ~ gender + age + svrty_of_illness + admt_type  + diagnosis + admission_delay + physician_timeseen, data=df, family=quasipoisson (link=log))
summary(qpoisson)
#' Given that the dispersion parameter (lambda) is 104.3475 and quite far from 1, negative binomial regression may be more appropriate.

#negative binomial models : best model 
library(MASS)
nb  <- glm.nb(los_hours ~ gender + age + svrty_of_illness + admt_type  + diagnosis + admission_delay + physician_timeseen, data=df)
summary(nb)

#compare models
#poisson and qpoisson have the same results 

library(stargazer)
stargazer(poisson, qpoisson, nb, type="text", title = "comapre poisson, quasipoisson, negative binomial models ")
#Negative binomial appears to be the best model for los_hours, given dispersion in data.

##########Test models#############

#Poisson models are robust to violations in normality of residuals, homoscedasticity, and linearity. 
#However, we must still test for multicollinearity between predictors and independence(autocorrelation).

#' VIF test for multicollinearity
library("car")
vif(nb)

#admt_type & emergency_room_flag have VIF > 5. Hence, we must exlcude emergency_room_flag to avoid multicollinearity in our model.
#After remove emergency_room_flag predictor, None of the predictors have VIF > 5. Hence, we conclude that there is no multicollinearity in our model.

#' Durbin-Watson test for independence
#' DW value range: [0, 4], values between 1.5 and 2.5 suggest no autocorrelation
#' H0: Autocorrelation = 0. Reject H0 if p<0.05
library(lmtest)
dwtest(nb)

#Since DW = 1.9884, p-value = 0.1604, no autocorrelation
#=> pass all the test assumption



