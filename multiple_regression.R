#packages
require(tidyverse)
require(dplyr)
library(Hmisc)
library(ggpubr)
library(lmtest)
library(glmnet)

#Multiple Linear Regression
multi_data <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Baseball/strikeout_many_variables.csv")
multi_data$percent_strikeout <- 100*(multi_data$p_strikeout/multi_data$p_total_pa)
multi_data <- multi_data %>%
  select(c(percent_strikeout, ff_avg_speed, ff_avg_spin, ff_avg_break_x, ff_avg_break_z))

multi_model <- glm(percent_strikeout~ ff_avg_speed + ff_avg_spin + ff_avg_break_x +ff_avg_break_z, data=multi_data )
summary(multi_model)

#multivariate normality
plot(multi_model, 2) #looks ok
ks.test(x=multi_model$residuals, y = pnorm, alternative = "two.sided") #not normal
#homoscedacitiy
plot(multi_model, 1) #without the outlier looks fine
gqtest(formula = multi_model, fraction = .2) #p-value = 0.06367 variance fine
#no multicollinearity
mydata.rcorr = rcorr(as.matrix(multi_data))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

mydata.coeff
mydata.p #got some issues

#RIDGE REGESSION
#https://www.statology.org/ridge-regression-in-r/
#make the model
multi_complete_data <- multi_data[complete.cases(multi_data),]
y <- multi_complete_data$percent_strikeout
x <- data.matrix(multi_complete_data[,c('ff_avg_speed', 'ff_avg_spin', 'ff_avg_break_x', 'ff_avg_break_z')])
ridge_model <- glmnet(x, y, alpha = 0)
summary(ridge_model)

##perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #1.195782

#produce plot of test MSE by lambda value
plot(cv_model) 

#look at the best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#produce Ridge trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(model, xvar = "lambda")

#LASOO REGRESSION
