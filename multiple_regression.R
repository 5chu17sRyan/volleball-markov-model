#packages
require(tidyverse)
require(dplyr)
library(Hmisc)
library(ggpubr)
library(lmtest)
library(glmnet)
library(caret)
library(randomForest)
library(boot)
#MULTIPLE LINEAR REGRESSION
multi_data <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Baseball/strikeout_many_variables.csv")

multi_data$percent_strikeout <- 100*(multi_data$p_strikeout/multi_data$p_total_pa)
multi_data <- multi_data %>%
  select(c(percent_strikeout, ff_avg_speed, ff_avg_spin, ff_avg_break_x, ff_avg_break_z))
multi_data <- multi_data[complete.cases(multi_data),]
multi_model <- lm(percent_strikeout~ ff_avg_speed + ff_avg_spin + ff_avg_break_x +ff_avg_break_z, data=multi_data )

#looking at the summary of the normal 
summary(multi_model)
#Adjusted R-squared:  0.1877 
#Residual Standard Error: 6.304 on 442 degrees of freedom

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

#bootstrapping (this is professor hartlaub's code from STAT226)
f4<-function(data, i, formula) {
  d=data[i,]
  model<-lm(formula, data=d)
  coef(model)
}
bootlm<-boot(data=multi_data, f4, R=1000, formula=multi_model)
#Take a look at bootlm.
boot.ci(bootlm, type="all", index = 4) 
bootlm

#backwards regression (https://www.statology.org/stepwise-regression-r/)
#define intercept-only model
intercept_only <- lm(percent_strikeout ~ 1, data=multi_data)

#define model with all predictors
all <- lm(percent_strikeout ~ ., data=multi_data)

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)

#view results of backward stepwise regression
backward$anova

# Step Df Deviance Resid. Df Resid. Dev     AIC
#1      NA       NA       442   17563.68 1650.95
#so don't remove not sig term

#RIDGE REGESSION
#https://www.statology.org/ridge-regression-in-r/
#make the model

y <- multi_data$percent_strikeout
x <- data.matrix(multi_data[,c('ff_avg_speed', 'ff_avg_spin', 'ff_avg_break_x', 'ff_avg_break_z')])
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
best_model #the percent (of null) deviance explained (%dev) = 19.32

#produce Ridge trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(model, xvar = "lambda")

#LASOO REGRESSION
#https://www.statology.org/lasso-regression-in-r/
lasooo_model <- glmnet(x, y, alpha = 1)
summary(lasooo_model)

##perform k-fold cross-validation to find optimal lambda value
cv_model2 <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda2 <- cv_model2$lambda.min
best_lambda2 #1.195782

#produce plot of test MSE by lambda value
plot(cv_model2)

#look at the best model
best_model2 <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model2)
best_model2
#the percent (of null) deviance explained (%dev) = 15.32

#K FOLD CROSS VALIDATION
#https://www.statology.org/k-fold-cross-validation-in-r/

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#fit a regression model and use k-fold CV to evaluate performance
lassoo_cv <- train(x, y, alpha = 1, lambda = best_lambda, trControl = ctrl)
ridge_cv <- train(x, y, alpha = 0, lambda = best_lambda, trControl = ctrl)
linear_cv <- train(percent_strikeout~., data=multi_data, method="lm", trControl = ctrl)

#view summary of k-fold CV               
print(lassoo_cv) #    6.579732  0.1489319  5.178836
print(ridge_cv) #     6.552741  0.1488891  5.165473 
print(linear_cv) #    6.330036  0.2051039  4.936704

