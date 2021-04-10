#packages
require(tidyverse)
require(dplyr)
library(Hmisc)

#Ryan did this
# LINEAR REGRESSION
# #loading in the data
# data <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Baseball/strikeouts_pitchspin.csv")
# 
# #cleaning up column names
# data <- data %>%
#   select(-c(player_id, X))
# 
# names <- c("last_name", 'first_name', 'year', 'batters_facing', 'strikeouts', 'average_spin')
# colnames(data) <- names
# 
# data$percent_strikeout <- data$strikeouts/data$batters_facing
# data$percent_strikeout <- data$percent_strikeout*100
# 
# #write out file
# #write.csv(data, "C:/Users/18083/Desktop/202110/Sports Analytics/Baseball/strikeout_data.csv")
# 
# #LEAST SQUARES REGRESSION
# #check for linear relationship
# plot(data$percent_strikeout, data$average_spin) #kind of
# 
# #make model
# model <- glm(percent_strikeout~., data=data)
# 
# #check for homoscedasticity
# plot(model, 1) #fat in the middle
# 
# #check for normality
# plot(model, 2) 

#Multiple Linear Regression
multi_data <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Baseball/strikeout_many_variables.csv")
multy_data <- multi_data %>%
  select(c(percent_strikeout, ff_avg_speed, ff_avg_spin, ff_avg_break_x, ff_avg_break_z))
multi_data$percent_strikeout <- 100*(multi_data$p_strikeout/multi_data$p_total_pa)
head(multi_data)
multi_model <- glm(percent_strikeout~ ff_avg_speed + ff_avg_spin + ff_avg_break_x +ff_avg_break_z, data=multi_data )
summary(multi_model)

#multivariate normality
plot(multi_model, 2) #looks ok
#homoscedacitiy
plot(multi_model, 1) #without the outlier looks fine
#no multicollinearity
mydata.rcorr = rcorr(as.matrix(multy_data))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

mydata.coeff
mydata.p #got some issues

#RIDGE REGESSION

#LASOO REGRESSION

#WEIGHTED LEAST SQUARES REGRESSION

#K NEAREST NEIGHBORS