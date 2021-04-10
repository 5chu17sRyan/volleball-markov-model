#Current directory
currentDirectory <- getwd()

#packages
require(tidyverse)
library(ggpubr)
library(lmtest)

#install.packages("lmtest", dependencies = T, repos='http://cran.us.r-project.org' )
#install.packages("ggpubr")

#loading in the data
#data <- read.csv(paste0(currentDirectory, "/strikeouts_pitchspin.csv"))

#cleaning up column names
# data <- data %>%
#   select(-c(player_id, X))
# 
# names <- c("last_name", 'first_name', 'year', 'batters_facing', 'strikeouts', 'average_spin')
# colnames(data) <- names
# 
# data$percent_strikeout <- data$strikeouts/data$batters_facing
# data$percent_strikeout <- data$percent_strikeout*100

#read in file
strikeout_data <- read.csv(paste0(currentDirectory, "/strikeout_data.csv"))

head(strikeout_data)

#### LEAST SQUARES REGRESSION ####
#check for linear relationship
plot(strikeout_data$percent_strikeout, strikeout_data$average_spin) #kind of

#Check that variables are normal
ggqqplot(strikeout_data$percent_strikeout) #gray band is 95% confidence interval
ggqqplot(strikeout_data$average_spin)

#make model
leastSquares <- lm(percent_strikeout~average_spin, data=strikeout_data)
summary(leastSquares)

#check for homoscedasticity
plot(leastSquares, 1) #fat in the middle
gqtest(formula = leastSquares, fraction = .2) # https://www.statisticshowto.com/goldfeld-quandt-test/
#variance does not significantly change (good)

#Independence: players on different teams are probably independent players on the same team may be trained in similar ways 

#check for autocorrelation
dwtest(formula = leastSquares) #https://www.statisticshowto.com/durbin-watson-test-coefficient/
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35c.htm
# https://www.statisticssolutions.com/autocorrelation/
#no autocorrelation (good)

#check for normality
ggqqplot(leastSquares$residuals)
ks.test(x=leastSquares$residuals, y = pnorm, alternative = "two.sided") #https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
#not normal (bad)

#### Multiple Linear Regression #####
strikeoutManyVariables <- read.csv(paste0(currentDirectory, "/strikeout_many_variables.csv"))

numPitchers <- nrow(strikeoutManyVariables)
maxVariables <- floor(numPitchers / 20) #Need at least 20 cases per independent variable

#RIDGE REGESSION

#LASOO REGRESSION

#WEIGHTED LEAST SQUARES REGRESSION

#K NEAREST NEIGHBORS