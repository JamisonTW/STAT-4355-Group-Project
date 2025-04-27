# load data and packages
data <- read.csv("Life Expectancy Data.csv")
library(tidyverse)
library(car)

# remove non-numeric data
data1 <- data %>% select(-Country, -Year)
# change status to a categorical variable to be used in regression
data1$Status <- relevel(as.factor(data$Status), ref = 'Developed')

# fit initial model
model1 <- lm(Life.expectancy ~ ., data = data1)
summary(model1)

# check if there is a difference if we subset the data
## subset data by status
data_developed <- subset(data, data$Status == 'Developed')
data1_developed <- data_developed %>% select(-Status, -Country, -Year)
data_developing <- subset(data, data$Status == 'Developing')
data1_developing <- data_developing %>% select(-Status, -Country, -Year)

## fit subset models
model1_developed <- lm(Life.expectancy ~ ., data = data1_developed)
summary(model1_developed)
model1_developing <- lm(Life.expectancy ~ ., data = data1_developing)
summary(model1_developing)
# note: the model is less accurate after subsetting data by status, revisit this after fitting improved model

# check for multicollinearity on initial model
# using status as a factor variable causes multicolinearity issues because they are aliased coefficients, so we will not use it going forward
data2 <- data1 %>% select(-Status)
model2 <- lm(Life.expectancy ~ ., data = data2)
vif(model2)
# notably high vif are infant deaths, percentage expenditure, under five deaths, gdp, thinness 1-19 and thinness 5-9
# we will remove under five deaths and thinness 5-9
# we will also remove gdp as we believe it is causing issues with percentage expenditure and it has a higher vif
data3 <- data2 %>% select(-infant.deaths, - thinness.5.9.years, -GDP)

# fit new model
model3 <- lm(Life.expectancy ~ ., data = data3)
summary(model3)
vif(model3)

# test removing percentage expenditure instead of gdp
data4 <- data2 %>% select(-infant.deaths, - thinness.5.9.years, -percentage.expenditure)
model4 <- lm(Life.expectancy ~., data = data4)
summary(model4)
vif(model4)
# income composition of resources has a slightly higher vif when removing percentage expenditure rather than gdp, everything else is rather similar
# id rather use gdp as it is a more recognized variable

# test residuals
library(MASS)

standard_residual <- stdres(model4)
standard_residual

barplot(standard_residual)
abline(h = c(-3, 3), col = "red", lwd = 2)

plot(standard_residual)
abline(h = c(-3, 3), col = "red", lwd = 2)

# influential analysis
inf1 <- influence.measures(model4)
suminf1 <- summary(inf1)

dfbetasPlots(model4,intercept=T)
influenceIndexPlot(model4)

# based on influence index plot outliers are 1904, 2503, 2306, 1188, 1195

# normal probability plot (Q-Q plot)
par(mfrow=c(1,2))
hist(studres(model4), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(model4)

# residuals against fitted values
plot(fitted(model4), residuals(model4),
     main="Residuals vs Fitted",
     xlab="Fitted Values", ylab="Residuals")

#  start removing coefficients with the highest p value one by one to reduce and improve the model
# remove thinness first as it has highest p value
data5 <- data4 %>% select(-thinness..1.19.years)
model5 <- lm(Life.expectancy ~., data = data5)
summary(model5)
vif(model5)
