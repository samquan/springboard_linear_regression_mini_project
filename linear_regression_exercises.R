## EXERCISE 1: LEAST SQUARES REGRESSION 

# setting work directory and uploading data
setwd("C:/Users/Samantha Quan/Documents")
library(ggplot2)
states = readRDS("states.rds")

# examine and plotting data 
str(states)
summary(states)
ggplot(states, aes(x = metro, y = energy)) + geom_point()
# notes: points look like they're following a baseline model... 

# first model 
model1 = lm(energy ~ metro, data = states)
summary(model1)
SSE1 = sum(model1$residuals^2)
SSE1 
# note: adjusted R-squared 0.097, SSE 943102.08

# examining density and area
ggplot(states, aes(x = density, y = energy)) + geom_point()
ggplot(states, aes(x = area, y = energy)) + geom_point()
# notes: density looks like a flat line, area looks like an upward slope  

# second model
model2 = lm(energy ~ metro + density + area, data = states)
summary(model2)
SSE2 = sum(model2$residuals^2)
SSE2
# notes: adjusted R-squared 0.4636, SSE 536852.8

# model2 is significantly better than model with /metro/ as the only predictor


## EXERCISE 2: INTERACTIONS AND FACTORS

# adding an interaction term income, maybe the energy consumed has to do with income?
model3 = lm(energy ~ (metro + density + area)*income, data = states)
summary(model3)
SSE3 = sum(model3$residuals^2)
SSE3
# notes: adjusted R-squared 0.5061, SSE 451330.2
# model3 seems to be better than model2 since adjusted R-squared has gotten larger and SSE has gotten lower


# make sure R knows region is categorical
factor(states$region)
states$region
str(states$region)

# adding region
model4 = lm(energy ~ (metro + density + area)*income + region, data = states)
summary(model4)

# There are not significant differences across the four regions, coefficients are kind of small
