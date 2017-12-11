library(ggplot2)
library(geepack)
library(dplyr)


setwd('stem-gender-analysis')
cgoal <- read.csv('Data/career-goals.csv')

View(cgoal)
# Get means
means <- aggregate(stem ~ gender + time, data = cgoal, FUN=mean, na.rm = TRUE, weights=weight)
means

# Plot means
ggplot(means, aes(x=as.numeric(time), y=stem, group=gender, color=gender)) + geom_point() +geom_line()

# Sample sizes
count(cgoal, gender)

# GEE model
m1 <- geeglm(stem ~ gender*time, data = cgoal, id=id, family=binomial(), weights=weight)
summary(m1)

glimpse(cgoal)
