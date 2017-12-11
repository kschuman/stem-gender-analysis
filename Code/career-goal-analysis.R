library(ggplot2)
library(geepack)
library(dplyr)


setwd('stem-gender-analysis')
cgoal <- read.csv('Data/career-goals.csv')


# Get means
means <- aggregate(stem ~ gender + yr, data = cgoal, FUN=mean, na.rm = TRUE)

# Plot means
ggplot(means, aes(x=as.numeric(yr), y=stem, group=gender, color=gender)) + geom_point() +geom_line()

# Sample sizes
count(cgoal, cohort)

head(cgoal)
m1 <- geeglm(stem ~ gender*time, data = cgoal, id=id, family=binomial())
summary(m1)
