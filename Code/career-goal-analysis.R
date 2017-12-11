library(ggplot2)
library(geepack)
library(dplyr)


setwd('stem-gender-analysis')
cgoal <- read.csv('Data/career-goals.csv')
cgoal <- cgoal[c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'stem', 'weight', 'time')]

load("/Users/kschuman/stem-gender-analysis/Data/college_employment_long.RData")
after_hs <- longData
after_hs <- after_hs[c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'engagement', 'weight', 'year_from_baseline')]
names(cgoal) <- names(after_hs)

all_data <- rbind(cgoal, after_hs)
glimpse(all_data)


# Get means
means <- aggregate(engagement ~ GENDER + year_from_baseline, data = all_data, FUN=mean, na.rm = TRUE, weights=weight)
means


# Plot means
ggplot(means, aes(x=as.numeric(year_from_baseline), y=engagement, group=GENDER, color=GENDER)) + 
  geom_point() +geom_line()


# GEE model
m1 <- geeglm(engagement ~ GENDER*year_from_baseline, data = cgoal, id=CASENUM, family=binomial(), weights=weight)
summary(m1)

glimpse(cgoal)

View(all_data)

# Grade school
grade <- all_data[which(all_data$year_from_baseline < 6),]


post_college <- all_data[which(all_data$year_from_baseline > 12),]

m2 <- geeglm(engagement ~ GENDER*year_from_baseline, data = post_college, id=CASENUM, family=binomial(), weights=weight)
summary(m2)

