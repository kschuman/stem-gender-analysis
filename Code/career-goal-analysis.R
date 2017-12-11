library(ggplot2)
library(geepack)
library(dplyr)

# Read data and merge to full dataset
setwd('stem-gender-analysis')
cgoal <- read.csv('Data/career-goals.csv')
cgoal <- cgoal[c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'stem', 'weight', 'time')]

load("Data/college_employment_long.RData")
after_hs <- longData
after_hs <- after_hs[c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'engagement', 'weight', 'year_from_baseline')]
names(cgoal) <- names(after_hs)

all_data <- rbind(cgoal, after_hs)

# Get means
means <- aggregate(engagement ~ GENDER + year_from_baseline, data = all_data, FUN=mean, na.rm = TRUE, weights=weight)
means

# Plot means
ggplot(means, aes(x=as.numeric(year_from_baseline), y=engagement, group=GENDER, color=GENDER)) + 
  geom_point() +geom_line()

# GEE model - overall
m1 <- geeglm(engagement ~ GENDER*year_from_baseline, data = cgoal, id=CASENUM, family=binomial(), weights=weight)
summary(m1)



# Grade school
grade <- all_data[which(all_data$year_from_baseline < 6),]
m4 <- geeglm(engagement ~ GENDER*year_from_baseline, data = grade, id=CASENUM, family=binomial(), weights=weight)
summary(m4)$coef

means.1 <- means[which(means$year_from_baseline < 6),]
ggplot(means.1, aes(x=as.numeric(year_from_baseline), y=engagement, group=GENDER, color=GENDER)) + 
  geom_point() +geom_line()

# College
coll <- all_data[which(all_data$year_from_baseline < 12 & all_data$year_from_baseline > 5),]
m3 <- geeglm(engagement ~ GENDER*year_from_baseline, data = coll, id=CASENUM, family=binomial(), weights=weight)
summary(m3)$coef
summary(m3)

means.2 <- means[which(means$year_from_baseline < 12 & means$year_from_baseline > 5),]
ggplot(means.2, aes(x=as.numeric(year_from_baseline), y=engagement, group=GENDER, color=GENDER)) + 
  geom_point() +geom_line()

# Post college
post_college <- all_data[which(all_data$year_from_baseline > 12),]
m2 <- geeglm(engagement ~ GENDER*year_from_baseline, data = post_college, id=CASENUM, family=binomial(), weights=weight)
summary(m2)$coef

means.3 <- means[which(means$year_from_baseline > 12),]
ggplot(means.3, aes(x=as.numeric(year_from_baseline), y=engagement, group=GENDER, color=GENDER)) + 
  geom_point() +geom_line()
