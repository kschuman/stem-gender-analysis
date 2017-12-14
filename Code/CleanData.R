library(tidyr)
library(reshape2)
library(CRTgeeDR)
library(ggplot2)
library(dplyr)

setwd('stem-gender-analysis')

# Import data
load("Data/30263-0001-Data.rda")
rawData <- da30263.0001

# Pull out relevant columns

# Informational variables
col_summary <- c("CASENUM","COHORT","GENDER","STRATA",
                 "SCHOOLID","RACETH","RMAJLAST")

# 2007 - 2011 Survey: Standard format variables
# All variables that had the standard name with the
# R through V prefix to designate years
# In order: 
# categorized occupations, size of town/city/etc,
# number of kids, highest level of education attained
prefix <- c("R","S","T","U","V")
columns_employ <- c("STEMM10","URBAN5",
                    "NUMKID","SEDAN5")
prefix_columns <- expand.grid(prefix,columns_employ)
col_employ <- apply(prefix_columns, 1, function(x) paste0(x[1],x[2]) )
col_employ <- col_employ[col_employ != "VURBAN5"]

# 2007 - 2011 Survey: Non-standard format variables
# In order: health ranking, current field of study (2007 has undergrad & grad, 2009 missing),
# marital change and/or marital status 
# (2007, 2011: just status, others have change then status)
y2007 <- c("R20","R316","R321","R3")
y2008 <- c("S1G2","S1B1C","S1D","S1D1")
y2009 <- c("T26","T1D","T20") # Missing T12??
y2010 <- c("U26","U12","U1D","U20")
y2011 <- c("V22","V12","V76AP","VMARIT")

# Extract columns
relevant_columns <- c(col_summary,col_employ,y2007,y2008,y2009,y2010,y2011)
allData <- rawData[,relevant_columns]

# For the RMAJLAST categories, which ones do we consider STEM
# Codes can be found on p.45 of User Guide
stem_majors <- c(1,2,3,4)

# For the STEMM10 codes, which ones do we consider STEM?
# Codes can be found on p.49 of User Guide
stem_jobs <- c(1,2,4,7)

# Define function for these re-groupings
regroup <- function(x,accepted) {
  ifelse(test=is.na(x),
         yes=NA,
         no=as.numeric(x %in% accepted))
}

# Define recoding for marital status columns
desired_levels <- data.frame(final_levels=levels(allData$VMARIT)) %>%
  separate(final_levels,into=c("num","to_level"),sep="\\)") %>%
  select(-num)

codes07 <- levels(allData$R3)
codes08 <- levels(allData$S1D1)
codes09 <- levels(allData$T20)
codes10 <- levels(allData$U20)

map07 <- data.frame(c07=codes07[c(1,2,3,5,4)],desired_levels) %>%
  separate(c07,into=c("num","from_level"),sep="\\)") %>%
  select(-num)

map08 <- data.frame(c08=codes08[c(1,2,3,4,5)],to_level=desired_levels[c(1,1,2,3,4),"to_level"]) %>%
  separate(c08,into=c("num","from_level"),sep="\\)") %>%
  select(-num)

map09 <- data.frame(c09=codes09[c(1,2,3,4)],to_level=desired_levels[c(1,2,3,4),"to_level"]) %>%
  separate(c09,into=c("num","from_level"),sep="\\)") %>%
  select(-num)

map10 <- data.frame(c10=codes10[c(1,2,3,4)],to_level=desired_levels[c(1,2,3,4),"to_level"]) %>%
  separate(c10,into=c("num","from_level"),sep="\\)") %>%
  select(-num)

# Correct health status - adds columns h2007:h2010
health_status <- rawData[c('CASENUM', 'R20', 'HS2008', 'HS2009', 'U26', 'V22', 'S1G', 'T1G', 'U1G', 'V1G')] # Relevent columns
new_health_status <- data.frame(health_status$CASENUM) # Create data frame to store new values
names(new_health_status) <- 'CASENUM' 
new_health_status$h2007 <- health_status$R20 # Add value for each year, back-filling if necessary
new_health_status$h2008 <- health_status$HS2008
new_health_status$h2008 <- ifelse(is.na(new_health_status$h2008) & !is.na(health_status$S1G), new_health_status$h2007, new_health_status$h2008)
new_health_status$h2009 <- health_status$HS2009
new_health_status$h2009 <- ifelse(is.na(new_health_status$h2009) & !is.na(health_status$T1G), new_health_status$h2008, new_health_status$h2009)
new_health_status$h2010 <- health_status$U26
new_health_status$h2010 <- ifelse(is.na(new_health_status$h2010) & !is.na(health_status$U1G), new_health_status$h2009, new_health_status$h2010)
new_health_status$h2011 <- health_status$V22
new_health_status$h2011 <- ifelse(is.na(new_health_status$h2011) & !is.na(health_status$V1G), new_health_status$h2010, new_health_status$h2011)
new_health_status$h2010 <- ifelse(is.na(new_health_status$h2010)  # Use mean for 2010 if 2010 is NA, but 2009 and 2011 aren't
                                  & !is.na(new_health_status$h2009) 
                                  & !is.na(new_health_status$h2011), 
                                  (new_health_status$h2009 + new_health_status$h2011)/2, new_health_status$h2010)
allData <- merge(allData, new_health_status, by='CASENUM') # Merge on case number with original dataset
allData <- subset(allData, select = -c(R20, U26)) # Remove old columns

# Prepare data by recoding college and employment fields
cleanData <- allData %>%
  mutate(MAJSTEM=regroup(as.numeric(RMAJLAST)-1,accepted=stem_majors)) %>% 
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=as.numeric) %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=funs(regroup),accepted=stem_jobs) %>%
  mutate(RMAJLAST=regroup(as.numeric(RMAJLAST)-1,accepted=stem_majors))



#longData <- select(stem_majors, -c(RSTEMM10, SSTEMM10, TSTEMM10, USTEMM10, VSTEMM10, RMAJLAST)) %>% 
#  melt(variable.name='key', value.names='value', 
#       id.vars=c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'SCHOOLID', 'RACETH'))



#---------------------------
# Condition on STEM major in college
stem_majors_list <- cleanData[which(cleanData$RMAJLAST == 1),]

# Table for yearly STEM engagement
stem_table <- stem_majors_list[c('CASENUM', 'RSTEMM10', 'SSTEMM10', 'TSTEMM10', 'USTEMM10', 'VSTEMM10')]
names(stem_table) <- c('CASENUM', 'j07', 'j08', 'j09', 'j10', 'j11')
stem_table <- melt(stem_table, variable.name = 'life_stage', value.name='STEM', id.var = 'CASENUM')


# Long form for all data
column_years <- read.csv('Data/col_years.csv', header=TRUE)
names(column_years) <- c('key', 'life_stage')
longData <- select(stem_majors_list, -c(RSTEMM10, SSTEMM10, TSTEMM10, USTEMM10, VSTEMM10, 
                                   RMAJLAST, R321, R316, S1G2, S1B1C, S1D, S1D1, T26, T1D, T20, U12,
                                   U1D, U20, V22, V12, V76AP, VMARIT, R3)) %>% 
  melt(variable.name='key', value.names='value', 
                 id.vars=c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'SCHOOLID', 'RACETH')) %>% 
  merge(column_years, by='key') %>% 
  left_join(stem_table, by = c('CASENUM', 'life_stage'))

head(longData)

## Haven't gotten to this part yet---old code
# Convert to long format
years_translator <- read.csv("Data/years_translator.csv",header=TRUE)
cohort_years <- years_translator %>%
  gather(key="cohort",value="life_stage",cohort2,cohort1) %>%
  separate(cohort,into=c("junk","cohort"),sep=-2) %>%
  select(-junk) %>%
  mutate(cohort=as.numeric(cohort))


# Add years from baseline based on cohort and year
cohort_years$COHORT <- ifelse(cohort_years$cohort == 2, '(2) Younger', '(1) Older')
cohort_years <- cohort_years[c('year_from_baseline', 'life_stage', 'COHORT')]
longData <- merge(longData, cohort_years, by=c('life_stage', 'COHORT'))
View(longData)



#---------------------------
# Wide data
wideData <- spread(longData, key=key, value=value, convert=TRUE)
names(wideData)
wideData$urban <- NA
wideData$nkids <- NA
wideData$edulevel <- NA
wideData$health <- NA

wideData$urban <- wideData$RURBAN5
wideData$urban <- ifelse(is.na(wideData$urban), wideData$SURBAN5, wideData$urban) 
wideData$urban <- ifelse(is.na(wideData$urban), wideData$TURBAN5, wideData$urban) 
wideData$urban <- ifelse(is.na(wideData$urban), wideData$UURBAN5, wideData$urban) 

wideData$nkids <- wideData$RNUMKID
wideData$nkids <- ifelse(is.na(wideData$nkids), wideData$SNUMKID, wideData$nkids) 
wideData$nkids <- ifelse(is.na(wideData$nkids), wideData$TNUMKID, wideData$nkids) 
wideData$nkids <- ifelse(is.na(wideData$nkids), wideData$UNUMKID, wideData$nkids) 
wideData$nkids <- ifelse(is.na(wideData$nkids), wideData$VNUMKID, wideData$nkids) 

wideData$edulevel <- wideData$RSEDAN5
wideData$edulevel <- ifelse(is.na(wideData$edulevel), wideData$SSEDAN5, wideData$edulevel) 
wideData$edulevel <- ifelse(is.na(wideData$edulevel), wideData$TSEDAN5, wideData$edulevel) 
wideData$edulevel <- ifelse(is.na(wideData$edulevel), wideData$USEDAN5, wideData$edulevel) 
wideData$edulevel <- ifelse(is.na(wideData$edulevel), wideData$VSEDAN5, wideData$edulevel) 

wideData$health <- wideData$h2007
wideData$health <- ifelse(is.na(wideData$health), wideData$h2008, wideData$health)
wideData$health <- ifelse(is.na(wideData$health), wideData$h2009, wideData$health)
wideData$health <- ifelse(is.na(wideData$health), wideData$h2010, wideData$health)
wideData$health <- ifelse(is.na(wideData$health), wideData$h2011, wideData$health) 

wideData <- select(wideData, CASENUM, COHORT, GENDER, STRATA, SCHOOLID, RACETH, STEM, year_from_baseline,
                   urban, nkids, edulevel, health)
head(wideData)
wideData <- wideData[order(wideData$CASENUM),]



# -------------------------------
# Modeling

modelData <- wideData

# Prep Data
modelData$time <- modelData$year_from_baseline - 20
modelData$MISSING <- ifelse(is.na(modelData$STEM), 1, 0)
modelData$GENDER <- as.factor(ifelse(modelData$GENDER == '(2) Male', 'Male', 'Female'))
modelData$GENDER.int <- as.integer(modelData$GENDER) - 1
modelData$edulevel <- as.factor(ifelse(modelData$edulevel == '(4) Masters', '(3) Bacc', 
                                       modelData$edulevel))
levels(modelData$edulevel) <- c('<PHD', 'PHD')
View(modelData)

# Time * Gender
model1 <- geeDREstimation(STEM~as.factor(GENDER)*time, id='CASENUM', data=modelData,
                          nameTRT='time', family='binomial',
                          model.weights = I(MISSING==0) ~ time*GENDER.int)


info <- data.frame(Parameter=c('Intercept', 'Gender-Male', 'Time', 'Gender-Male:Time'), 
                   Estimate=c(0.375, 3.610, 0.969, 1.015),
                   Lower=c(0.201, 1.744, 0.838, 0.858),
                   Upper=c(0.701, 7.475, 1.120, 1.202),
                   SE=round(c(exp(.318), exp(.371), exp(.074), exp(.086)), 3),
                   `p-value`=c('0.002', '<.001', '0.667', '0.860'))
info
info %>%kable(format='latex')


# Time * Gender * # Kids
model2 <- geeDREstimation(STEM~time*nkids*as.factor(GENDER), id='CASENUM', data=modelData,
                          nameTRT='time', family='binomial',
                          model.weights = I(MISSING==0) ~ time * STRATA * COHORT * nkids )

info <- data.frame(Parameter=c('Intercept', 'Time', 'Number Kids', 'Gender-Male',
                               'Time:NumberKids', 'Time:Gender-Male', 'NumberKids:Gender-Male',
                               'Time:NumberKids:Gender-Male'),
                   Estimate=c(0.503, 0.952, 0.735, 3.510, 1.033, 1.000, 1.079, 1.005),
                   Lower=c(0.219, 0.781, 0.412, 1.339, 0.911, 0.788, 0.560, 0.871),
                   Upper=c(1.156, 1.161, 1.314, 9.201, 1.170, 1.268, 2.076, 1.159),
                   SE=round(exp(c( 0.424, 0.101, 0.296, 0.492, 0.064, 0.121, 0.334, 0.073)), 3),
                   `p-value`=c(0.106, 0.626, 0.299, 0.011, 0.615, 0.998, 0.820, 0.950))

info %>%kable(format='latex')


getPSPlot(model2)
print(summary(model1))

median(model2$weights)
(model1$weights)

getCI(model1, name='time') %>% kable(format="latex")



summary(model2)
getCI(model2, name='time')
getPSPlot(model2, typeplot = 0)
print(summary(model2))


# GENDER
means.1 <- aggregate(modelData$STEM, by=list(modelData$GENDER, modelData$time, modelData$COHORT), FUN=mean, na.rm = T)
means.1
names(means.1) <- c('Gender', 'Time', 'Cohort', 'Mean STEM Engagement')
means.1$`GenderCohort` <- paste(means.1$Gender, means.1$Cohort)
means.1$Gender <- as.factor(means.1$Gender)
ggplot(means.1, aes(y=`Mean STEM Engagement`, x=Time, col=GenderCohort, group=GenderCohort)) + 
  geom_point() + 
  geom_line() +
  labs(x = 'Time (post-baseline, in years)', 
       title = 'Mean STEM Engagement in Adults Over Time')


# Number of Kids
means.kids <- aggregate(modelData$nkids, by=list(modelData$GENDER, modelData$time), FUN=mean, na.rm = T)
names(means.kids) <- c('Gender', 'Time', 'Mean # Children')
means.kids$Gender <- as.factor(means.kids$Gender)
ggplot(means.kids, aes(y=`Mean # Children`, x=Time, col=Gender, 
                    group=Gender)) + geom_point() + geom_line() +
  labs(x = 'Time (post-baseline, in years)', 
       title = 'Mean Number of Children at Home Over Time')

# Number of Kids & STEM 
means.kids.stem <- aggregate(modelData$STEM, by=list(modelData$GENDER, modelData$nkids), FUN=mean, na.rm=T)
names(means.kids.stem) <-  c('Gender', '# Children', 'Mean STEM Engagement')
means.kids.stem$Gender <- as.factor(means.kids.stem$Gender)
ggplot(means.kids.stem, aes(y=`Mean STEM Engagement`, x=`# Children`, col=Gender, 
                       group=Gender)) + geom_point() + geom_line() +
  labs(title = 'STEM Engagement by Number of Children')

modelData[which(modelData$nkids > 4),]





# Health Over Time
means.health <- aggregate(modelData$health, by=list(modelData$GENDER, modelData$time), FUN=mean, na.rm=T)
names(means.health) <- c('Gender', 'Time', 'Mean Health Rating')
means.health$Gender <- as.factor(means.health$Gender)

ggplot(means.health, aes(y=`Mean Health Rating`, x=Time, col=Gender,
                    group=Gender)) + geom_point() + geom_line() +
  labs(x = 'Time (post-baseline, in years)', 
       title = 'Mean Health Rating in Adults Over Time')

# Health Rating by # Children
means.health2 <- aggregate(modelData$health, by=list(modelData$GENDER, modelData$nkids), FUN=mean, na.rm=T)
names(means.health2) <- c('Gender', '# Children', 'Mean Health Rating')
means.health2$Gender <- as.factor(means.health2$Gender)

ggplot(means.health2, aes(y=`Mean Health Rating`, x=`# Children`, col=`Gender`,
                         group=`Gender`)) + geom_point() + geom_line() +
  labs(title = 'Mean Health Rating in Adults Over Time')

# Health and STEM
modelData$health.cut <- cut(modelData$health, 10, labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
modelData$health.cut
means.health3 <- aggregate(modelData$STEM, by=list(modelData$GENDER, modelData$health.cut), FUN=mean, na.rm=T)
means.health3
names(means.health3) <- c('Gender', 'Health', 'STEM')
means.health3$Gender <- as.factor(means.health3$Gender)
means.health3$Health <- as.factor(means.health3$Health)

ggplot(means.health3, aes(y=STEM, x=Health, col=Gender,
                          group=`Gender`)) + geom_point() + geom_line() +
  labs(title = 'Mean Stem Engagement by Health Status', x='Health Rating', y='Mean STEM Engagement')



# STEM by # Children
ggplot(modelData, aes(x=as.factor(STEM), y=as.factor(nkids), color=GENDER)) + geom_jitter(alpha=.7) + 
  labs(x='STEM Engagement', y='# Children at Home', title='STEM Engagement by Number of Children')



# Missing values
summary(modelData)

