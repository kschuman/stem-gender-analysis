library(tidyr)
library(dplyr)
library(reshape2)
library(CRTgeeDR)

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
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=as.numeric) %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=funs(regroup),accepted=stem_jobs) %>%
  mutate(RMAJLAST=regroup(as.numeric(RMAJLAST),accepted=stem_majors))


#longData <- select(stem_majors, -c(RSTEMM10, SSTEMM10, TSTEMM10, USTEMM10, VSTEMM10, RMAJLAST)) %>% 
#  melt(variable.name='key', value.names='value', 
#       id.vars=c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'SCHOOLID', 'RACETH'))

#ids <- 
#k <- melt(stem_majors, variable.name = 'key', value.name='value', id.vars = )




#---------------------------
# Condition on STEM major in college
stem_majors <- cleanData[which(cleanData$RMAJLAST == 1),]

# Table for yearly STEM engagement
stem_table <- stem_majors[c('CASENUM', 'RSTEMM10', 'SSTEMM10', 'TSTEMM10', 'USTEMM10', 'VSTEMM10')]
names(stem_table) <- c('CASENUM', 'j07', 'j08', 'j09', 'j10', 'j11')
stem_table <- melt(stem_table, variable.name = 'life_stage', value.name='STEM', id.var = 'CASENUM')


# Long form for all data
column_years <- read.csv('Data/col_years.csv', header=TRUE)
names(column_years) <- c('key', 'life_stage')
longData <- select(stem_majors, -c(RSTEMM10, SSTEMM10, TSTEMM10, USTEMM10, VSTEMM10, 
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

wideData <- select(wideData, c('CASENUM', 'COHORT', 'GENDER', 'STRATA', 'SCHOOLID', 'RACETH', 'STEM', 'year_from_baseline',
                   'urban', 'nkids', 'edulevel', 'health'))
wideData <- wideData[order(wideData$CASENUM),]



# -------------------------------
# Modeling
model1 <- geeDREstimation(STEM~GENDER.int*year_from_baseline, id='CASENUM', data=wideData, nameTRT='GENDER.int', family='binomial')
summary(model1)

wideData$MISSING <- ifelse(is.na(wideData$STEM), 1, 0)
wideData$TRT <- wideData$GENDER.int
wideData$OUTCOME <- wideData$STEM
wideData$AGE <- wideData$year_from_baseline
wideData$CLUSTER <- wideData$CASENUM
head(wideData)

weights <- as.integer(I(wideData$MISSING==0)-wideData$GENDER.int*wideData$year_from_baseline)
weights

model2 <- geeDREstimation(OUTCOME~TRT*AGE, id='CLUSTER', data=wideData, family='binomial',
                          model.weights = I(wideData$MISSING==0)-wideData$TRT*wideData$AGE)

model2 <- geeDREstimation(OUTCOME~TRT*AGE, id='CLUSTER', data=data.sim, family='binomial',
                                    model.weights = data.sim$MISSING)
  
data.sim$MISSING

head(data.sim)

summary(model2)

model2 <- geeDREstimation(STEM~GENDER.int*year_from_baseline*health, id='CASENUM', data=wideData, 
                          nameTRT='GENDER.int', family='binomial',model.weights = I(wideData$MISSING==0)~GENDER.int*wideData$year_from_baseline*health)

summary(model2)



wideData


library(ggplot2)


means <- aggregate(wideData$STEM, by=list(wideData$GENDER, wideData$year_from_baseline), FUN=mean, na.rm = T)

ggplot(means, aes(y=x, x=Group.2)) + geom_point()







