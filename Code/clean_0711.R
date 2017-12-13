library(tidyr)
library(dplyr)

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


# Prepare data by recoding college and employment fields

cleanData <- allData %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=as.numeric) %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=funs(regroup),accepted=stem_jobs) %>%
  mutate(RMAJLAST=regroup(as.numeric(RMAJLAST),accepted=stem_majors))
glimpse(cleanData)


## Haven't gotten to this part yet---old code
# Convert to long format
years_translator <- read.csv("Data/years_translator.csv",header=TRUE)
cohort_years <- years_translator %>%
  gather(key="cohort",value="life_stage",cohort2,cohort1) %>%
  separate(cohort,into=c("junk","cohort"),sep=-2) %>%
  select(-junk) %>%
  mutate(cohort=as.numeric(cohort))

