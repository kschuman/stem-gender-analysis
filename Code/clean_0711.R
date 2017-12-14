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
# In order: health ranking,
# marital change and/or marital status 
# (2007, 2011: just status, others have change then status)
y2007 <- c("R20","R3")
y2008 <- c("S1G2","S1D","S1D1")
y2009 <- c("T26","T1D","T20")
y2010 <- c("U26","U1D","U20")
y2011 <- c("V22","VMARIT")

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
  select(-num) %>%
  mutate(to_level=as.character(to_level))
marital_levels <- levels(desired_levels$to_level)

codes07 <- levels(allData$R3)
codes08 <- levels(allData$S1D1)
codes09 <- levels(allData$T20)
codes10 <- levels(allData$U20)

map07 <- data.frame(c07=codes07[c(1,2,3,5,4)],desired_levels) %>%
  separate(c07,into=c("num","from_level"),sep="\\)") %>%
  select(-num)

map08 <- data.frame(c08=codes08[c(1,2,3,4,5)],to_level=desired_levels[c(1,1,2,3,4),"to_level"]) %>%
  separate(c08,into=c("num","from_level"),sep="\\)") %>%
  select(-num) %>%
  mutate(to_level=as.character(to_level))
  
map09 <- data.frame(c09=codes09[c(1,2,3,4)],to_level=desired_levels[c(1,2,3,4),"to_level"]) %>%
  separate(c09,into=c("num","from_level"),sep="\\)") %>%
  select(-num) %>%
  mutate(to_level=as.character(to_level))

map10 <- data.frame(c10=codes10[c(1,2,3,4)],to_level=desired_levels[c(1,2,3,4),"to_level"]) %>%
  separate(c10,into=c("num","from_level"),sep="\\)") %>%
  select(-num)  %>%
  mutate(to_level=as.character(to_level))

remap <- function(x,mapping,change_flag=FALSE,change_column=NA,prev_column=NA) {
  if (change_flag) {
    new <- data.frame(from_level=x) %>%
      bind_cols(data.frame(change_column=change_column-1,
                           prev_level=prev_column)) %>%
      separate(from_level,into=c("num","from_level"),sep="\\)") %>%
      select(-num) %>%
      left_join(mapping,by="from_level") %>%
      mutate(update=ifelse(test=is.na(change_column),
                           yes=NA,
                           no=ifelse(test=change_column==1,
                                     yes=to_level,
                                     no=prev_level))) %>%
      select(update)
    return(new$update)
  }
  else {
    x <- data.frame(from_level=x) %>%
      separate(from_level,into=c("num","from_level"),sep="\\)") %>%
      select(-num)
    new <- left_join(x,mapping, by="from_level") %>%
      select(to_level)
    return(new$to_level)
  }
}

# Prepare data:
# (1) recode college and employment fields to STEM / Not-STEM
# (2) put marital status in same groupings year-to-year

cleanData <- allData %>%
  mutate(MAJSTEM=regroup(as.numeric(RMAJLAST)-1,accepted=stem_majors)) %>%
  filter(MAJSTEM==1) %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=as.numeric) %>%
  mutate_at(.vars=vars("RSTEMM10","SSTEMM10","TSTEMM10","USTEMM10","VSTEMM10"),
            .funs=funs(regroup),accepted=stem_jobs) %>%
  mutate_at(.vars=vars("R3","S1D1","T20","U20","VMARIT"),
            .funs=funs(as.character)) %>%
  mutate_at(.vars=vars("S1D","T1D","U1D"),
            .funs=funs(as.numeric)) %>%
  mutate(MARIT07=remap(R3,map07)) %>%
  mutate(MARIT08=remap(S1D1,map08,change_flag=TRUE,change_column=.[,"S1D"],prev_column=.[,"MARIT07"])) %>%
  mutate(MARIT09=remap(T20,map09,change_flag=TRUE,change_column=.[,"T1D"],prev_column=.[,"MARIT08"])) %>%
  mutate(MARIT10=remap(U20,map10,change_flag=TRUE,change_column=.[,"U1D"],prev_column=.[,"MARIT09"])) %>%
  separate(VMARIT,into=c("num","MARIT11"),sep="\\)") %>%
  select(-num)

glimpse(cleanData)


## Haven't gotten to this part yet---old code
# Convert to long format
years_translator <- read.csv("Data/years_translator.csv",header=TRUE)
cohort_years <- years_translator %>%
  gather(key="cohort",value="life_stage",cohort2,cohort1) %>%
  separate(cohort,into=c("junk","cohort"),sep=-2) %>%
  select(-junk) %>%
  mutate(cohort=as.numeric(cohort))

cleanData %>%
  select("RSTEMM10") %>%
  skim()
