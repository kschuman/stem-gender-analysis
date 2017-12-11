library(tidyr)
library(dplyr)

# Import data
load("Data/30263-0001-Data.rda")
rawData <- da30263.0001

# Recode majors
NA10 <- levels(rawData$NA10)
OC12 <- levels(rawData$OC12)
QC10 <- levels(rawData$QC10)
majors <- data.frame(survey=c(rep('P1',length(NA10)),
                              rep('P2',length(OC12)),
                              rep('P4',length(QC10))),
                     major=c(NA10,OC12,QC10))
write.table(majors,"Data/majors.csv",row.names = FALSE,sep=',')

# Pull out relevant columns

# Information
col_meta <- c("CASENUM","COHORT","GENDER","STRATA")
col_ps <- c("NA2","OC2","QC2",
            "NA10","OC12","QC10",
            "WEIGHTP1","WEIGHTP2","WEIGHTP4")

employ_prefix <- c("R","S","T","U","V")
columns_employ <- c("FLAG","STEMMA","STEMMB","STEMM10")
prefix_columns <- expand.grid(employ_prefix,columns_employ)
combined_vars <- apply(prefix_columns, 1, function(x) paste0(x[1],x[2]) )
col_employ <- c(combined_vars,"WEIGHTR","WEIGHTS","WEIGHTT","WEIGHTU","WEIGHTV",
                    "ROCC8")

relevant_columns <- c(col_meta,col_ps,col_employ)
                      
allData <- rawData[,relevant_columns]

# Bring in hand-coded majors: STEM, Medicine, Social Science
major_labels <- read.csv("Data/majors_coded.csv",header=TRUE)
head(major_labels)
p1 <- major_labels[major_labels$survey=="P1",]
p2 <- major_labels[major_labels$survey=="P2",]
p4 <- major_labels[major_labels$survey=="P4",]

# For the _STEMM10 codes, which ones do we consider STEM?
# Codes can be found on p.49 of User Guide
stem_jobs <- c(1,2,4,7)


# Prepare data by recoding college and employment fields
cleanData <- allData %>%
  left_join(p1,by=c("NA10"="major")) %>%
  select(-survey) %>%
  rename(p1_stem=stem,p1_med=medicine,p1_ss=social.science) %>%
  left_join(p2,by=c("OC12"="major")) %>%
  select(-survey) %>%
  rename(p2_stem=stem,p2_med=medicine,p2_ss=social.science) %>%
  left_join(p4,by=c("QC10"="major")) %>%
  select(-survey) %>%
  rename(p4_stem=stem,p4_med=medicine,p4_ss=social.science) %>%
  mutate(j07_stem=ifelse(is.na(RSTEMM10),NA,as.numeric(RSTEMM10) %in% stem_jobs),
         j08_stem=ifelse(is.na(SSTEMM10),NA,as.numeric(SSTEMM10) %in% stem_jobs),
         j09_stem=ifelse(is.na(TSTEMM10),NA,as.numeric(TSTEMM10) %in% stem_jobs),
         j10_stem=ifelse(is.na(USTEMM10),NA,as.numeric(USTEMM10) %in% stem_jobs),
         j11_stem=ifelse(is.na(VSTEMM10),NA,as.numeric(VSTEMM10) %in% stem_jobs)) %>%
  mutate(j07_stem=ifelse(j07_stem,1,ifelse(is.na(j07_stem),NA,0)),
         j08_stem=ifelse(j08_stem,1,ifelse(is.na(j08_stem),NA,0)),
         j09_stem=ifelse(j09_stem,1,ifelse(is.na(j09_stem),NA,0)),
         j10_stem=ifelse(j10_stem,1,ifelse(is.na(j10_stem),NA,0)),
         j11_stem=ifelse(j11_stem,1,ifelse(is.na(j11_stem),NA,0)))
glimpse(cleanData)

# Convert to long format
years_translator <- read.csv("Data/years_translator.csv",header=TRUE)
cohort_years <- years_translator %>%
  gather(key="cohort",value="life_stage",cohort2,cohort1) %>%
  separate(cohort,into=c("junk","cohort"),sep=-2) %>%
  select(-junk) %>%
  mutate(cohort=as.numeric(cohort))

longData <- cleanData %>%
  select(CASENUM,COHORT,GENDER,STRATA,
         WEIGHTP1,WEIGHTP2,WEIGHTP4,
         WEIGHTR,WEIGHTS,WEIGHTT,WEIGHTU,WEIGHTV,
         p1_stem,p2_stem,p4_stem,
         j07_stem,j08_stem,j09_stem,
         j10_stem,j11_stem) %>%
  unite(p1,WEIGHTP1,p1_stem) %>%
  unite(p2,WEIGHTP2,p2_stem) %>%
  unite(p4,WEIGHTP4,p4_stem) %>%
  unite(j07,WEIGHTR,j07_stem) %>%
  unite(j08,WEIGHTS,j08_stem) %>%
  unite(j09,WEIGHTT,j09_stem) %>%
  unite(j10,WEIGHTU,j10_stem) %>%
  unite(j11,WEIGHTV,j11_stem) %>%
  gather(key="life_stage",value="values",
         p1,p2,p4,j07,j08,j09,j10,j11) %>%
  separate(values,into=c("weight","engagement"),sep="_") %>%
  mutate_at(vars(weight,engagement),funs(as.numeric)) %>%
  mutate(cohort = as.numeric(COHORT)) %>%
  left_join(cohort_years,by=c("cohort"="cohort","life_stage"="life_stage"))

