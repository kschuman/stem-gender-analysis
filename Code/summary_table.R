library(knitr)
finalSummary <- finalData %>%
  select(COHORT,GENDER,STEM,time,nkids) %>%
  group_by(COHORT,GENDER) %>%
  skim() %>%
  ungroup()

finalSummary %>%
  filter(type=="integer") %>%
  select(COHORT,GENDER,var,stat,value) %>%
  filter(stat %in% c("complete","missing","n","mean")) %>%
  spread(key=stat,value=value) %>%
  select(cohort=COHORT,gender=GENDER,column=var,total=n,complete=complete,missing=missing,mean=mean) %>%
  kable(format="latex")