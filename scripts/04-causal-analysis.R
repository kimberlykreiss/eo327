source("scripts/global-variables.R")

### event study. We can forecast or get a group to comapre to. 
# let's get seasonally adjusted for AA group into government 
# and non-AA group on the same graph 

# let's do a differences in differences: 

# there are two periods, before and after EO 327 goes into effect 
# there is a population n that is observed in both periods (workers in NJ in PDL )
# at time t=1 a fraction of the population (those without a 4-year degree) are treated 
# every unit has two different potential outcomes in every period: 
# an outcome under treatment and an outcome under control, 
# the parameter of interest is the average treatment effect on the treated 
# assumption 1: parallel trends 

# OK. We need data on hiring rates for people with a college degree or more 
# and people with less than college. 

# People who had less than college by Oct 2023
nj_educ <-  query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "V2_EDUCATION_APR24") %>% #query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "EDUCATION_V2_040324") %>% 
  select(ID, BGI_DEGREE,  BGI_DEGREE_MAX, BGI_SCHOOL_CONFIDENCE, DEGREES, END_DATE ) %>% 
  inner_join(nj_ids, by = "ID") %>% 
  collect() %>% 
  unique() %>% 
  filter(BGI_SCHOOL_CONFIDENCE >= 78)

nj_educ_wide <- nj_educ %>% 
  select(ID, BGI_DEGREE_MAX) %>% 
  mutate(BGI_DEGREE_MAX = if_else(is.na(BGI_DEGREE_MAX), "Not Listed", BGI_DEGREE_MAX), 
         n=1)%>%
  unique() %>%
  pivot_wider(names_from = BGI_DEGREE_MAX, values_from = n,values_fill=0)

nj_educ_aa <- nj_educ_wide %>% 
  filter(`Master's Degree`!= 1 & `Bachelor's Degree`!=1 & `Doctorate Degree`!=1)

nj_educ_ba_plus <- nj_educ_wide %>% 
  filter(`Master's Degree`== 1 | `Bachelor's Degree`==1 | `Doctorate Degree`==1)

nrow(nj_educ_ba_plus) + nrow(nj_educ_aa)
# nj_aa_most <-nj_educ %>% 
#   filter((!(BGI_DEGREE_MAX %in% c("Master's Degree", "Bachelor's Degree", "Doctorate Degree")) | (str_detect(DEGREES, "associate")))) %>% 
#   filter(!str_detect(DEGREES, "bachelor")) %>% 
#   filter(!str_detect(DEGREES, "doctor")) %>% 
#   filter(!str_detect(DEGREES, "master")) %>% 
#   filter(!str_detect(DEGREES, "medical"))

# nj_ba_at_least <- nj_educ %>% 
#   filter(!(ID %in% nj_aa_most$ID))





