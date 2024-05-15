source(paste0(here::here(),"/scripts/global-variables.R"))

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SnowflakeDSIIDriver",
  Server = "pca67849.snowflakecomputing.com",
  Uid = "KIM_KREISS",
  Pwd = Sys.getenv('pwd')
)

nj_ids <- query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "V2_ROOT_PERSON_APR24") %>%
  filter(BGI_STATE_NAME == "New Jersey") %>% 
  select(ID) 


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


unique_gfs_nj <- readRDS(paste0(here::here(),"/data/unique_gfs.rds"))
##### Now merge with nj_experience
nj_experience <- readRDS(paste0(here::here(),"/data/nj_experience.rds"))

aa_exp_nj <- nj_educ_aa %>% 
  inner_join(nj_experience, by ="ID")

AA_hires_into_govt_nj <- aa_exp_nj %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs_nj$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university") %>% 
  group_by(START_DATE) %>% 
  summarise(n=n())

nj_govt_experience <- nj_experience %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs_nj$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university")


ct_ids <- query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "V2_ROOT_PERSON_APR24") %>%
  filter(BGI_STATE_NAME == "Connecticut") %>% 
  select(ID) 


### event study. We can forecast or get a group to comapre to. 
# let's get seasonally adjusted for AA group into government 
# and non-AA group on the same graph 

# let's do a differences in differences: 

# there are two periods, before and after EO 327 goes into effect 
# there is a population n that is observed in both periods (workers in ct in PDL )
# at time t=1 a fraction of the population (those without a 4-year degree) are treated 
# every unit has two different potential outcomes in every period: 
# an outcome under treatment and an outcome under control, 
# the parameter of interest is the average treatment effect on the treated 
# assumption 1: parallel trends 

# OK. We need data on hiring rates for people with a college degree or more 
# and people with less than college. 

# People who had less than college by Oct 2023
ct_educ <-  query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "V2_EDUCATION_APR24") %>% #query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "EDUCATION_V2_040324") %>% 
  select(ID, BGI_DEGREE,  BGI_DEGREE_MAX, BGI_SCHOOL_CONFIDENCE, DEGREES, END_DATE ) %>% 
  inner_join(ct_ids, by = "ID") %>% 
  collect() %>% 
  unique() %>% 
  filter(BGI_SCHOOL_CONFIDENCE >= 78)

ct_educ_wide <- ct_educ %>% 
  select(ID, BGI_DEGREE_MAX) %>% 
  mutate(BGI_DEGREE_MAX = if_else(is.na(BGI_DEGREE_MAX), "Not Listed", BGI_DEGREE_MAX), 
         n=1)%>%
  unique() %>%
  pivot_wider(names_from = BGI_DEGREE_MAX, values_from = n,values_fill=0)

ct_educ_aa <- ct_educ_wide %>% 
  filter(`Master's Degree`!= 1 & `Bachelor's Degree`!=1 & `Doctorate Degree`!=1)

ct_educ_ba_plus <- ct_educ_wide %>% 
  filter(`Master's Degree`== 1 | `Bachelor's Degree`==1 | `Doctorate Degree`==1)

nrow(ct_educ_ba_plus) + nrow(ct_educ_aa)


unique_gfs_ct <- readRDS(paste0(here::here(),"/data/unique_gfs.rds"))
##### Now merge with ct_experience
ct_experience <- readRDS(paste0(here::here(),"/data/ct_experience.rds"))

aa_exp_ct <- ct_educ_aa %>% 
  inner_join(ct_experience, by ="ID")

AA_hires_into_govt_ct <- aa_exp_ct %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs_ct$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university") %>% 
  group_by(START_DATE) %>% 
  summarise(n=n())

ct_govt_experience <- ct_experience %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs_ct$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university")

###############################
#
###############################



################################
# plot CT and NJ on the same graph 
################################
govt_monthly_hiring_aa_nj <- aa_exp_nj %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  filter(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")

gg2_aa_govt_nj <- govt_monthly_hiring_aa_nj %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "NJ Gov't Hires for non-BA holders") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue")
gg2_aa_govt_nj

## seasonally adjusted normal hiring 
aa_monthly_hiring_ts_nj<- ts(govt_monthly_hiring_aa_nj, frequency = 12, start = c(2019, 1))
aa_monthly_hiring_ts_nj <- aa_monthly_hiring_ts_nj[,2]

plot.ts(aa_monthly_hiring_ts_nj)
aa_hiring_comp_nj <- decompose(aa_monthly_hiring_ts_nj, type="multiplicative")
#plot(hiring_comp)

aa_hiringSeasonAdj_nj <- aa_monthly_hiring_ts_nj/aa_hiring_comp_nj$seasonal
plot.ts(aa_hiringSeasonAdj_nj)

aa_hiringSeasonAdj_df_nj <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = aa_hiringSeasonAdj_nj) %>% 
  left_join(govt_monthly_hiring_aa_nj, by = c("year"="START_DATE")) 


gg2_govt_aa_nj <- aa_hiringSeasonAdj_df_nj %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "NJ Gov't Hires for non-BA holders (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") 
gg2_govt_aa_nj

################### 
# CT 
##################
govt_monthly_hiring_aa_ct <- aa_exp_ct %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  filter(COMPANY_NAME %in% ct_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")

gg2_aa_govt_ct <- govt_monthly_hiring_aa_ct %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "CT Gov't Hires for non-BA holders") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue")
gg2_aa_govt_ct

## seasonally adjusted normal hiring 
aa_monthly_hiring_ts_ct<- ts(govt_monthly_hiring_aa_ct, frequency = 12, start = c(2019, 1))
aa_monthly_hiring_ts_ct <- aa_monthly_hiring_ts_ct[,2]

plot.ts(aa_monthly_hiring_ts_ct)
aa_hiring_comp_ct <- decompose(aa_monthly_hiring_ts_ct, type="multiplicative")
#plot(hiring_comp)

aa_hiringSeasonAdj_ct <- aa_monthly_hiring_ts_ct/aa_hiring_comp_ct$seasonal
plot.ts(aa_hiringSeasonAdj_ct)

aa_hiringSeasonAdj_df_ct <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = aa_hiringSeasonAdj_ct) %>% 
  left_join(govt_monthly_hiring_aa_ct, by = c("year"="START_DATE")) 


gg2_govt_aa_ct <- aa_hiringSeasonAdj_df_ct %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "CT Gov't Hires for non-BA holders (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") 
gg2_govt_aa_ct




######################
# Together 
######################

all_govt_hiring <- full_join(govt_monthly_hiring_aa_nj, govt_monthly_hiring_aa_ct, by = "START_DATE", 
                             suffix = c("_nj", "_ct"))


gg2_all_govt <- all_govt_hiring %>% 
  #filter(START_DATE >= "2022-01-01") %>%
  pivot_longer(cols=c(hires_n_nj,hires_n_ct), names_to = 'hires') %>%
  ggplot(aes(x=START_DATE, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in NJ vs CT Gov't Jobs for non-BA holders") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("orange", "grey"), name = "", labels = c("CT", "NJ")) +
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") #+geom_smooth()
gg2_all_govt



all_hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), aa_nj_season_adj_hires = aa_hiringSeasonAdj_nj) %>% 
  left_join(data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), aa_ct_season_adj_hires = aa_hiringSeasonAdj_ct), by = 'year') 


gg2_govt_sa <- all_hiringSeasonAdj_df %>% 
 # filter(year >= "2022-01-01") %>% 
  pivot_longer(cols=c(aa_nj_season_adj_hires, aa_ct_season_adj_hires), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in CT vs NJ for non-BA holders (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("orange", "grey"), name = "", labels = c("CT", "NJ"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") #+geom_smooth()
gg2_govt_sa

#########################
# Regression 
#########################

government_hires <- all_govt_hiring %>% 
  mutate(month = 1:n()) %>% 
  pivot_longer(cols = c(hires_n_nj, hires_n_ct), names_to = "state", values_to = "hires") %>%
  mutate(after_policy = as.numeric(START_DATE >= "2023-10-01"), 
         treated = if_else(state == 'hires_n_nj', 1, 0))

did_model <- lm(hires ~  treated * month, data = government_hires)
summary(did_model)

did_model2 <- lm(hires ~  treated + after_policy + treated*after_policy, data = government_hires)
summary(did_model2)

government_hires_sa <- all_hiringSeasonAdj_df %>% 
  mutate(month = 1:n()) %>% 
  pivot_longer(cols = c(aa_nj_season_adj_hires, aa_ct_season_adj_hires), names_to = "state", values_to = "hires") %>%
  mutate(after_policy = as.numeric(year >= "2023-10-01"), 
         treated = if_else(state == 'aa_nj_season_adj_hires', 1, 0))

did_model_sa <- lm(hires ~  treated * month, data = government_hires_sa)
summary(did_model_sa)

did_model_sa2 <- lm(hires ~  treated + after_policy + treated*after_policy, data = government_hires_sa)
summary(did_model_sa2)















