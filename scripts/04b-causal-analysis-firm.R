source("scripts/global-variables.R")


### event study. We can forecast or get a group to comapre to. 
# let's get seasonally adjusted for AA group into government 
# and non-AA group on the same graph 

# let's do a differences in differences: 

# there are two periods, before and after EO 327 goes into effect 
# there is a population n that is observed in both periods (firms in NJ with an AA in PDL )
# at time t=1 a fraction of the population (government firms) are treated 
# every unit has two different potential outcomes in every period: 
# an outcome under treatment and an outcome under control, 
# the parameter of interest is the average treatment effect on the treated 
# assumption 1: parallel trends 

# OK. We need data on hiring rates for AA workers in government firms and non-government firms 



nj_educ_wide <- readRDS("data/nj_educ_wide.rds")

aa_max_ids <- nj_educ_wide %>% 
  filter(`Master's Degree`!= 1 & `Bachelor's Degree`!=1 & `Doctorate Degree`!=1)
  
nj_experience <- readRDS("data/nj_experience.rds")
unique_gfs <- readRDS("data/unique_gfs.rds")
nj_govt_experience <- nj_experience %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university")

nj_non_govt_experience <- nj_experience %>% 
  filter(!(COMPANY_NAME %in% nj_govt_experience$ID))

# number of hires for government firms by month 
monthly_govt_hiring <- nj_govt_experience %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  group_by(START_DATE) %>% 
  summarise(govt_hiring_n=n())
# number of hires for non-government firms by month 
monthly_non_govt_hiring <- nj_non_govt_experience %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  group_by(START_DATE) %>% 
  summarise(non_govt_hiring_n=n())

aa_experience <- aa_max_ids %>% 
  inner_join(nj_experience, by = "ID")

################################## 
# ALL ASSOCIATES DEGREE HIRING
##################################

aa_all_monthly <-  aa_experience %>% 
  mutate(START_DATE=as.Date(START_DATE), 
                           START_DATE= floor_date(START_DATE, unit="month")) %>%
  group_by(START_DATE) %>%
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")

gg2_aa_all <- aa_all_monthly %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "All hiring for people without AA") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue")
gg2_aa_all


## seasonally adjusted normal hiring 
aa_monthly_hiring_ts<- ts(aa_all_monthly, frequency = 12, start = c(2019, 1))
aa_monthly_hiring_ts <- aa_monthly_hiring_ts[,2]

plot.ts(aa_monthly_hiring_ts)
aa_hiring_comp <- decompose(aa_monthly_hiring_ts, type="multiplicative")
#plot(hiring_comp)

aa_hiringSeasonAdj <- aa_monthly_hiring_ts/aa_hiring_comp$seasonal
plot.ts(aa_hiringSeasonAdj)

aa_hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = aa_hiringSeasonAdj) %>% 
  left_join(aa_all_monthly, by = c("year"="START_DATE")) 

gg2_all_aa <- aa_hiringSeasonAdj_df %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in All Jobs for AA or Less (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") 
gg2_all_aa

################################## 
# AA Hires into Govt 
##################################

aa_govt_hires<- aa_experience %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  filter(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")


gg2_aa_govt <- aa_govt_hires %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "NJ Government Hires w/ AA at most") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue")
gg2_aa_govt


## seasonally adjusted normal hiring 
aa_govt_monthly_hiring_ts<- ts(aa_govt_hires, frequency = 12, start = c(2019, 1))
aa_govt_monthly_hiring_ts <- aa_govt_monthly_hiring_ts[,2]

plot.ts(aa_govt_monthly_hiring_ts)
aa_govt_hiring_comp <- decompose(aa_govt_monthly_hiring_ts, type="multiplicative")
#plot(hiring_comp)

aa_govt_hiringSeasonAdj <- aa_govt_monthly_hiring_ts/aa_govt_hiring_comp$seasonal
plot.ts(aa_hiringSeasonAdj)

aa_govt_hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = aa_govt_hiringSeasonAdj) %>% 
  left_join(aa_govt_hires, by = c("year"="START_DATE")) 

gg2_govt_aa <- aa_govt_hiringSeasonAdj_df %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in Govt Jobs for AA or Less (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") 
gg2_govt_aa

######################################## 
 # 


aa_non_govt_hires<- aa_experience %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
  filter(!(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME)) %>%
  group_by(START_DATE) %>%
  summarise(hires_n = n()) %>%
  ungroup() %>%
  mutate(START_DATE=as.Date(START_DATE)) %>%
  filter(START_DATE <= "2024-01-24") 


gg2_aa_non_govt <- aa_non_govt_hires %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "NJ Non-Government Hires w/ AA at most") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue") + 
  geom_smooth()
gg2_aa_non_govt

## seasonally adjusted normal hiring 
aa_non_govt_monthly_hiring_ts<- ts(aa_non_govt_hires, frequency = 12, start = c(2019, 1))
aa_non_govt_monthly_hiring_ts <- aa_non_govt_monthly_hiring_ts[,2]

plot.ts(aa_non_govt_monthly_hiring_ts)
aa_non_govt_hiring_comp <- decompose(aa_non_govt_monthly_hiring_ts, type="multiplicative")
#plot(hiring_comp)

aa_non_govt_hiringSeasonAdj <- aa_non_govt_monthly_hiring_ts/aa_non_govt_hiring_comp$seasonal
plot.ts(aa_hiringSeasonAdj)

aa_non_govt_hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = aa_non_govt_hiringSeasonAdj) %>% 
  left_join(aa_non_govt_hires, by = c("year"="START_DATE")) 

gg2_non_govt_aa <- aa_non_govt_hiringSeasonAdj_df %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in non_govt Jobs for AA or Less (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + 
  geom_smooth()
gg2_non_govt_aa

#######################################################################
# Let's get government hiring and non-government hiring on the same GRAPH 
########################################################################
hiring_by_firm <- full_join(aa_govt_hires, aa_non_govt_hires, by = "START_DATE", suffix = c("_govt", "_non_govt")) %>% 
   left_join(monthly_govt_hiring, by = "START_DATE") %>% 
  left_join(monthly_non_govt_hiring, by = "START_DATE") %>%
   mutate(share_govt = hires_n_govt/govt_hiring_n,
         share_non_govt = hires_n_non_govt/non_govt_hiring_n)

gg2_hiring_by_firm <- hiring_by_firm %>% 
  pivot_longer(cols=c(hires_n_govt, hires_n_non_govt), names_to = 'hires') %>%
#  pivot_longer(cols=c(share_govt, share_non_govt), names_to = 'hires') %>%
  filter(START_DATE >= "2022-01-01") %>% 
  ggplot(aes(x=START_DATE, y=value, group=hires, color=hires)) + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring by firm") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("orange", "grey"), name = "", labels = c("Government", "Non_Government"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") +geom_smooth(se=T)
gg2_hiring_by_firm


#################### 
# Overall SA 
####################
hiring_by_firm_sa <- full_join(aa_govt_hiringSeasonAdj_df %>% 
                                 select(-hires_n), 
                               aa_non_govt_hiringSeasonAdj_df %>% 
                                 select(-hires_n), by = "year", suffix = c("_govt", "_non_govt")) #%>% 
# left_join(aa_all_monthly, by = "START_DATE") %>% 
# mutate(share_govt = hires_n_govt/hires_n,
#        share_non_govt = hires_n_non_govt/hires_n)

gg2_hiring_by_firm <- hiring_by_firm_sa %>% 
  pivot_longer(cols=c(season_adj_hires_govt, season_adj_hires_non_govt), names_to = 'hires') %>%
  filter(hires == "season_adj_hires_non_govt") %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring by firm") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("orange", "grey"), name = "", labels = c("Government", "Non_Government"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + geom_smooth()
gg2_hiring_by_firm
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
