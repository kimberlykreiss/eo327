library(DBI)
library(dbplyr)
library(tidyverse)
library(bgi)
library(openxlsx)
library(scales)
library(readxl)

source("scripts/global-variables.R")

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


nj_educ <-  query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "V2_EDUCATION_APR24") %>% #query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "EDUCATION_V2_040324") %>% 
  select(ID, BGI_DEGREE) %>% 
  inner_join(nj_ids, by = "ID") %>% 
  collect() %>% 
  unique()

nj_experience <- query_table_sf(con, "PDL_WORKING", "APR_24", "EXPERIENCE") %>%
  select(ID, COMPANY_TYPE, COMPANY_NAME, START_DATE, END_DATE) %>% 
  filter(START_DATE >= as.Date("2019-01-01")) %>%
  inner_join(nj_ids, by = "ID") %>% 
  collect()


nj_final <- inner_join(nj_educ, nj_experience, by = "ID") %>% 
  collect()

#################################################### 
# What types of skills are common to these roles? 
## Run code below before doing above analysis to get the govt jobs
#####################################################

opra_updated_jobs <-
  read.xlsx(
    "data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx",
    sheet = 1,
    startRow = 2
  )
opra_all4yr_jobs <-
  read.xlsx(
    "data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx",
    sheet = 2,
    startRow = 2
  )

nj_postings <-
  query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>%
  filter(POSTED >= as.Date("2022-01-01") & STATE == 34) %>%
  collect() %>%
  filter(COMPANY_IS_STAFFING == FALSE)

## filter down to just government jobs in NJ 
nj_postings <-
  query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>%
  filter(POSTED >= as.Date("2021-01-01") & STATE == 34) %>%
  filter(COMPANY_IS_STAFFING == FALSE) %>%
  select(ID, POSTED, STATE, COMPANY_NAME, COMPANY_RAW)%>%
  collect() 


filter_phrases <- c(govt_firms, school_related)

govt_firms_source <- nj_postings %>%
  mutate(COMPANY_NAME_AND_RAW = paste(COMPANY_NAME, COMPANY_RAW, sep="/")) %>% 
  filter(Reduce(`|`, lapply(filter_phrases, function(phrase)
    grepl(phrase, COMPANY_RAW, ignore.case = TRUE))) |
      (COMPANY_RAW %in% NJ_counties) |
      (COMPANY_RAW %in% NJ_towns) | (COMPANY_RAW %in% NJ_counties) | 
      (COMPANY_RAW %in% NJ_towns)) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Foundation")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "United States Department")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Sales Department")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Department Store")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Animal Clinic")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Animal Hospital")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Central Avenue Special Improvement")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Head Start")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Adult Day Center")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Parts Authority")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Citco Agency")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Rose's Agency")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "U.S Environmental Protection Agency")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "The Arc")) %>%
  filter(!str_detect(COMPANY_NAME_AND_RAW, "Arc Mercer"))

all_firms_nj <- nj_postings %>% 
  filter(POSTED >= as.Date("2023-01-01") & POSTED <= as.Date("2023-12-31")) %>% 
  select(COMPANY_NAME, COMPANY_RAW) %>% 
  distinct()
unique_gfs <- govt_firms_source %>%
  select(COMPANY_NAME, COMPANY_RAW) %>%
  distinct()

govt_firms_source_2023 <- govt_firms_source %>% 
  filter(POSTED >= as.Date("2023-01-01") & POSTED <= as.Date("2023-12-31"))
nj_postings_2023 <- nj_postings %>% 
  filter(POSTED >= as.Date("2023-01-01") & POSTED <= as.Date("2023-12-31")) 
nrow(govt_firms_source_2023) / nrow(nj_postings_2023)


pdl_nj_employers <- nj_experience %>% 
  select(COMPANY_NAME) %>% 
  distinct()

nj_govt_experience <- nj_experience %>% 
  filter(COMPANY_NAME %in% tolower(unique_gfs$COMPANY_NAME)) %>% 
  filter(COMPANY_NAME != "princeton university")

monthly_hiring <- nj_final %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
#  filter(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")
  # mutate(hires_n = if_else(START_DATE >= "2023-10-01", hires_n + 500, as.double(hires_n)),
  #        hires_n = if_else(START_DATE == "2023-09-01", hires_n+2000, as.double(hires_n)))
 

#### let's vizualize monthly hiring 
gg2 <- monthly_hiring %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "New Hires in the State of New Jersey") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue") #+ 
  # geom_vline(xintercept = as.Date("2022-10-01"), color="red", linetype=2) + 
  # geom_vline(xintercept = as.Date("2021-10-01"), color="red", linetype=2) + 
  # geom_vline(xintercept = as.Date("2020-10-01"), color="red", linetype=2) + 
  # geom_vline(xintercept = as.Date("2019-10-01"), color="red", linetype=2)
gg2


govt_monthly_hiring <- nj_final %>% 
  mutate(START_DATE=as.Date(START_DATE), 
         START_DATE= floor_date(START_DATE, unit="month")) %>%
    filter(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n()) %>% 
  ungroup() %>% 
  mutate(START_DATE=as.Date(START_DATE)) %>% 
  filter(START_DATE <= "2024-01-24")
# mutate(hires_n = if_else(START_DATE >= "2023-10-01", hires_n + 500, as.double(hires_n)),
#        hires_n = if_else(START_DATE == "2023-09-01", hires_n+2000, as.double(hires_n)))


#### let's vizualize monthly hiring 
gg2_govt <- govt_monthly_hiring %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  #geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "New Hires in Government New Jersey") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="blue") #+ 
# geom_vline(xintercept = as.Date("2022-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2021-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2020-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2019-10-01"), color="red", linetype=2)
gg2_govt

## seasonally adjusted normal hiring 
monthly_hiring_ts<- ts(monthly_hiring, frequency = 12, start = c(2019, 1))
monthly_hiring_ts <- monthly_hiring_ts[,2]

plot.ts(monthly_hiring_ts)
hiring_comp <- decompose(monthly_hiring_ts)
#plot(hiring_comp)

hiringSeasonAdj <- monthly_hiring_ts - hiring_comp$seasonal
plot.ts(hiringSeasonAdj)

hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = hiringSeasonAdj) %>% 
  left_join(monthly_hiring, by = c("year"="START_DATE")) 

gg2 <- hiringSeasonAdj_df %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
#  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in All NJ Jobs (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") 
gg2

## seasonally adjusted monthly hiring for govt jobs
monthly_hiring_ts<- ts(govt_monthly_hiring, frequency = 12, start = c(2019, 1))
monthly_hiring_ts <- monthly_hiring_ts[,2]

plot.ts(monthly_hiring_ts)
hiring_comp <- decompose(monthly_hiring_ts)
#plot(hiring_comp)

hiringSeasonAdj <- monthly_hiring_ts - hiring_comp$seasonal
plot.ts(hiringSeasonAdj)

hiringSeasonAdj_df <- data.frame(year = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "month"), season_adj_hires = hiringSeasonAdj) %>% 
  left_join(govt_monthly_hiring, by = c("year"="START_DATE")) 

gg2_govt <- hiringSeasonAdj_df %>% 
  pivot_longer(cols=c(season_adj_hires, hires_n), names_to = 'hires') %>%
  ggplot(aes(x=year, y=value, group=hires, color=hires)) + 
  #  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "Hiring in NJ Government Jobs (Seasonally Adjusted)") + 
  geom_vline(xintercept = as.Date("2023-10-01")) + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=c("grey", "blue"), name = "", labels = c("No Adjustment", "Seasonally-Adjusted"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + 
  ylim()
gg2_govt

######## JOLTS data ############ 

jolts_sa <- read.xlsx("data/jolts_nj_hires.xlsx") %>% 
  pivot_longer(names_to = "time", cols = c(Jan:Dec)) %>% 
  mutate(START_DATE = paste(time, Year, sep="-"), 
         START_DATE = as.Date(paste0("01-", START_DATE), format = "%d-%b-%Y")) %>% 
  select(START_DATE, hires_n = value) %>% 
  filter(!is.na(hires_n))

jolts <- read.xlsx("data/jolts_not_seasonally_adj.xlsx") %>% 
  pivot_longer(names_to = "time", cols = c(Jan:Dec)) %>% 
  mutate(START_DATE = paste(time, Year, sep="-"), 
         START_DATE = as.Date(paste0("01-", START_DATE), format = "%d-%b-%Y")) %>% 
  select(START_DATE, hires_n = value) %>% 
  filter(!is.na(hires_n))

jolts_gg2 <- jolts %>% 
  left_join(jolts_sa, by = c("START_DATE"), suffix = c("", "_seasAdj")) %>%
  filter(START_DATE >= "2019-01-01") %>%
  ggplot() + 
  #geom_point() + 
  geom_line(aes(x=as.Date(START_DATE), y=hires_n), color="grey") + 
  geom_line(aes(x=as.Date(START_DATE), y=hires_n_seasAdj), color="blue") +
  ggthemes::theme_clean() + 
  labs(x="", y ="", title = "New Hires in the State of New Jersey") + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") + 
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + 
  ylim(0, 400)#+ 
# geom_vline(xintercept = as.Date("2022-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2021-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2020-10-01"), color="red", linetype=2) + 
# geom_vline(xintercept = as.Date("2019-10-01"), color="red", linetype=2)
jolts_gg2











