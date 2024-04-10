library(DBI)
library(dbplyr)
library(tidyverse)
library(bgi)
library(openxlsx)
library(readxl)

source("scripts/global-variables.R")

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SnowflakeDSIIDriver",
  Server = "pca67849.snowflakecomputing.com",
  Uid = "KIM_KREISS",
  Pwd = Sys.getenv('pwd')
)

nj_ids <- query_table_sf(con, "PDL_CLEAN", "V2", "ROOT_PERSON") %>%
  filter(BGI_STATE_NAME == "New Jersey") %>% 
  select(ID) 


nj_educ <-  query_table_sf(con, "PDL_CLEAN", "V2", "EDUCATION") %>% #query_table_sf(con, "TEMPORARY_DATA", "ARATHI", "EDUCATION_V2_040324") %>% 
  select(ID, BGI_DEGREE) %>% 
  inner_join(nj_ids, by = "ID") %>% 
  collect() %>% 
  unique()

nj_experience <- query_table_sf(con, "PDL_CLEAN", "V2", "EXPERIENCE") %>%
  select(ID, COMPANY_TYPE, COMPANY_NAME, BGI_ONET_NAME, START_DATE, END_DATE) %>% 
  filter(START_DATE >= as.Date("2022-01-01")) %>%
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
  filter(COMPANY_NAME %in% nj_govt_experience$COMPANY_NAME) %>%
  group_by(START_DATE) %>% 
  summarise(hires_n = n())
 

#### let's vizualize monthly hiring 
gg2 <- monthly_hiring %>% 
  ggplot(aes(x=as.Date(START_DATE), y=hires_n)) + 
  geom_point() + 
  geom_line() + 
  ggthemes::theme_clean() + 
  labs(x="", y ="") 
gg2

library(rdwd)
library(tsibble)
library(feasts)

monthly_hiring_ts <- tsibble::as_tsibble(monthly_hiring, index = START_DATE) %>%
  model(STL(hires_n ~ season(period = "year"), robust = TRUE)) |>
  components() |>
  autoplot()























monthly_hiring_ts<- ts(monthly_hiring, frequency = 12, start = c(2022, 1))
monthly_hiring_ts <- monthly_hiring_ts[,2]

plot.ts(monthly_hiring_ts)
hiring_comp <- decompose(monthly_hiring_ts)

plot(hiring_comp)

hiringSeasonAdj <- monthly_hiring_ts - hiring_comp$seasonal
plot.ts(hiringSeasonAdj)
