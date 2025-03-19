library(bgi)
library(tidyverse)
library(DBI)
library(readxl)

#### 
# we want to look at migration trends to lithuania 
# look at people who come back, vs who don't 
# gender lens 
# ukrainian refugees also (they work in ukraine at some point)
# take a look at WIL 
# lithuanian names, lithuanian universities 
# people who did not live in lithuania last year, but now they do 


# Connect to BGI snowflake data
con <- DBI::dbConnect(odbc::odbc(), Driver = "SnowflakeDSIIDriver",
                      Server = "pca67849.snowflakecomputing.com", Uid = "KIM_KREISS",
                      Pwd = Sys.getenv('pwd'))
lt_ids <-  query_table_sf(con, "temporary_data", "kkreiss", "lt_ppl") %>% 
  distinct(PERSON_ID)

lt_ppl <- query_table_sf(con, "temporary_data", "kkreiss", "lt_ppl") %>% 
  collect() %>% 
  mutate(lith_language = case_when(str_detect(LANGUAGE_LIST,"lithuanian") ~ 1,
                                   !(str_detect(LANGUAGE_LIST, "lithuanian")) ~ 0,
                                   is.na(LANGUAGE_LIST) ~ NA_real_))

table(lt_ppl$LOCATION_COUNTRY, useNA = "ifany")
table(lt_ppl$JOB_COMPANY_LOCATION_COUNTRY, useNA="ifany")
table(lt_ppl$LANGUAGE_LIST)
table(lt_ppl$lith_language, useNA = "ifany") %>% prop.table()

ltu_edu <- query_table_sf(con, 'pdl_clean', 'v4', 'education') %>% 
  inner_join(lt_ids, by = 'PERSON_ID') %>% 
  collect()

table(ltu_edu$BGI_COUNTRY)

ltu_exp <- query_table_sf(con, 'pdl_clean', 'v4', 'experience') %>% 
  inner_join(lt_ids, by = 'PERSON_ID') %>% 
  collect()


