# I am interested in seeing profiles/employment history of people in Trenton NJ.
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

# postings_meta <-
#   query_table_sf(con, "PDL_WORKING", "JAN_24", "LOCATION") %>%
#   filter(LOCALITY == "trenton" & REGION == "new jersey") %>%
#   collect()
# 
# trenton_ids <-
#   query_table_sf(con, "PDL_WORKING", "JAN_24", "LOCATION") %>%
#   filter(LOCALITY == "trenton" & REGION == "new jersey")
# 
# trenton_educations <-
#   query_table_sf(con, "PDL_WORKING", "JAN_24", "EDUCATION") %>%
#   inner_join(trenton_ids, by = "ID") %>%
#   collect()
# 
# mercer <- trenton_educations %>%
#   filter(SCHOOL_RAW == 'mercer county community college') %>%
#   select(ID) %>%
#   distinct()

nj_postings <-
  query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>%
  filter(POSTED >= as.Date("2023-01-01") &
           POSTED <= as.Date("2023-12-31") & STATE == 34) %>%
  collect() %>%
  filter(COMPANY_IS_STAFFING == FALSE)

staffing_nj_postings <-
  query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>%
  filter(POSTED >= as.Date("2023-01-01") &
           POSTED <= as.Date("2023-12-31") & STATE == 34) %>%
  collect() %>%
  filter(COMPANY_IS_STAFFING == TRUE)

unclassified_nj_postings <- nj_postings %>% 
  filter(COMPANY_NAME == "Unclassified") 
compoanies_unclas <- unclassified_nj_postings %>% 
  select(COMPANY_RAW) %>% 
  unique()

#################################################
# OPRA data analysis
#################################################

opra_updated_jobs <-
  read.xlsx(
    "kim-projects/data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx",
    sheet = 1,
    startRow = 2
  )
opra_all4yr_jobs <-
  read.xlsx(
    "kim-projects/data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx",
    sheet = 2,
    startRow = 2
  )

## What share of postings are from a government firm in NJ?
nj_postings_firms <- nj_postings %>%
  select(COMPANY_NAME) %>%
  distinct()



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


unique_gfs <- govt_firms_source %>%
  select(COMPANY_NAME, COMPANY_RAW) %>%
  distinct()
nrow(govt_firms_source) / nrow(nj_postings)




# 
# 
# 
# filter(!str_detect(COMPANY_NAME, "Head Start")) %>%
#   filter(!str_detect(COMPANY_NAME, "Adult Day Center")) %>%
#   filter(!str_detect(COMPANY_NAME, "Parts Authority")) %>%
#   filter(!str_detect(COMPANY_NAME, "Citco Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "Rose's Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "U.S Environmental Protection Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "The Arc")) %>%
#   filter(!str_detect(COMPANY_NAME, "Arc Mercer"))  %>%
#   filter(!str_detect(COMPANY_NAME, "Sas Retail Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "Farm Service Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "Diversified Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "Wawa Agency")) %>%
#   filter(!str_detect(COMPANY_NAME, "Ames Community")) %>%
#   filter(!str_detect(COMPANY_NAME, "Medical Center")) %>%
#   filter(!str_detect(COMPANY_NAME, "Care")) %>%
#   filter(!str_detect(COMPANY_NAME, "Acton Academy")) %>%
#   filter(!str_detect(COMPANY_NAME, "Urology")) %>%
#   filter(!str_detect(COMPANY_NAME, "Dental")) %>%
#   filter(!str_detect(COMPANY_NAME, "Animal")) %>%
#   filter(!str_detect(COMPANY_NAME, "Acelero")) %>%
#   filter(!str_detect(COMPANY_NAME, "Amsalem")) %>%
#   filter(!str_detect(COMPANY_NAME, "Dermatology")) %>%
#   filter(!str_detect(COMPANY_NAME, "Acelero")) %>%
#   filter(!str_detect(COMPANY_NAME, "Vascular")) %>%
#   filter(!str_detect(COMPANY_NAME, "Endodontics")) %>%
#   filter(!str_detect(COMPANY_NAME, "Mental Health")) %>%
#   filter(!str_detect(COMPANY_NAME, "Healthcare")) %>%
#   filter(!str_detect(COMPANY_NAME, "Digestive")) %>%
#   filter(!str_detect(COMPANY_NAME, "Pediatrics")) %>%
#   filter(!str_detect(COMPANY_NAME, "Hunterdon Helpline")) %>%
#   filter(!str_detect(COMPANY_NAME, "Hunterdon Hospital")) %>%
#   filter(!str_detect(COMPANY_NAME, "Hudson Group")) %>%
#   filter(!str_detect(COMPANY_NAME, "Masonic Village")) %>%
#   filter(!str_detect(COMPANY_NAME, "Tender Smiles")) %>%
#   filter(!str_detect(COMPANY_NAME, "Ramapo Communication Corp.")) %>%
#   filter(!str_detect(COMPANY_NAME, "Apparel")) %>%
#   filter(!str_detect(COMPANY_NAME, "Chiropractic")) %>%
#   filter(!str_detect(COMPANY_NAME, "Cardiology")) %>%
#   filter(!str_detect(COMPANY_NAME, "Swim School")) %>%
#   filter(!str_detect(COMPANY_NAME, "Salem Press")) %>%
#   filter(!str_detect(COMPANY_NAME, "Monmouth Telecom")) %>%
#   filter(!str_detect(COMPANY_NAME, "Veterinary Center Of Morris County")) %>%
#   filter(!str_detect(COMPANY_NAME, "Sussexpress")) %>%
#   filter(!str_detect(COMPANY_NAME, "Brewing Co")) %>%
#   filter(!str_detect(COMPANY_NAME, "Commercial District Services")) %>%
#   filter(!str_detect(COMPANY_NAME, "Welders"))
# 


# posts_nj_govt_2023 <- query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>%
#   filter(sql("ARRAY_CONTAINS('Government'::VARIANT, SOURCE_TYPES)")) %>% # Looking at just postings  featured on a company website
#   #filter(sql("ARRAY_CONTAINS('Company'::VARIANT, SOURCE_TYPES)"), DUPLICATES == 0) %>% ... #looking at postings JUST on a company website
#   #select(ID, POSTED, SOURCE_TYPES,COMPANY_NAME, SOC_5, SOC_5_NAME, SOC_2, SOC_2_NAME, NAICS2, NAICS2_NAME, STATE) %>%
#   filter(POSTED >= as.Date("2023-01-01") & POSTED <= as.Date("2023-12-31"), STATE==34) %>%
#   mutate(year = lubridate::year(POSTED))%>%
#   collect() %>%
#   filter(COMPANY_IS_STAFFING ==FALSE )
#
# firms_nj <- posts_nj_govt_2023 %>%
#   select(COMPANY_NAME) %>%
#   unique()

# some weird ones that would not count:
#Allegheny Valley School
#Adult Day Center Of Somerset County
#Ames Community School District
# British Swim School
# Cumberland Dairy
