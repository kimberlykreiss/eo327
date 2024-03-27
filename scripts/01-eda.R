# I am interested in seeing profiles/employment history of people in Trenton NJ. 
library(DBI)
library(dbplyr)
library(tidyverse)
library(bgi)
library(openxlsx)
library(readxl)

con <- DBI::dbConnect(odbc::odbc(), Driver = "SnowflakeDSIIDriver", 
                      Server = "pca67849.snowflakecomputing.com", Uid = "KIM_KREISS", 
                      Pwd = Sys.getenv('pwd'))

postings_meta <- query_table_sf(con, "PDL_WORKING", "JAN_24", "LOCATION") %>%
  filter(LOCALITY=="trenton" & REGION == "new jersey") %>%
  collect()

trenton_ids <- query_table_sf(con, "PDL_WORKING", "JAN_24", "LOCATION") %>%
  filter(LOCALITY=="trenton" & REGION == "new jersey") 

trenton_educations <- query_table_sf(con, "PDL_WORKING", "JAN_24", "EDUCATION") %>%
  inner_join(trenton_ids, by = "ID") %>%
  collect()

mercer <- trenton_educations %>% 
  filter(SCHOOL_RAW=='mercer county community college') %>% 
  select(ID) %>% 
  distinct()

nj_postings <- query_table_sf(con, "EMSI_BURNING_GLASS_INSTITUTE", "US", "POSTINGS") %>% 
  filter(POSTED >= as.Date("2023-01-01") & POSTED <= as.Date("2023-12-31") & STATE==34) %>% 
  filter(COMPANY_IS_STAFFING ==FALSE ) %>% 
  collect()

################################################# 
# OPRA data analysis 
#################################################

opra_updated_jobs <- read.xlsx("kim-projects/data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx", sheet = 1, startRow = 2)
opra_all4yr_jobs <- read.xlsx("kim-projects/data/OPRA Request C25235 EO327 4 Yr Degree Titles.xlsx", sheet = 2, startRow = 2)

## What share of postings are from a government firm in NJ? 



## need to figure out how to more accurately identify government jobs in NJ 
govt_firms <- c("State Of New Jersey", "City Of", "County Of", "District", "Authority","Commission", 
                "New Jersey Courts", "New Jersey Economic Development Authority", "New Jersey Housing Mortgage Finance Agency", 
                "Nj Transit", "Non-Federal Agency", "City Hall", "Port Newark Container Terminal", "New Jersey Department of Health")
NJ_counties<-c('Atlantic County','Bergen County','Burlington','Camden County','CapeMay','Cumberland County','Essex',
'Gloucester','Hudson','Hunterdon','Mercer','Middlesex','Monmouth','Morris County',
'Ocean County','Passaic','Salem','Somerset County','Sussex','Union County','Warren')
school_related <- c("District", "Charter School", "Community College", "Board of Education", 
                    "Kean", "Monclair", "Ramapo", "New Jersey City University", "Rowan", "State Library", 
                    "Stockton", "The College of New Jersey", "Thomas Edison State University", "William Paterson", "Rutgers", 
                    "New Jersey Institute of Technology", "New Jersey Transit", "South Jersey Port Corporation", "State Colleges")

filter_phrases <- c(govt_firms, NJ_counties, school_related)

govt_firms_source <- nj_postings%>% 
  filter(Reduce(`|`, lapply(filter_phrases, function(phrase) grepl(phrase, COMPANY_NAME, ignore.case = TRUE)))) %>% 
  filter(!str_detect(COMPANY_NAME, "Head Start")) %>% 
  filter(!str_detect(COMPANY_NAME, "Adult Day Center")) %>% 
  filter(!str_detect(COMPANY_NAME, "Parts Authority")) %>% 
  filter(!str_detect(COMPANY_NAME, "Citco Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Rose's Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "U.S Environmental Protection Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "The Arc")) %>% 
  filter(!str_detect(COMPANY_NAME, "Arc Mercer"))  %>% 
  filter(!str_detect(COMPANY_NAME, "Sas Retail Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Farm Service Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Diversified Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Wawa Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Ames Community")) %>% 
  filter(!str_detect(COMPANY_NAME, "Hunterdon Medical Center")) 

posts_nj_govt <- posts_nj_2023 %>% 
  filter(Reduce(`|`, lapply(filter_phrases, function(phrase) grepl(phrase, COMPANY_NAME, ignore.case = TRUE)))) %>% 
  filter(!str_detect(COMPANY_NAME, "Head Start")) %>% 
  filter(!str_detect(COMPANY_NAME, "Adult Day Center")) %>% 
  filter(!str_detect(COMPANY_NAME, "Parts Authority")) %>% 
  filter(!str_detect(COMPANY_NAME, "Citco Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Rose's Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "U.S Environmental Protection Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "The Arc")) %>% 
  filter(!str_detect(COMPANY_NAME, "Arc Mercer"))  %>% 
  filter(!str_detect(COMPANY_NAME, "Sas Retail Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Farm Service Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Diversified Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Wawa Agency")) %>% 
  filter(!str_detect(COMPANY_NAME, "Ames Community")) %>% 
  filter(!str_detect(COMPANY_NAME, "Medical Center")) %>% 
  filter(!str_detect(COMPANY_NAME, "Foundation")) %>% 
  filter(!str_detect(COMPANY_NAME, "Care")) %>% 
  filter(!str_detect(COMPANY_NAME, "Acton Academy")) %>% 
  filter(!str_detect(COMPANY_NAME, "Urology")) %>% 
  filter(!str_detect(COMPANY_NAME, "Dental")) %>% 
  filter(!str_detect(COMPANY_NAME, "Animal")) %>% 
  filter(!str_detect(COMPANY_NAME, "Acelero")) %>% 
  filter(!str_detect(COMPANY_NAME, "Amsalem")) %>% 
  filter(!str_detect(COMPANY_NAME, "Dermatology")) %>% 
  filter(!str_detect(COMPANY_NAME, "Acelero")) %>% 
  filter(!str_detect(COMPANY_NAME, "Vascular")) %>% 
  filter(!str_detect(COMPANY_NAME, "Endodontics")) %>% 
  filter(!str_detect(COMPANY_NAME, ""))
  

firms <- posts_nj_govt %>% 
  select(COMPANY_NAME) %>% 
  unique()
  




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

