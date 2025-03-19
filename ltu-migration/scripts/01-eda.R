library(bgi)
library(tidyverse)
library(DBI)
library(readxl)


# Connect to BGI snowflake data
con <- DBI::dbConnect(odbc::odbc(), Driver = "SnowflakeDSIIDriver",
                      Server = "pca67849.snowflakecomputing.com", Uid = "KIM_KREISS",
                      Pwd = Sys.getenv('pwd'))

# cert <- query_table_sf(con, 'pdl_clean', 'v4', 'education') %>% 
#   filter(BGI_COUNTRY == "Lithuania") %>% 
#   collect()

lt_names_sql <- query_table_sf(con, "temporary_data", "kkreiss", "lithuanian_names")
lt_last_names_sql <- query_table_sf(con, "temporary_data", "kkreiss", "lithuanian_last_name_endings")
# 
# people <- query_table_sf(con, 'pdl_clean', 'v4', 'root_person') %>% 
#   inner_join(lt_last_names_sql, by = c("FIRST_NAME" = "lt_names")) %>% collect()

# Define common Lithuanian last name endings
lt_last_name_patterns <- c(
  "%ėnas", "%aitis", "%ūnas", "%auskas", "%iauskas", "%inskas", "%evičius", "%avičius",
  "%aitė", "%ytė", "%utė", "%iūtė", "%ienė", "%ckas"
)# "%as", "%is", "%ys", "%us",

# Construct SQL query condition for pattern matching
lt_last_name_conditions <- paste0("last_name LIKE '", lt_last_name_patterns, "'", collapse = " OR ")

# Query Snowflake database for matching last names
people <- query_table_sf(con, 'pdl_clean', 'v4', 'root_person') %>%
  filter(sql(paste0("(", lt_last_name_conditions, ")"))) %>%
  collect()
max_length <- 255  # Adjust based on Snowflake's schema
people <- people %>%
  mutate(across(where(is.character), ~substr(., 1, max_length)))

write_table_sf(people, con, "temporary_data", "kkreiss", "lt_ppl")




# try making this into extremely common LT first names. Like you are almost definitely lithuanian if you have one of these.

lithuanian_names <- c(
  # Male Names
  "jonas", "tomas", "marius", "darius", "mindaugas", "andrius", "giedrius", "vytautas", "gediminas", 
  "algirdas", "rokas", "lukas", "paulius", "arūnas", "saulius", "antanas", "domantas", "julius", 
  "kęstutis", "laurynas", "martynas", "rimantas", "simonas", "tautvydas", "vilius", "žygimantas", 
  "egidijus", "evaldas", "ramūnas", "rolandas", "ričardas", "deividas", "eimantas", "erikas", "gytis", 
  "henrikas", "justinas", "augustas", "aidas", "almantas", "benas", "dainius", "eldaras", "elvinas", 
  "emilis", "feliksas", "gintaras", "haroldas", "jokūbas", "kazimieras", "leonardas", "mantas", "modestas", 
  "nerijus", "orestas", "povilas", "pranas", "ramūnas", "robertas", "šarūnas", "sigitas", "stanislovas", 
  "stepas", "titas", "vaclovas", "valentas", "valdas", "viktoras", "vincentas", "virgilijus", "vladislovas", 
  "vygantas", "žilvinas", "edvinas", "vainius", "arnoldas", "dovydas", "gabrielius", "karolis", "naglis", 
  "petras", "rapolas", "tadas", "vytenis", "žygis", "benas", "vaidotas", "armandas", "džiugas", "orestas", 
  "gustas", "adomas", "marijus", "vytis", "gedvydas", "aivaras", "alvydas", "dalius", "raimondas", "vilius",
  
  # Female Names
  "ieva", "eglė", "austėja", "rūta", "gabija", "indrė", "justina", "monika", "lina", "vaida", "agnė", 
  "aistė", "birutė", "dovilė", "gintarė", "greta", "jūratė", "karolina", "kristina", "laimutė", "miglė", 
  "neringa", "raminta", "rasa", "rimantė", "sandra", "sigita", "ugnė", "viktorija", "živilė", "aurelija", 
  "beata", "brigita", "danielė", "diana", "edita", "evelina", "ilona", "inga", "jadvyga", "janina", "julija", 
  "kamila", "laima", "marija", "meda", "natalija", "ona", "renata", "rugilė", "salomėja", "samanta", 
  "simona", "skaistė", "sonata", "vaiva", "vilma", "žana", "joana", "giedrė", "kristė", "ema", "milda", 
  "airida", "silvija", "evelinija", "ala", "eulalija", "nida", "stefanija", "teresė", "zita", "albina", 
  "elenė", "filomena", "kotryna", "loreta", "marina", "otilija", "regina", "roma", "snieguolė", "virginija", 
  "zofija", "vitalija", "arūnė", "vesta", "goda", "tėja", "vakarė", "kamilė", "izabelė", "liucija"
) %>% as.data.frame() 
names(lithuanian_names) <- "lt_names"

lithuanian_last_name_endings <- c(
  # Masculine endings
  "-as", "-is", "-ys", "-us", "-ėnas", "-aitis", "-ūnas", "-auskas", "-iauskas", "-inskas", "-evičius", "-avičius",
  
  # Unmarried female endings
  "-aitė", "-ytė", "-utė", "-iūtė",
  
  # Married female endings
  "-ienė"
) %>% as.data.frame()
names(lithuanian_last_name_endings) <- "lt_names"


write_table_sf(lithuanian_names, con, "temporary_data", "kkreiss", "lithuanian_names")
write_table_sf(lithuanian_last_name_endings, con, "temporary_data", "kkreiss", "lithuanian_last_name_endings")

ltu_edu <- query_table_sf(con, 'pdl_clean', 'v4', 'education') %>% 
  rename_with(tolower) %>% 
  filter(is_higher_ed == 1 
   #       , bgi_school_confidence >= !!school_location_confidence 
         , !is.na(bgi_school_ipeds_name) 
         , !is.na(bgi_start_date)
         , bgi_country == "Lithuania") %>% 
  group_by(person_id) %>% 
  mutate(last_us_start_date = max(bgi_start_date)) %>% 
  filter(bgi_start_date == last_us_start_date) %>% 
  distinct(person_id, bgi_school_ipeds_name, last_us_start_date) %>% 
  collect()


