library(lubridate)
library(scales)

source("scripts/global-variables.R")

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SnowflakeDSIIDriver",
  Server = "pca67849.snowflakecomputing.com",
  Uid = "KIM_KREISS",
  Pwd = Sys.getenv('pwd')
)

nj_postings <- read_rds("data/nj_postings.rds") 

nj_postings_ <- nj_postings %>% 
  mutate(aa_or_less = if_else(MIN_EDULEVELS %in% c(0,1,99), "AA or Less", "BA+"), 
         POST_DATE= as.Date(floor_date(POSTED, unit="month"))) %>% 
 # filter((COMPANY_NAME %in% unique_gfs$COMPANY_NAME)) %>%
  filter(POSTED < "2024-04-01")

monthly_posts <- nj_postings_ %>% 
  group_by(POST_DATE) %>% 
  summarise(total_n=n())

gg2_posts <- monthly_posts %>% 
 # filter(POST_DATE >= "2021-01-01") %>%
  ggplot(aes(x=as.Date(POST_DATE), y=total_n)) + 
  geom_line() +
  ggthemes::theme_clean() + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")

gg2_posts

monthly_posts_educ <- nj_postings_ %>% 
  group_by(POST_DATE, aa_or_less) %>% 
  summarise(n=n()) %>% 
  left_join(monthly_posts, by = "POST_DATE") %>% 
  ungroup() %>% 
  mutate(share = n/total_n)

gg_educ_posts <- monthly_posts_educ %>% 
  filter(POST_DATE >= "2021-01-01") %>%
  ggplot(aes(x=as.Date(POST_DATE), y=n, color=aa_or_less)) + 
  geom_line() +
  ggthemes::theme_clean() + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values=c("orange", "grey"), name = "") + # , labels = c("BA+", "AA or less"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + 
  labs(x="", y="", title = "All NJ postings where minimum education level 
       is not specified or at most AA") 
gg_educ_posts


####################################
# NON-GOVERNMENT VS GOVERNMENT AA POSTINGS
####################################

nj_postings_govt_aa <- nj_postings %>% 
  mutate(aa_or_less = if_else(MIN_EDULEVELS %in% c(0,1,99), "AA or Less", "BA+"), 
         POST_DATE= as.Date(floor_date(POSTED, unit="month"))) %>% 
   filter((COMPANY_NAME %in% unique_gfs$COMPANY_NAME)) %>%
  filter(aa_or_less == "AA or Less") %>%
  filter(POSTED < "2024-04-01") %>% 
  group_by(POST_DATE) %>% 
  summarise(n_govt_aa=n())

nj_postings_nongovt_aa <- nj_postings %>% 
  mutate(aa_or_less = if_else(MIN_EDULEVELS %in% c(0,1,99), "AA or Less", "BA+"), 
         POST_DATE= as.Date(floor_date(POSTED, unit="month"))) %>% 
  filter(!(COMPANY_NAME %in% unique_gfs$COMPANY_NAME)) %>%
  filter(aa_or_less == "AA or Less") %>%
  filter(POSTED < "2024-04-01")%>% 
  group_by(POST_DATE) %>% 
  summarise(n_nongovt_aa=n())

nj_postings_aa_only <- nj_postings_govt_aa %>% 
  left_join(nj_postings_nongovt_aa, by = "POST_DATE")


gg_educ_posts <- nj_postings_aa_only %>% 
  filter(POST_DATE >= "2021-01-01") %>%
  ggplot() + 
  geom_line(aes(x=as.Date(POST_DATE), y=n_govt_aa), color='orange') +
  geom_line(aes(x=as.Date(POST_DATE), y=n_nongovt_aa), color="grey") +
  ggthemes::theme_clean() + 
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom") +
  scale_color_manual(values=c("orange", "grey"), name = "") + # , labels = c("BA+", "AA or less"))+
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  geom_vline(xintercept = as.Date("2023-10-01"), color="red") +
  geom_vline(xintercept = as.Date("2023-04-01"), color="navyblue") + 
  geom_vline(xintercept = as.Date("2023-12-01"), color="grey", linetype=2) + 
  labs(x="", y="", title = "All NJ postings where minimum education level 
       is not specified or at most AA") 
gg_educ_posts


