library(tidyverse)

gdp_pc<-read.csv("ltu-migration/data/7a9b8d41-cb08-42e5-abea-e88a32a55ee1_Data.csv")
gdp_pc <- gdp_pc[c(1:11),] 
gdp_pc_long <- gdp_pc %>% 
  pivot_longer(cols = c(`X2004..YR2004.`:`X2023..YR2023.`))

gdp_ppp<-read.csv("ltu-migration/data/d7d9c94d-dbb3-479b-bb56-a742c22ec275_Data.csv") %>% 
  filter(Series.Name=='GDP per capita, PPP (constant 2021 international $)')
gdp_ppp <- gdp_ppp[,(3:24)] 
gdp_ppp_long <- gdp_ppp %>% 
  pivot_longer(cols = c(`X2014..YR2014.`:`X2007..YR2007.`)) %>% 
  mutate(name = str_remove_all(name,"X"), 
         name = str_sub(name,1,4)) %>% 
  rename(year = name) %>% 
  mutate(year = as.Date(year, '%Y')) %>% arrange(year)

index_df <- gdp_ppp %>% 
  mutate(across(`X2014..YR2014.`:`X2007..YR2007.`,
    ~ . / `X2015..YR2015.`)) %>%
  pivot_longer(cols = c(`X2014..YR2014.`:`X2007..YR2007.`)) %>% 
  mutate(name = str_remove_all(name,"X"), 
         name = str_sub(name,1,4)) %>% 
  rename(year = name) %>% 
  mutate(year = as.Date(year, '%Y'))

gg_ppp <- gdp_ppp_long %>% 
  filter(year >="2015-04-02") %>%
  filter(!(Country.Name %in% c("United States", "Norway","Poland", "Ireland", "France", "Finland", "Sweden"))) %>%
  ggplot(aes(x = year, y = value, group = Country.Name)) + 
  geom_line(aes(color = Country.Name, size = Country.Name == "Lithuania")) + 
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.5), guide = "none") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(
    color = "Country",
    title = "GDP per Capita, PPP (Constant 2021, international $)",
    subtitle = "By country",
    x = "",
    y = ""
  ) + ylim(0,80000) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "$"), 
                     limits = c(0,80000))
gg_ppp

index_ppp <- index_df %>% 
  filter(!(Country.Name %in% c("United States", "Ireland","Poland", "Norway", "France", "Finland", "Sweden"))) %>%
  filter(year >="2015-04-02") %>%
  ggplot(aes(x=year, y =value, group=Country.Name)) + 
  # geom_point(aes(color=Country.Name))+
  #geom_line(aes(color=Country.Name), size=1.5) + 
  geom_line(aes(color = Country.Name, size = Country.Name == "Lithuania")) + 
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.5), guide = "none") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(color = "Country",
       title = "GDP per Capita, PPP  (Constant 2021, international $)",
       subtitle = "Indexed to 2015, by country",
       x = "",
       y = "") 
index_ppp

