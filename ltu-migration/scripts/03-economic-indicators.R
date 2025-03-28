library(tidyverse)

gdp_pc<-read.csv("ltu-migration/data/7a9b8d41-cb08-42e5-abea-e88a32a55ee1_Data.csv")
gdp_pc <- gdp_pc[c(1:11),] 
gdp_pc_long <- gdp_pc %>% 
  pivot_longer(cols = c(`X2004..YR2004.`:`X2023..YR2023.`))

gdp_ppp<-read.csv("ltu-migration/data/53db6c18-750f-4b7f-b002-0210720fbba7_Data.csv")
gdp_ppp <- gdp_ppp[c(1:11),] 
gdp_ppp_long <- gdp_ppp %>% 
  pivot_longer(cols = c(`X2004..YR2004.`:`X2023..YR2023.`))

gg_ppp <- gdp_ppp_long %>% 
  filter(!(Country.Name %in% c("United States", "Estonia", "Latvia", "France", "Finland"))) %>%
  ggplot(aes(x=name, y =value, group=Country.Name)) + 
 # geom_point(aes(color=Country.Name))+
  geom_line(aes(color=Country.Name), size=2) + 
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(color = "Country")
gg_ppp
