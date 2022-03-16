
EU = merge(pop_size, deaths, by="GEO",all = TRUE)
EU = merge(EU, Life_expectancy, by.x="GEO", by.y= "GEO/TIME", all = TRUE)

setwd("C:/Users/user/Downloads/")
getwd()
write.csv(x= eu_map_plot ,
          file="FINAL_eu_map_plot.csv")
#____________________________________
#the EU map preparation 
install.packages("eurostat")

eu_map <- eurostat::get_eurostat_geospatial(resolution = "60") %>% 
  filter(LEVL_CODE == 0)
#data preparation for merge

install.packages("countrycode")
library("countrycode")
codelist <-codelist 
EU = merge(EU, codelist, by.x = "GEO", by.y = "country.name.en", all.x = TRUE)
EU$geo = EU$eurostat.x
#ready for merging
library("dplyr")
library(sf)
eu_map_plot <- left_join(x=eu_map, y=EU, by = "geo")
#_____________________________________
#Plotting the EU map 
library(ggplot2)
library("sf")

#General

#Population size
eu_map_plot$Population_K= eu_map_plot$Population/1000

ggplot(eu_map_plot) +
  geom_sf(aes(fill = Population_K), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Population size in Europe",
    subtitle = "in 1,000 persons",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) 
#Mediterranean countries (France, Spain, Italy) as well as the United Kingdom have higher population than other countries in Europe.
#Turkey has the largest population. 

#Births & deaths

#Calculation of crude rates 
#crude birth rate (CBR)
eu_map_plot$CBR = eu_map_plot$`Live-births` / eu_map_plot$Population *1000 
#crude death rate (CDR)
eu_map_plot$CDR = eu_map_plot$Deaths / eu_map_plot$Population *1000 

#Births 
ggplot(eu_map_plot) +
  geom_sf(aes(fill = CBR), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Crude birth rate in Europe",
    subtitle = "per 1000 people in the population",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
#South Europe has on average lower CBR than the rest of Europe. Turkey has the highest CBR.

#Deaths
ggplot(eu_map_plot) +
  geom_sf(aes(fill = CDR), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Crude death rate in Europe",
    subtitle = "per 1000 people in the population",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
#CDR is high in Baltics and Hungary,Romania, Bulgaria, Serbia. The highest CDR is in Bulgaria and Latvia. The lowest CDR is in Turkey

#Health
#Life expectancy 
ggplot(eu_map_plot) +
  geom_sf(aes(fill = Life_expectancy), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Life expectancy at birth in Europe",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())
#Life expectancy is overall high (80+ years) within whole Europe. Lower life expectancy is within the same group of countries with high CDR.

#Economy
#Unemployment

ggplot(eu_map_plot) +
  geom_sf(aes(fill = Unemp_perc_l_force), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Unemployment in Europe",
    subtitle = "as a percentage of the labor force",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())

#Inequality
eu_map_plot$Income_ratio=as.numeric(eu_map_plot$"Income-ratio")

ggplot(eu_map_plot) +
  geom_sf(aes(fill = Income_ratio), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Inequality in Europe",
    subtitle = "Income quintile share ratio S80/S20 for disposable income",
    caption = "Source: EU-SILC survey, Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())

#Marriages & Divorces

#Marriages
eu_map_plot$Crude_marriage_rate=as.numeric(eu_map_plot$Crude_marriage_rate)
ggplot(eu_map_plot) +
  geom_sf(aes(fill = Crude_marriage_rate), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Crude marriage rate in Europe",
    subtitle = "Marriages per 1,000 population",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())

#Divorces
ggplot(eu_map_plot) +
  geom_sf(aes(fill = Crude_divorce_rate), color= "grey50", size = 0.1) +
  geom_sf_text(aes(label = GEO))+
  coord_sf(xlim=c(-12,44), ylim=c(35,70)) +
  scale_fill_viridis_c(na.value = "white", direction = -1, name = NULL) +
  labs(
    title = "Crude divorce rate in Europe",
    subtitle = "Divorces per 1,000 population",
    caption = "Source: Eurostat 2018"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())

             