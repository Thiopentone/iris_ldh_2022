## POPULATION OFFSETS SCRIPT ##

# ONS data set (available here: https://www.ons.gov.uk/timeseriestool?topic=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries)
uk<- read_csv("Data Sets/uk_pop.csv")

uk<- uk %>%
  filter(Title==2018 | Title==2019 | Title== 2020 | Title==2021)

colnames(uk)<-c("year", "ENPOP",	"GBPOP",	"EWPOP",	"WAPOP",	"UKPOP",	"NIPOP",	"SCPOP")

uk<- data.frame(uk[, c(1,2,5,7,8)])

colnames(uk)<- c("year", "England", "Wales", "Northern Ireland", "Scotland")

uk<- pivot_longer(uk, col = 2:5, names_to = c("country"))

uk<- uk[, c(2,1,3)]

# World Bank data (available here: https://data.worldbank.org/indicator/SP.POP.TOTL)
world<- read_csv("Data Sets/world_pop.csv")
colnames(world)[1]<- "country"
world<- world %>%
  filter(
    country==	"Australia"	|
      country==	"Belgium"	|
      country==	"Brazil"	|
      country==	"Canada"	|
      country==	"China"	|
      country==	"Colombia"	|
      country==	"Hong Kong SAR, China"	|
      country==	"Czechia"	|
      country==	"Denmark"	|
      country==	"Finland"	|
      country==	"France"	|
      country==	"Germany"	|
      country==	"Greece"	|
      country==	"Iceland"	|
      country==	"Ireland"	|
      country==	"Israel"	|
      country==	"Luxembourg"	|
      country==	"New Zealand"	|
      country==	"Poland"	|
      country=="Paraguay" |
      country==	"South Africa"	|
      country==	"Korea, Rep."	|
      country==	"Spain"	|
      country==	"Sweden"	|
      country==	"Switzerland"	|
      country==	"Netherlands"	|
      country==	"United Kingdom")

world$country[world$country=="Korea, Rep."]<- "South Korea"
world$country[world$country=="Hong Kong SAR, China"]<- "China (Hong Kong, SAR)"
world$country[world$country=="Netherlands"]<- "The Netherlands"
world$country[world$country=="Czechia"]<- "Czech Republic"

world<- pivot_longer(world, col = 3:6, names_to = c("year"))
world<- world[, -c(2)]

world<- rbind(world, uk)

world<- world[order(world$country), ]

world<- world %>%
  filter(country!="United Kingdom")
sp<-s_pneumoniae
sp$z<- NA
new_dat<- sp[0, ]
sp_country<- sp %>%
  group_by(country) %>%
  summarise(count = n())
for (i in sp_country$country){
  x<- sp %>%
    filter(country==i)
  
  y<- world %>%
    filter(country==i)
  
  e<- as.numeric(y[1,3])
  n<- as.numeric(y[2,3])
  t<- as.numeric(y[3,3])
  to<-as.numeric(y[4,3])
  x$z[x$isoyear_sampled==2018]<- e
  x$z[x$isoyear_sampled==2019]<- n
  x$z[x$isoyear_sampled==2020]<- t
  x$z[x$isoyear_sampled==2021]<- to
  
  new_dat<- rbind(new_dat, x)
}
