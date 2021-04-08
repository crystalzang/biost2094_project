# load libraries
rm(list = ls())

for (pkg in c("tidyverse", "readr", "dplyr", "countrycode")) {library(pkg, character.only = TRUE)}
vaccine <- read_csv("data/vaccinations.csv")
name_list <- c("OWID_ENG", "OWID_NIR", "OWID_SCT", "OWID_WLS")
vaccine$iso_code <- with(vaccine, replace(iso_code, iso_code %in% name_list, "GBR"))
coronavirus_summary <- read_csv("data/coronavirus_summary.csv")
coronavirus_summary$iso_code <- countrycode(coronavirus_summary$country, 'country.name','iso3c')
covronavirus <- coronavirus_summary %>% select(-country)
combine <- merge(vaccine, covronavirus, by="iso_code", all.x=T)
head(combine)
combine_new <-combine %>%
  select(country,date,total_vaccinations,people_fully_vaccinated,people_vaccinated,total_vaccinations_per_hundred,continent)
vaccine_by_country<-combine_new%>%
group_by(country)%>%
  filter(!is.na(total_vaccinations))%>%
  filter(total_vaccinations==max(total_vaccinations)) %>%
  slice(which.max(as.Date(date)))
head(vaccine_by_country, n=50)
us_df <- combine_new %>% filter (country=="United States")
class(combine_new$date)
library(ggplot2)
library(dplyr)



# Most basic bubble plot
p <- ggplot(us_df, aes(x=date, y=total_vaccinations)) +
  geom_line() +
  xlab("")
p
