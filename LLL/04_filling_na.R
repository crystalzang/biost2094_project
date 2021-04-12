---
  title: "03_filling_na"
author: "Liling Lu"
date: "4/10/2021"
output: html_document
editor_options:
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
library(Hmisc)
# Dealing with missing value
for (pkg in c("tidyverse", "readr", "dplyr", "countrycode", "zoo")) {library(pkg, character.only = TRUE)}
vaccine <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/vaccinations.csv")
name_list <- c("OWID_ENG", "OWID_NIR", "OWID_SCT", "OWID_WLS")
vaccine$iso_code <- with(vaccine, replace(iso_code, iso_code %in% name_list, "GBR"))
coronavirus_summary <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/coronavirus_summary.csv")
coronavirus_summary$iso_code <- countrycode(coronavirus_summary$country, 'country.name','iso3c')
covronavirus <- coronavirus_summary %>% select(-country)
combine <- merge(vaccine, covronavirus, by="iso_code", all.x=T)
combine$date <- as.Date(combine$date,"%m/%d/%Y")
combine <-combine %>%
  select(country,date,total_vaccinations,daily_vaccinations,daily_vaccinations_per_million,
         people_vaccinated,people_vaccinated_per_hundred,people_vaccinated_per_hundred,
         people_fully_vaccinated,people_fully_vaccinated_per_hundred,continent)%>%
  mutate(daily_vaccinations = if_else(is.na(daily_vaccinations ), 0, daily_vaccinations))
#combine_new<-combine_new[!is.na(combine_new$continent),]
#check missingness
apply(combine, 2, function(x) sum(is.na(x)))
# drop na if total_vaccinations and continent missing
combine<-combine %>% drop_na(total_vaccinations)
combine<-combine %>% drop_na(continent)
apply(combine, 2, function(x) sum(is.na(x)))
#check pearson relationship
df <- combine %>%
  select(daily_vaccinations,total_vaccinations,daily_vaccinations_per_million,
                         people_vaccinated,people_vaccinated_per_hundred,people_vaccinated_per_hundred,
                         people_fully_vaccinated,people_fully_vaccinated_per_hundred,)
rcorr(data.matrix(df))
# fill missing value in "people_vaccinated" by linear relationship with "daily_vaccinated"
lm1 <- lm(reformulate('daily_vaccinations', "people_vaccinated"), combine)$coef
combine$people_vaccinated <-
  ifelse(is.na(combine$people_vaccinated), lm1[1] + combine$daily_vaccinations*lm1[2], combine$people_vaccinated)
# fill missing value in people_fully_vaccinated by subtract people_vaccinated from toatl_vaccinations
combine$people_fully_vaccinated <- combine$total_vaccinations - combine$people_vaccinated
# set negative values in people_fully_vaccinated to zero
combine$people_fully_vaccinated[combine$people_fully_vaccinated < 0] <- 0
apply(combine, 2, function(x) sum(is.na(x)))
# fill missing value in "people_vaccinated_per_hundred" by linear relationship with "people_vaccinated"
lm2 <- lm(reformulate('people_vaccinated', "people_vaccinated_per_hundred"), combine)$coef
combine$people_vaccinated_per_hundred <-
  ifelse(is.na(combine$people_vaccinated_per_hundred), lm2[1] + combine$people_vaccinated*lm2[2], combine$people_vaccinated_per_hundred)
apply(combine, 2, function(x) sum(is.na(x)))
# fill missing value in "people_fully_vaccinated_per_hundred" by linear relationship with "people_fully_vaccinated"
lm3 <- lm(reformulate('people_fully_vaccinated', "people_fully_vaccinated_per_hundred"), combine)$coef
combine$people_fully_vaccinated_per_hundred <-
  ifelse(is.na(combine$people_fully_vaccinated_per_hundred), lm3[1] + combine$people_fully_vaccinated*lm3[2], combine$people_fully_vaccinated_per_hundred)
apply(combine, 2, function(x) sum(is.na(x)))
# fill missing value in "daily_vaccinations_per_million" by linear relationship with "daily_vaccinations"
lm4 <- lm(reformulate('daily_vaccinations', "daily_vaccinations_per_million"), combine)$coef
combine$daily_vaccinations_per_million <-
  ifelse(is.na(combine$daily_vaccinations_per_million), lm4[1] + combine$daily_vaccinations*lm4[2], combine$daily_vaccinations_per_million)
apply(combine, 2, function(x) sum(is.na(x)))
