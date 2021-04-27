# Finalized code

## load in r libraries and data

for (pkg in c("tidyverse", "readr", "dplyr", "countrycode", "zoo")) {library(pkg, character.only = TRUE)}

vaccine <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/vaccinations.csv")
name_list <- c("OWID_ENG", "OWID_NIR", "OWID_SCT", "OWID_WLS")
vaccine$iso_code <- with(vaccine, replace(iso_code, iso_code %in% name_list, "GBR"))
coronavirus_summary <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/coronavirus_summary.csv")
daily_fill <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/clean.csv")
coronavirus_summary$iso_code <- countrycode(coronavirus_summary$country, 'country.name','iso3c')
covronavirus <- coronavirus_summary %>% select(-country)
combine <- merge(vaccine, covronavirus, by="iso_code", all.x=T)
combine$date <- as.Date(combine$date,"%m/%d/%Y")

## fill na for people vaccinated


combine2 <-combine %>%
  select(country,date, people_vaccinated)%>%
  group_by(country)%>%
  arrange(date)%>%
  mutate(people_vaccinated = if_else(is.na(people_vaccinated ), 0, people_vaccinated))%>%
  group_by(country)%>%
  mutate(sum2 = cumsum(people_vaccinated))


#countries that doesn't have any people_vaccinated
countries_2 <- combine2%>%
  group_by(country)%>%
  summarise(total_vaccinations2 = max(sum2, na.rm=T))%>%
  filter(total_vaccinations2==0)%>%
  pull(country)

'%notin%' <- Negate('%in%')
combine_new_2 <- combine2%>%
  select(country, date, people_vaccinated)%>%
  filter(country %notin% countries_2)
#country_info_2 <- combine_new_2%>%
#  select(country,continent)%>%
#  distinct()
#if values are 0, fill in as na
combine_new_2$people_vaccinated <-na_if(combine_new_2$people_vaccinated,0)

combine_new_spread_2 <- combine_new_2%>%
  spread(key = date, value =people_vaccinated)%>%
  column_to_rownames("country")
#fill columns with at least to NAs
idx2 <- colSums(!is.na(combine_new_spread_2)) > 1
idx2
# [1] FALSE FALSE  TRUE  TRUE  TRUE

# interpolate 'TRUE columns' only
s2 <- apply(combine_new_spread_2[,idx2], 2, function(x) na.fill(x, list(0, "extend", "extend")))
s2 <- as.data.frame(s2)
s2 <- s2%>%
  rownames_to_column("country")
people_fill <- gather(s2, key = "date", value = "people_vaccinated",-country)
#people_fill <- left_join(people_fill, country_info_2, by = c("country","date"))
people_fill$date <- as.Date(people_fill$date,"%Y-%m-%d")
people_fill <- merge(people_fill, daily_fill, by = c("country", "date"))


## fillna for people_fully_vaccinated
combine3 <-combine %>%
  select(country,date, people_fully_vaccinated)%>%
  group_by(country)%>%
  arrange(date)%>%
  mutate(people_fully_vaccinated = if_else(is.na(people_fully_vaccinated ), 0, people_fully_vaccinated))%>%
  group_by(country)%>%
  mutate(sum3 = cumsum(people_fully_vaccinated))


#countries that doesn't have any people_vaccinated
countries_3 <- combine3%>%
  group_by(country)%>%
  summarise(total_vaccinations3 = max(sum3, na.rm=T))%>%
  filter(total_vaccinations3==0)%>%
  pull(country)

combine_new_3 <- combine3%>%
  select(country, date, people_fully_vaccinated)%>%
  filter(country %notin% countries_3)
#if values are 0, fill in as na
combine_new_3$people_fully_vaccinated <-na_if(combine_new_3$people_fully_vaccinated,0)

combine_new_spread_3 <- combine_new_3%>%
  spread(key = date, value =people_fully_vaccinated)%>%
  column_to_rownames("country")
#fill columns with at least to NAs
idx3 <- colSums(!is.na(combine_new_spread_3)) > 1
# [1] FALSE FALSE  TRUE  TRUE  TRUE

# interpolate 'TRUE columns' only
s3 <- apply(combine_new_spread_3[,idx3], 2, function(x) na.fill(x, list(0, "extend", "extend")))
s3 <- as.data.frame(s3)
s3 <- s3%>%
  rownames_to_column("country")

people_fully_fill <- gather(s3, key = "date", value = "people_fully_vaccinated",-country)
people_fully_fill$date <- as.Date(people_fully_fill$date,"%Y-%m-%d")
clean <- merge(people_fully_fill, people_fill, by = c('country', 'date'))

clean <- clean %>% select(-c(people_vaccinated.y,people_fully_vaccinated.y,people_fully_vaccinated.x,people_fully_vaccinated_1))
names(clean)[names(clean) ==  "people_vaccinated.x" ] <- "people_vaccinated"

clean['people_fully_vaccinated_per_million'] <- (clean$people_fully_vaccinated/clean$population)*1000000
clean['people_vaccinated_per_million'] <- (clean$people_vaccinated/clean$population)*1000000
clean['daily_vaccinated_per_million'] <- (clean$daily_vaccinations/clean$population)*1000000

#aggregate by continent
combine_continent<-clean%>%
  select(country,continent,date,people_fully_vaccinated,people_fully_vaccinated_per_million,
         people_vaccinated,people_vaccinated_per_million,
         daily_vaccinations, daily_vaccinated_per_million,population)%>%
  group_by(date,continent)%>%
  summarise(people_fully_vaccinated = sum(people_fully_vaccinated),
            people_fully_vaccinated_per_million = sum(people_fully_vaccinated_per_million),
            people_vaccinated=sum(people_vaccinated),
            daily_vaccinations=sum(daily_vaccinations),
            daily_vaccinated_per_million=sum(daily_vaccinated_per_million),
            people_vaccinated_per_million=sum(people_vaccinated_per_million),
            population = sum(population))
## fix abnormal data in people_vaccinated and people_fully_vaccinated
fix_abnormalities <- function(data,cn){
  cty_list<-unique(data$cn)
  var_list <- c("people_vaccinated","people_fully_vaccinated")
  for (c in cty_list){
    for (var in var_list){
      value_list <- data[var][data$cn==c]
      for (i in range(1,length(value_list))){
        if (value_list[i]<value_list[i-1]){
          value_list[i]<-value_list[i-1]
        }
      }
      for (j in range(1,length(value_list)-1)){
        if (value_list[j] == value_list[j+1]){
          value_list[j] <- (value_list[j-1]+value_list[j+1])/2
        }
      }
      value_list <- lapply(value_list,round,0)
      data[var][data$cn==c] <- value_list
    }
  }
}

clean_2 = fix_abnormalities(clean,"country")
combine_continent = fix_abnormalities(combine_continent,"continent")
