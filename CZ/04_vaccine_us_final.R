
for (pkg in c("tidyverse", "readr", "dplyr", "countrycode", "janitor", "maps", "Hmisc"))
  {library(pkg, character.only = TRUE)}

vaccine_us <- read_csv("/Users/czang/Documents/2021Spring/R/biost2094_project/data/vaccine_us.csv")
vaccine_us <- vaccine_us%>%
  clean_names()

vaccine_us_new <- vaccine_us%>%
  select(state_territory_federal_entity,
         people_with_at_least_one_dose_by_state_of_residence,
         percent_of_total_pop_with_at_least_one_dose_by_state_of_residence,
         percent_of_total_pop_fully_vaccinated_by_state_of_residence,
         percent_of_65_pop_with_at_least_one_dose_by_state_of_residence,
         percent_of_65_pop_fully_vaccinated_by_state_of_residence,
         percent_of_18_pop_with_at_least_one_dose_by_state_of_residence,
         percent_of_18_pop_fully_vaccinated_by_state_of_residence,
         #brand delivered/administrated
         total_number_of_pfizer_doses_delivered,
         total_number_of_pfizer_doses_adminstered,
         total_number_of_janssen_doses_delivered,
         total_number_of_janssen_doses_administered,
         total_number_of_moderna_doses_delivered,
         total_number_of_moderna_doses_administered,
         #brand fully vaccinated
         people_fully_vaccinated_moderna_resident,
         people_fully_vaccinated_pfizer_resident,
         people_fully_vaccinated_janssen_resident
  )%>%
  rename("state" = "state_territory_federal_entity",
         "people_vaccinated" = "people_with_at_least_one_dose_by_state_of_residence",
         "pct_vaccinated" = "percent_of_total_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_fully_vaccinated" = "percent_of_total_pop_fully_vaccinated_by_state_of_residence",
         "pct_65_vaccinated" = "percent_of_65_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_65_fully_vaccinated"= "percent_of_65_pop_fully_vaccinated_by_state_of_residence",
         "pct_18_vaccinated" = "percent_of_18_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_18_fully_vaccinated"= "percent_of_18_pop_fully_vaccinated_by_state_of_residence",
         "pfizer_deliver" = "total_number_of_pfizer_doses_delivered",
         "pfizer_admin"= "total_number_of_pfizer_doses_adminstered",
         "pfizer_fully" = "people_fully_vaccinated_pfizer_resident",
         "janssen_deliver" = "total_number_of_janssen_doses_delivered",
         "janssen_admin" = "total_number_of_janssen_doses_administered",
         "janssen_fully" = "people_fully_vaccinated_janssen_resident",
         "moderna_deliver" = "total_number_of_moderna_doses_delivered",
         "moderna_admin" = "total_number_of_moderna_doses_administered",
         "moderna_fully"  = "people_fully_vaccinated_moderna_resident"
  )%>%
  mutate( janssen_pct_admin = (janssen_deliver-janssen_admin)/janssen_deliver,
          pfizer_pct_admin = (pfizer_deliver - pfizer_admin)/pfizer_deliver ,
          moderna_pct_admin =  (moderna_deliver-moderna_admin)/moderna_deliver)

vaccine_us_new <- apply(vaccine_us_new, 2, as.numeric)
vaccine_us_new <- as.data.frame(vaccine_us_new)
vaccine_us_new$state <- vaccine_us$state_territory_federal_entity
vaccine_us_new$state <-  tolower(vaccine_us_new$state)

vaccine_us_long <- vaccine_us_new%>%
  #gather by age group percent
  gather(key ="population" , value="percent_vaccinated", pct_vaccinated, pct_65_vaccinated, pct_18_vaccinated )%>%
  mutate(population = if_else(population == "pct_18_vaccinated", "18+",
                              if_else(population == "pct_65_vaccinated","65+",
                                      "all")))%>%
  #gather by fully/partial status percent
  gather(key ="population2" , value="percent_fully_vaccinated", pct_fully_vaccinated, pct_65_fully_vaccinated, pct_18_fully_vaccinated )%>%
  select(-population2)%>%
  #gather overall percent
  gather(key = "status", value = "percent", percent_vaccinated, percent_fully_vaccinated)%>%
  mutate(status = if_else(status == "percent_fully_vaccinated", "fully vaccinated", "at least one dose"))

#1. at least one dose, all
df1 <- filter(vaccine_us_long, status == "at least one dose", population == "all")
pct_vaccinated_q <- quantile(df1$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df1  <- df1%>%
  mutate(pct_vaccinated_q = if_else(percent < pct_vaccinated_q[2], "[15%,29%)",
                                    if_else(percent < pct_vaccinated_q[3], "[29%,33%)",
                                            if_else(percent < pct_vaccinated_q[4], "[33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_vaccinated_q")

#1.5. fully, all
df1.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "all")
pct_fully_vaccinated_q <- quantile(df1.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

df1.5  <- df1.5%>%
  mutate(pct_fully_vaccinated_q = if_else(percent < pct_fully_vaccinated_q[2], "[15%,29%)",
                                          if_else(percent < pct_fully_vaccinated_q[3], "[29%,33%)",
                                                  if_else(percent < pct_fully_vaccinated_q[4], "[33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_fully_vaccinated_q")


#2. at least one dose, 65+
df2 <- filter(vaccine_us_long, status == "at least one dose", population == "65+")
pct_65_vaccinated_q <- quantile(df2$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df2 <- df2%>%
  mutate(pct_65_vaccinated_q = if_else(percent < pct_65_vaccinated_q[2], "[15%,29%)",
                                       if_else(percent < pct_65_vaccinated_q[3], "[29%,33%)",
                                               if_else(percent < pct_65_vaccinated_q[4], "[33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_65_vaccinated_q")

#2.5. fully, 65+
df2.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "65+")
pct_65_fully_vaccinated_q <- quantile(df2.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df2.5 <- df2.5%>%
  mutate(pct_65_fully_vaccinated_q = if_else(percent < pct_65_fully_vaccinated_q[2], "[15%,29%)",
                                             if_else(percent < pct_65_fully_vaccinated_q[3], "[29%,33%)",
                                                     if_else(percent < pct_65_fully_vaccinated_q[4], "33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_65_fully_vaccinated_q")

#3. at least one dose, 18+
df3 <- filter(vaccine_us_long, status == "at least one dose", population == "18+")
pct_18_vaccinated_q <- quantile(df3$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

df3 <- df3%>%
  mutate(pct_18_vaccinated_q = if_else(percent < pct_18_vaccinated_q[2], "[15%,29%)",
                                       if_else(percent < pct_18_vaccinated_q[3], "[29%,33%)",
                                               if_else(percent < pct_18_vaccinated_q[4], "[33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_18_vaccinated_q")

#3.5. fully, 18+
df3.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "18+")
pct_18_fully_vaccinated_q <- quantile(df3.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

df3.5 <- df3.5%>%
  mutate(pct_18_fully_vaccinated_q = if_else(percent < pct_18_fully_vaccinated_q[2], "[15%,29%)",
                                             if_else(percent < pct_18_fully_vaccinated_q[3], "[29%,33%)",
                                                     if_else(percent < pct_18_fully_vaccinated_q[4], "[33%,35%)", "[35%,47%)"))))%>%
  rename("percent_q" = "pct_18_fully_vaccinated_q")

all <- rbind(df1, df1.5, df2, df2.5, df3, df3.5)
#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_vaccine <- left_join(us_states, all, by=c("region" = "state"))
us_states_vaccine$region <-  capitalize(us_states_vaccine$region)


##########################################
# vaccine brand
vaccine_brand <- vaccine_us_new%>%
  mutate(population = people_vaccinated/ pct_vaccinated * 100,
         pct_janssen = janssen_admin/population * 100,
         pct_moderna = moderna_admin/population * 100,
         pct_pfizer = pfizer_admin/population * 100,
         janssen_pct_fully = janssen_fully/ population * 100,
         moderna_pct_fully = moderna_fully/population *100,
         pfizer_pct_fully = pfizer_fully/population *100
  )

quantile_janssen <- quantile(vaccine_brand$pct_janssen, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
quantile_moderna <- quantile(vaccine_brand$pct_moderna, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
quantile_pfizer <- quantile(vaccine_brand$pct_pfizer, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

vaccine_brand <- vaccine_brand%>%
  mutate(pct_janssen_q = if_else(pct_janssen < quantile_janssen[2], "[0%,1.01%)",
                                 if_else(pct_janssen < quantile_janssen[3], "[1.01%,1.17%)",
                                         if_else(pct_janssen < quantile_janssen[4], "[1.17%,1.34%)", "[1.34,2.34)"))))%>%
  mutate(pct_moderna_q = if_else(pct_moderna < quantile_moderna[2], "[7.03%,20.82%)",
                                 if_else(pct_moderna < quantile_moderna[3], "[20.82%,23.69%)",
                                         if_else(pct_moderna < quantile_moderna[4], "[23.69%,25.90%)", "[25.90%,73.89%)"))))%>%
  mutate(pct_pfizer_q = if_else(pct_pfizer < quantile_pfizer[2], "[0%,23.78%)",
                                if_else(pct_pfizer < quantile_pfizer[3], "[23.78%,26.15%)",
                                        if_else(pct_pfizer < quantile_pfizer[4], "[26.15%,28.28%)", "[28.28%,45.25%)"))))%>%
  gather(key = "brand", value = "percent_q", pct_pfizer_q, pct_janssen_q, pct_moderna_q)%>%
  mutate(brand = if_else(brand == "pct_janssen_q", "Janssen(J&J)",
                         if_else(brand == "pct_moderna_q", "Moderna", "Pfizer")))%>%
  gather(key = "brand2", value = "percent", pct_pfizer, pct_janssen, pct_moderna)%>%
  select(-brand2)%>%
  mutate(percent= round(percent, 2))

#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_brand <- left_join(us_states, vaccine_brand, by=c("region" = "state"))
us_states_brand$region <-  capitalize(us_states_brand$region)