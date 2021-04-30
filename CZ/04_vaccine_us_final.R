
# for (pkg in c("tidyverse", "readr", "dplyr", "countrycode", "janitor", "maps", "Hmisc", "tidyselect","USAboundaries"))
#   {library(pkg, character.only = TRUE)}



# read in data
vaccine_us <- read_csv("data/vaccine_us.csv")
# clean varaible names
vaccine_us <- vaccine_us%>%
  clean_names()%>%
  mutate(state_territory_federal_entity = if_else(state_territory_federal_entity == "New York State", "New York", state_territory_federal_entity))



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
# convert all variables to numeric (state name will be NA)
suppressWarnings(vaccine_us_new <- apply(vaccine_us_new, 2, as.numeric))
vaccine_us_new <- as.data.frame(vaccine_us_new)

#set state name using previous dataframe
vaccine_us_new$state <- vaccine_us$state_territory_federal_entity
vaccine_us_new$state <-  tolower(vaccine_us_new$state)

#gather by age group percent
vaccine_us_new_pop <- vaccine_us_new%>%
  gather(key ="population" , value="percent_vaccinated", pct_vaccinated, pct_65_vaccinated, pct_18_vaccinated )%>%
  mutate(population = if_else(population == "pct_18_vaccinated", "18+",
                              if_else(population == "pct_65_vaccinated","65+",
                                      "all")))%>%
  select(state, population, percent_vaccinated)%>%
  filter(!is.na(percent_vaccinated))

#gather by fully/partial status percent
vaccine_us_new_status <- vaccine_us_new%>%
  gather(key ="population" , value="percent_fully_vaccinated", pct_fully_vaccinated, pct_65_fully_vaccinated, pct_18_fully_vaccinated )%>%
  select(state, population, percent_fully_vaccinated)%>%
  mutate(population = if_else(population == "pct_18_fully_vaccinated", "18+",
                              if_else(population == "pct_65_fully_vaccinated","65+",
                                      "all")))%>%
  filter(!is.na(percent_fully_vaccinated))

vaccine_us_new_pop_status <- merge(vaccine_us_new_pop, vaccine_us_new_status, by = c("state", "population"), all =T)

state <- state_codes
state$state_name <-  tolower(state$state_name)


#gather overall percent
vaccine_us_long <- vaccine_us_new_pop_status%>%
  gather(key = "status", value = "percent", percent_vaccinated, percent_fully_vaccinated)%>%
  mutate(status = if_else(status == "percent_fully_vaccinated", "fully vaccinated", "at least one dose"))%>%
  select(state,population, status, percent)%>%
  filter(!is.na(percent))%>%
  left_join(state, by=c("state" = "state_name"))%>%
  filter(jurisdiction_type %in% c("state", "district"))%>%
  select(-jurisdiction_type, -state_code,-state_abbr)


# function to automatically format quantile label
qname <- function (q, a, b, c){
  paste("[", as.character(q[a]), ",",as.character(q[b]), ")",c,sep="")
}

#1. at least one dose, all
df1 <- filter(vaccine_us_long, status == "at least one dose", population == "all")
q <- quantile(df1$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df1  <- df1%>%
  mutate(percent_q = if_else(percent < q[2], qname(q,1,2,""),
                                    if_else(percent < q[3], qname(q,2,3,""),
                                            if_else(percent < q[4], qname(q,3,4,""),qname(q,4,5,"")))))

#1.5. fully, all
df1.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "all")
q <- quantile(df1.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

df1.5  <- df1.5%>%
  mutate(percent_q = if_else(percent < q[2], qname(q,1,2,""),
                                          if_else(percent < q[3], qname(q,2,3,""),
                                                  if_else(percent < q[4],qname(q,3,4,""), qname(q,4,5,"")))))


#2. at least one dose, 65+
df2 <- filter(vaccine_us_long, status == "at least one dose", population == "65+")
q <- quantile(df2$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df2 <- df2%>%
  mutate(percent_q = if_else(percent < q[2], qname(q,1,2,""),
                                       if_else(percent < q[3],  qname(q,2,3,""),
                                               if_else(percent < q[4],  qname(q,3,4,""),  qname(q,4,5,"")))))

#2.5. fully, 65+
df2.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "65+")
q <- quantile(df2.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df2.5 <- df2.5%>%
  mutate(percent_q = if_else(percent < q[2],  qname(q,1,2," "),
                                             if_else(percent < q[3],  qname(q,2,3," "),
                                                     if_else(percent < q[4],  qname(q,3,4," "), qname(q,4,5," ")))))

#3. at least one dose, 18+
df3 <- filter(vaccine_us_long, status == "at least one dose", population == "18+")
q <- quantile(df3$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df3 <- df3%>%
  mutate(percent_q = if_else(percent < q[2], qname(q,1,2," "),
                                       if_else(percent < q[3], qname(q,2,3," "),
                                               if_else(percent < q[4], qname(q,3,4," "), qname(q,4,5," ")))))

#3.5. fully, 18+
df3.5 <- filter(vaccine_us_long, status == "fully vaccinated", population == "18+")
q <- quantile(df3.5$percent, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
df3.5 <- df3.5%>%
  mutate(percent_q = if_else(percent < q[2], qname(q,1,2,"  "),
                                             if_else(percent < q[3], qname(q,2,3,"  "),
                                                     if_else(percent < q[4], qname(q,3,4,"  "), qname(q,4,5,"  ")))))

vaccine_us_long_quantile <- rbind(df1, df1.5, df2, df2.5, df3, df3.5)
factor  <- vaccine_us_long_quantile%>%
  dplyr::group_by(population, status,percent_q)%>%
  dplyr::summarize(N=n())

vaccine_us_long_quantile$percent_q <- factor(vaccine_us_long_quantile$percent_q, levels =factor$percent_q)
#add state abbreviation
vaccine_us_long_quantile$abb<-state.abb[match(vaccine_us_long_quantile$state,tolower(state.name))]


#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_vaccine <- left_join(us_states, vaccine_us_long_quantile, by=c("region" = "state"))
us_states_vaccine$region <-  capitalize(us_states_vaccine$region)

##########################################
# vaccine brand
vaccine_brand <- vaccine_us_new%>%
  filter(!is.na( pct_vaccinated))%>%
  mutate(population = round(people_vaccinated/ pct_vaccinated * 100, digits=2),
         pct_janssen = round(janssen_admin/population * 100,digits=2),
                pct_moderna = round(moderna_admin/population * 100,digits=2),
                      pct_pfizer = round(pfizer_admin/population * 100,digits=2),
                            janssen_pct_fully = round(janssen_fully/ population * 100,digits=2),
                                  moderna_pct_fully = round(moderna_fully/population *100,digits=2),
                                        pfizer_pct_fully = round(pfizer_fully/population *100,digits=2)
  )

quantile_janssen <- quantile(vaccine_brand$pct_janssen, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
quantile_moderna <- quantile(vaccine_brand$pct_moderna, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)
quantile_pfizer <- quantile(vaccine_brand$pct_pfizer, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

vaccine_brand <- vaccine_brand%>%
  mutate(pct_janssen_q = if_else(pct_janssen < quantile_janssen[2], qname(quantile_janssen,1,2,""),
                                 if_else(pct_janssen < quantile_janssen[3], qname(quantile_janssen,2,3,""),
                                         if_else(pct_janssen < quantile_janssen[4], qname(quantile_janssen,3,4,""), qname(quantile_janssen,4,5,"")))))%>%
  mutate(pct_moderna_q = if_else(pct_moderna < quantile_moderna[2],  qname(quantile_moderna,1,2,""),
                                 if_else(pct_moderna < quantile_moderna[3],  qname(quantile_moderna,2,3,""),
                                         if_else(pct_moderna < quantile_moderna[4],  qname(quantile_moderna,3,4,""),  qname(quantile_moderna,4,5,"")))))%>%
  mutate(pct_pfizer_q = if_else(pct_pfizer < quantile_pfizer[2], qname(quantile_pfizer,1,2,""),
                                if_else(pct_pfizer < quantile_pfizer[3], qname(quantile_pfizer,2,3,""),
                                        if_else(pct_pfizer < quantile_pfizer[4],qname(quantile_pfizer,3,4,""), qname(quantile_pfizer,4,5,"")))))

vaccine_brand_quantile <- vaccine_brand%>%
  select(state,pct_pfizer_q, pct_janssen_q, pct_moderna_q)%>%
  gather(key = "brand", value = "percent_q", pct_pfizer_q, pct_janssen_q, pct_moderna_q)%>%
  mutate(brand = if_else(brand == "pct_janssen_q", "Janssen(J&J)",
                         if_else(brand == "pct_moderna_q", "Moderna", "Pfizer")))


vaccine_brand_percent <- vaccine_brand%>%
  select(state, pct_moderna, pct_pfizer, pct_janssen)%>%
  gather(key = "brand", value = "percent",  pct_moderna, pct_pfizer, pct_janssen)%>%
  mutate(brand = if_else(brand == "pct_janssen", "Janssen(J&J)",
                         if_else(brand == "pct_moderna", "Moderna", "Pfizer")))

vaccine_brand_all <- merge(vaccine_brand_quantile, vaccine_brand_percent, by =c("state", "brand"), all=T)

factor  <- vaccine_brand_all%>%
  dplyr::group_by(brand,percent_q)%>%
  dplyr::summarize(N=n())

level <- c("[0.04,1.7225)", "[1.7225,2.225)", "[2.225,2.7575)", "[2.7575,5.22)" ,   "[8.93,27.27)" , "[27.27,30.925)" ,
           "[30.925,33.23)" ,  "[33.23,106.94)"  ,    "[0,32.765)"  ,     "[32.765,36.835)" ,
          "[36.835,40.9525)", "[40.9525,59.33)" )
vaccine_brand_all$percent_q <- factor(vaccine_brand_all$percent_q, levels =level)
#add state abbreviation
vaccine_brand_all$abb<-state.abb[match(vaccine_brand_all$state,tolower(state.name))]


#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_brand <- left_join(us_states, vaccine_brand_all, by=c("region" = "state"))
us_states_brand$region <-  capitalize(us_states_brand$region)

centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]


#output data:
#vaccine_us_long_quantile
#us_states_vaccine (with long lat)

#vaccine_brand
#vaccine_brand_all
#us_states_brand (with long lat)

#centroids (for abbreviation on the map)

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

state <- state_codes

#plot
colours <- c("#E5F2DF", "#95C182", "#3F8127", "#1F5208")

barplot_us <- function( pop=pop){
  vaccine_us_long$state <- str_to_title(vaccine_us_long$state)
  vaccine_us_long%>%
    mutate(state = if_else(state == "District Of Columbia", "District of Columbia", state))%>%
    spread(status, percent)%>%
    clean_names()%>%
    filter(population == pop)%>%
    left_join(state, by = c("state" = "state_name"))%>%
    filter(jurisdiction_type %in% c("state", "district"))%>%
    dplyr::select(-jurisdiction_type)%>%
    dplyr::arrange(at_least_one_dose)%>%
    mutate(state = factor(state, levels=state))%>%
   ggplot()+
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          text = element_text(size=20)) +

    # add a dummy point for scaling purposes
    geom_point(aes(x = 12, y = state),
               size = 0, col = "white")+

    # add the horizontal state lines
    geom_hline(yintercept = 1:50, col = "grey80")+

    # add a point for each fully vaccinated rate
    geom_point(aes(x = fully_vaccinated, y = state),
               size = 11, col = colours[2]) +
    # add a point for each partially vaccinated rate
    geom_point(aes(x = at_least_one_dose, y = state),
               size = 11, col = colours[4]) +
    # add the text (%) for each pfizer success rate
    geom_text(aes(x = fully_vaccinated, y = state,
                  label = paste0(round(fully_vaccinated, 1.3))),
              col = "black") +
    # add the text (%) for each moderna success rate
    geom_text(aes(x = at_least_one_dose, y = state,
                  label = paste0(round(at_least_one_dose, 1.3))),
              col = "white")+
    # add a label above the first two points
    geom_text(aes(x = x, y = y, label = label, col = label),
              data.frame(x = c(30, 50), y = 54,
                         label = c( "% Fully Vaccinated","% At Least One Dose")), size = 6) +
    scale_color_manual(values = c(colours[4], colours[2]), guide = "none")  +
    # manually set the spacing above and below the plot
    scale_y_discrete(expand = c(0.2, 0.05)) +
    # manually specify the x-axis
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0%", "20%", "40%", "60%","80%","100%"))
}

barplot_us(pop="18+")

barplot_brand <- function(vaccine_brand){
  vaccine_brand$state <- str_to_title(vaccine_brand$state)

  vaccine_brand%>%
    mutate(state = if_else(state == "District Of Columbia", "District of Columbia", state))%>%
    dplyr::select(state, pct_moderna, pct_pfizer, pct_janssen)%>%
    left_join(state, by = c("state" = "state_name"))%>%
    filter(jurisdiction_type %in% c("state", "district"))%>%
    dplyr::select(-jurisdiction_type)%>%
    arrange(pct_pfizer)%>%
    mutate(state = factor(state, levels=state))%>%
    ggplot()+
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        text = element_text(size=20)) +

  # add a dummy point for scaling purposes
  geom_point(aes(x = 12, y = state),
             size = 0, col = "white")+

  # add the horizontal state lines
  geom_hline(yintercept = 1:50, col = "grey80")+

  # add a point for each pfizer vaccination rate
  geom_point(aes(x = pct_pfizer, y = state),
             size = 11, col = colours[4]) +
  # add a point for each moderna success rate
  geom_point(aes(x = pct_moderna, y = state),
             size = 11, col = colours[3]) +
  # add a point for each J&J success rate
  geom_point(aes(x = pct_janssen, y = state),
             size = 11, col = colours[2])  +
  # add the text (%) for each pfizer success rate
  geom_text(aes(x = pct_pfizer, y = state,
                label = paste0(round(pct_pfizer, 1.3))),
            col = "white") +
  # add the text (%) for each moderna success rate
  geom_text(aes(x = pct_moderna, y = state,
                label = paste0(round(pct_moderna, 1.3))),
            col = "black")+
  # add the text (%) for each J&J success rate
  geom_text(aes(x = pct_janssen, y = state,
                label = paste0(round(pct_janssen, 1.3))),
            col = "black") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(6, 35, 45), y = 54,
                       label = c( "% J&J", "% Moderna","% Pfizer")), size = 6) +
  scale_color_manual(values = c(colours[2], colours[3], colours[4]), guide = "none")  +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.1, 0)) +
  # manually specify the x-axis
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0%", "20%", "40%", "60%","80%","100%"))
}

#barplot_brand(vaccine_brand)



