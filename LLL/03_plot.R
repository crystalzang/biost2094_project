# load libraries
rm(list = ls())
setwd("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project")
for (pkg in c("tidyverse", "readr", "dplyr", "countrycode")) {library(pkg, character.only = TRUE)}
#load data
vaccine <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/vaccinations.csv")
name_list <- c("OWID_ENG", "OWID_NIR", "OWID_SCT", "OWID_WLS")
vaccine$iso_code <- with(vaccine, replace(iso_code, iso_code %in% name_list, "GBR"))
coronavirus_summary <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/coronavirus_summary.csv")
coronavirus_summary$iso_code <- countrycode(coronavirus_summary$country, 'country.name','iso3c')
covronavirus <- coronavirus_summary %>% select(-country)
combine <- merge(vaccine, covronavirus, by="iso_code", all.x=T)
combine_new <-combine %>%
  select(country,continent,date,people_fully_vaccinated,people_fully_vaccinated_per_hundred,
         people_vaccinated,people_vaccinated_per_hundred,
         daily_vaccinations, daily_vaccinations_per_million)
combine_new<-combine_new[!is.na(combine_new$continent),]
combine_new['daily_vaccinations_per_hundred'] <- combine_new$daily_vaccinations_per_million * 10000
#define time stamp
min_date <- min(combine_new$date)
current_date <- max(combine_new$date)
#aggregate by continent
combine_continent<-combine_new%>%
  select(country,continent,date,people_fully_vaccinated,people_fully_vaccinated_per_hundred,
         people_vaccinated,people_vaccinated_per_hundred,
         daily_vaccinations, daily_vaccinations_per_hundred)%>%
  group_by(date,continent)%>%
  summarise(people_fully_vaccinated = sum(people_fully_vaccinated),
            people_fully_vaccinated_per_hundred = sum(people_fully_vaccinated_per_hundred),
            people_vaccinated=sum(people_vaccinated),
            daily_vaccinations=sum(daily_vaccinations),
            daily_vaccinations_per_hundred=sum(daily_vaccinations_per_hundred),
            people_vaccinated_per_hundred=sum(people_vaccinated_per_hundred))
apply(combine_new, 2, function(x) sum(is.na(x)))
x<-combine_new[is.na(combine_new$continent),]
x$country
# fill na for new_vaccine_cnt_rolling5
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos

  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

#add rolling 5-day averages for vaccine numbers
cnt_rolling5 <- function(df,feat){
  df["cnt_rolling5"]= df[feat]
  country_list = unique(df$country)
  for (i in 1:length(country_list)){
     country_sub = subset(df,country==country_list[i])
     country_sub <- country_sub[!is.na(country_sub[feat]),]
     #country_sub$daily_vaccinations <- replacecena2(country_sub$daily_vaccinations)

     if (nrow(country_sub)>=5){
        for (j in 5:nrow(country_sub)){
          country_sub$cnt_rolling5[j] = round(mean(country_sub[(j-4):j,feat],0))
          df$cnt_rolling5[df$country==country_list[i]] = country_sub$cnt_rolling5
        }
     }
  df$cnt_rolling5<- f1(df$cnt_rolling5)

  }
  return (df$cnt_rolling5)
}
combine_new['daily_vaccinations_5mean']<- cnt_rolling5(combine_new,"daily_vaccinations" )
combine_new['people_vaccinated_5mean']<- cnt_rolling5(combine_new,"people_vaccinated" )
combine_new['people_vaccinated_per_hundred_5mean']<- cnt_rolling5(combine_new,"people_vaccinated_per_hundred" )
combine_new['people_fully_vaccinated_5mean']<- cnt_rolling5(combine_new,"people_fully_vaccinated" )
combine_new['daily_vaccinations_per_million_5mean']<- cnt_rolling5(combine_new,"daily_vaccinations_per_million" )
combine_new['people_fully_vaccinated_per_hundred_5mean']<- cnt_rolling5(combine_new,"people_fully_vaccinated_per_hundred" )


# assign colours to countries to ensure consistency between plots
library(RColorBrewer)
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(combine_new$country)), as.character(unique(combine_continent$continent)))
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

# function to plot new vaccines by region
current_date = as.Date(max(combine_new$date),"%Y-%m-%d")
daily_vaccines_plot <- function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = outcome,
      fill = region,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        region,
        ": ",
        outcome
      )
    )
  ) +
  xlim(plot_start_date,current_date+5) +
  xlab("Date") +
  geom_bar(position="stack", stat="identity") +
    ylab("New (Dayly)") +
    #scale_fill_manual(values=combine_new$country) +
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10))
  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}
daily_vaccines_plot(combine_new,  as.Date("2020-12-22"))
# plot new vaccines per million by country
current_date = as.Date(max(combine_new$date),"%Y-%m-%d")
country_vaccines_per_million_plot = function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = daily_vaccinations_per_million_5mean,
      fill = country,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        country,
        ": ",
        daily_vaccinations_per_million_5mean
      )
    )
  ) +
    xlim(plot_start_date,current_date+5) +
    xlab("Date") +
    geom_bar(position="stack", stat="identity") +
    theme(legend.position = "none")

  g1 = g +
    geom_bar(position="stack", stat="identity") +
    ylab("New (Dayly)") + theme_bw() +
    #scale_fill_manual(values=combine_new$country) +
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10)
    )

  #ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}
country_vaccines_per_million_plot(combine_new,  as.Date("2020-12-22"))
# function to plot cumulative vaccines by region
current_date = as.Date(max(combine_new$date),"%Y-%m-%d")
cumulative_vaccinated_plot <- function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = outcome,
      color = region,
      group=1,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        region,
        ": ",
        outcome
      )
    ) ) +
    xlim(plot_start_date,current_date+5) +
    xlab("Date") +
    geom_line() +
    geom_point()+
    scale_colour_manual(values=country_cols) +
    theme(legend.position = "none")+
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10)
    )

  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}
cumulative_vaccinated_plot(combine_new,  as.Date("2020-12-22"))
# plot people fully vaccinated by country
current_date = as.Date(max(combine_new$date),"%Y-%m-%d")
country_fully_vaccinated_plot = function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = people_fully_vaccinated,
      group=1,
      color = country,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        country,
        ": ",
        people_fully_vaccinated
      )
    )
  ) +
    xlim(plot_start_date,current_date+5) +
    xlab("Date") +
    theme(legend.position = "none")

  g1 = g +
    geom_line(alpha=0.8,key_glyph = "timeseries")+
    geom_point(size = 1, alpha = 0.8)+
    ylab("cummulative cnts") + theme_bw() +
    #scale_fill_manual(values=combine_new$country) +
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10)
    )

  #ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}
country_fully_vaccinated_plot(combine_new,  as.Date("2020-12-22"))
#aggregate by continent with rolling5 cnts
colnames(combine_new)
combine_by_date_continent<-combine_new%>%
  select(continent,date,people_fully_vaccinated_5mean,
         people_vaccinated_5mean,
         people_vaccinated_per_hundred_5mean,
         daily_vaccinations_5mean,daily_vaccinations_per_million_5mean)%>%
  group_by(date,continent)%>%
  summarise(people_fully_vaccinated_5mean = sum(people_fully_vaccinated_5mean),
            people_vaccinated_5mean=sum(people_vaccinated_5mean),
            daily_vaccinations_5mean=sum(daily_vaccinations_5mean),
            daily_vaccinations_per_million_5mean=sum(daily_vaccinations_per_million_5mean),
            people_vaccinated_per_hundred_5mean=sum(people_vaccinated_per_hundred_5mean))
combine_by_date_continent_1<-combine_new%>%
  select(continent,date,people_fully_vaccinated,
         people_vaccinated,
         people_vaccinated_per_hundred,
         daily_vaccinations,daily_vaccinations_per_million)%>%
  group_by(date,continent)%>%
  #filter(!is.na(people_vaccinated))%>%
  summarise(people_fully_vaccinated = sum(people_fully_vaccinated),
            people_vaccinated=sum(people_vaccinated),
            daily_vaccinations=sum(daily_vaccinations),
            daily_vaccinations_per_million=sum(daily_vaccinations_per_million),
            people_vaccinated_per_hundred=sum(people_vaccinated_per_hundred))
# function to plot cummulative vaccines by continent'
current_date = as.Date(max(combine_new$date),"%Y-%m-%d")
continent_vaccines_plot = function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = people_vaccinated,
      color = continent,
      group=1,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        continent,
        ": ",
        people_vaccinated
      )
    )
  ) +
    xlim(plot_start_date,current_date+5) +
    xlab("Date") +
    theme(legend.position = "none")

  g1 = g +
    geom_line() + geom_point()+
    ylab("New (Dayly)") + theme_bw() +
    #scale_fill_manual(values=combine_new$country) +
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10)
    )

  #ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}
continent_vaccines_plot(combine_continent,  as.Date("2020-12-22"))

