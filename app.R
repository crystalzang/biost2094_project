if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")

if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(tidyselect)) install.packages("tidyselect", repos = "http://cran.us.r-project.org")
if(!require(USAboundaries)) install.packages("USAboundaries", repos = "http://cran.us.r-project.org")
if(!require(tools)) install.packages("tools", repos = "http://cran.us.r-project.org")



#Henry
worldcountry <- geojson_read("data/custom.geo.json", what = "sp")
countries <- read.csv("data/concap.csv")
vaccines <- read.csv("data/clean_3.csv")

vaccines$iso <- countrycode(vaccines$country, 'country.name','iso3c')
countries$iso <- countrycode(countries$CountryName, 'country.name','iso3c')


vax_max <- vaccines %>% dplyr::group_by(iso) %>%  dplyr::summarize(max = max(people_vaccinated))
country_pops <- vaccines %>% dplyr::group_by(iso) %>% dplyr::summarize(max2 = max(population.x))
total_vax <- vaccines %>% dplyr::group_by(iso) %>% dplyr::summarize(max3 = max(cumsum_total_vaccination))


total <- inner_join(vax_max, countries, by = "iso")
total2 <- inner_join(country_pops, total, by = "iso")
total3 <- inner_join(total_vax, total2, by = "iso")


# Liling
#load data
combine_new <- read_csv("data/clean_3.csv")
combine_continent <- read_csv("data/combine_continent_new.csv")

cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent")),4)
cls_names = c(as.character(unique(combine_new$country)), as.character(unique(combine_continent$continent)))
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names
##define time stamp
#combine_new$date = as.Date(combine_new$date, "%m/%d/%Y")
min_date <- min(combine_new$date)
current_date = max(combine_new$date)
# function to plot cumulative vaccines by region
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
    xlim(plot_start_date, current_date + 5) +
    xlab("Date") +
    ylab("Cumulative")+
    geom_line() +
    #geom_point()+
    #scale_colour_manual(values=country_cols) +
    theme(legend.position = "none") +
    theme(
      legend.title = element_blank(),
      legend.position = "",
      plot.title = element_text(size=10)
    )

  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot new vaccines by region
daily_vaccines_plot <- function(combine_new,  plot_start_date) {
  g = ggplot(
    combine_new, aes(
      x = date,
      y = daily_outcome,
      fill = region,
      text = paste0(
        format(date, "%d %B %Y"),
        "\n",
        region,
        ": ",
        daily_outcome
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

###########################
#Alexis
coronavirus_daily <- read.csv("data/coronavirus_daily.csv")
cleaned_data <- read.csv("data/clean_3.csv")
cleaned_data$iso_code <- countrycode(cleaned_data$country, 'country.name','iso3c')
cleaned_data <- select(cleaned_data, -country)

coronavirus_daily$iso_code <- countrycode(coronavirus_daily$country, 'country.name','iso3c')
coronavirus_daily[is.na(coronavirus_daily)] <- 0
coronavirus_daily$date <- format(as.Date(coronavirus_daily$date,"%m/%d/%Y"))
coronavirus_daily <- select(coronavirus_daily, -country)

vds <- inner_join(cleaned_data, coronavirus_daily, by = c("date", "iso_code"))

#Gather & Aggregate Data
gathered_data <- vds %>%
  gather("case_vac_group", "case_vac", c(2, 13, 16), -date) %>%
  select(iso_code, date, case_vac_group, case_vac)

aggregated_cases <- aggregate(vds$daily_new_cases, by = list(vds$date, vds$continent), sum) %>% rename(new = x, date = Group.1, continent = Group.2)
aggregated_cases2 <- aggregate(vds$daily_new_deaths, by = list(vds$date, vds$continent), sum) %>% rename (deaths = x, date = Group.1, continent = Group.2)
aggregated_cases3 <- aggregate(vds$daily_vaccinations, by = list(vds$date, vds$continent), sum) %>% rename(vaccs = x, date = Group.1, continent = Group.2)

agr <- merge(aggregated_cases, aggregated_cases2, by=c("date", "continent"))
agr <- merge(agr, aggregated_cases3, by=c("date", "continent"))

gather2 <- agr %>% gather("case_group", "total_cases", 3:5, -continent, -date) %>% select(continent, date, case_group, total_cases)

gathered_data$factored <- factor(gathered_data$case_vac_group,
                                 levels = c("daily_vaccinations", "daily_new_deaths", "daily_new_cases"),
                                 labels = c("Vaccinations", "New Deaths", "New Cases"))

gather2$factored <- factor(gather2$case_group,
                           levels = c("vaccs", "deaths", "new"),
                           labels = c("Vaccinations", "New Deaths", "New Cases"))

gathered_data$country <- countrycode(gathered_data$iso_code, 'iso3c', 'country.name')

#Crystal --US
source("CZ/04_vaccine_us_final.R")

colours <- c("#E5F2DF", "#95C182", "#3F8127", "#1F5208")
suppressWarnings(expr)
plot1 <- function(data=us_states_vaccine, pop="18+", vstatus = "fully vaccinated"){
  ggplot(data = filter(data, population== pop, status == vstatus),
         aes(x = long, y = lat,
             group = group, fill = percent,  text = paste0(region,": ",  percent, "% are vaccinated") ))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Status in the U.S. Among ", pop, " Who Had ", vstatus),
         x =" ", y = " ")+
    theme_void()+
    scale_fill_gradient2(low = "white", mid = "#67A24D", high = "#3B6824", midpoint = 50,
                         limits=c(0, 100), breaks=seq(0,100,by=20))+
    theme(legend.title = element_text( size = 18),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=18))+
    labs(fill = "Percent (%)")+
    with(centroids,
         annotate(geom="text", x = long, y=lat, label = abb,
                  size = 4,color="white",family="Times")
    )
}

plot1.quantile <- function(data=us_states_vaccine, pop="18+", vstatus = "fully vaccinated"){
  ggplot(data = filter(data, population == pop, status == vstatus),
         aes(x = long, y = lat,
             group = group, fill = percent_q,  text = paste0(region,": ",  percent, "% are vaccinated")))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Status in the U.S. Among ", pop, " Who Had ", vstatus),
         x =" ", y = " ")+
    theme_void()+
    theme(legend.title = element_text( size = 18),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=18))+
    labs(fill = "Percent (%)")+
    scale_fill_manual(values = colours)+
    with(centroids,
         annotate(geom="text", x = long, y=lat, label = abb,
                  size = 4,color="white",family="Times")
    )
}

plot2 <- function(data = us_states_brand, vbrand = vbrand){
  data <- data %>%
    filter(brand ==vbrand)
  ggplot(data ,
         aes(x = long, y = lat,
             fill = percent,  text = paste0(region,": ",  percent, "% are vaccinated with ", vbrand)))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Status in the U.S. By ", vbrand),
         x =" ", y = " ")+
    theme_void()+
    scale_fill_gradient2(low = "white", mid = "#67A24D", high = "#3B6824",
                         midpoint = 50,limits=c(0, 80), breaks=seq(0,80,by=20))+
    theme(legend.title = element_text( size = 18),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=18))+
    labs(fill = "Percent (%)")+
    with(centroids,
         annotate(geom="text", x = long, y=lat, label = abb,
                  size = 4,color="white",family="Times")
    )
}

plot2.quantile <- function(data , vbrand = vbrand){
  data <- data %>%
    filter(brand ==vbrand)
  ggplot(data = filter(us_states_brand, brand == vbrand),
         aes(x = long, y = lat,
             fill = percent_q,  text = paste0(region,": ",  percent, "% are vaccinated with ", vbrand)))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Status in the U.S. By ", vbrand),
         x =" ", y = " ")+
    theme_void()+
    theme(legend.title = element_text( size = 18),
          legend.text = element_text(size = 13),
          plot.title = element_text(size=18))+
    labs(fill = "Percentage Quantile") +
    scale_fill_manual(values = colours)+
    with(centroids,
         annotate(geom="text", x = long, y=lat, label = abb,
                  size = 4,color="white",family="Times")
    )
}

plot2.barplot <- function(data, vbrand){
  state <- state_codes
  state$state_name <-  tolower(state$state_name)

  data_i <-data%>%
    filter(brand == vbrand)%>%
    left_join(state, by = c("state" = "state_name"))%>%
    filter(jurisdiction_type %in% c("state", "district"))%>%
    arrange(percent)%>%
    mutate(state = factor(state, levels=state))

  ggplot(data_i, aes(x = state, y=percent, fill = percent))+
    geom_bar(stat="identity") +
    coord_flip()+
    theme_classic()+
    geom_text(aes(label = percent), vjust = -0.3,size = 3)
}


ui <- navbarPage(title = "COVID-19 Vaccine",
                 tabPanel(title = "Vaccine Progress Map",
                            leafletOutput("mymap", width="100%", height="100%")

                          ),
                 tabPanel(title = "Worldwide Vaccine Progress",
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              #span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),

                              #span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),

                              pickerInput("level_select", "Level:",
                                          choices = c("Continent", "Country"),
                                          selected = c("Country"),
                                          multiple = FALSE),

                              pickerInput("region_select", "Country/Region:",
                                          choices = as.character(unique(combine_new$country)),
                                          options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                          selected = c("United States","India","United Kingdom","Brazil","China","Germany","France"),
                                          multiple = TRUE),

                              pickerInput("outcome_select", "Outcome:",
                                          choices = c("People_vaccinated", "People_vaccinated(per million)","fully_vaccinated","fully_vaccinated(per million)"),
                                          selected = c("People_vaccinated"),
                                          multiple = FALSE),

                              sliderInput("minimum_date",
                                          "Minimum date:",
                                          min = min_date,
                                          max = current_date,
                                          value = min_date,
                                          timeFormat="%d%b%y"),
                            ),

                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotlyOutput("cumulative_plot")),
                                tabPanel("Daily", plotlyOutput("daily_plot"))
                              )
                            )
                          )
                 ),
                 navbarMenu(title = "Daily Statistics",
                            tabPanel(title = "Country",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("countrylist",
                                                     label = h4("Country"),
                                                     choices = gathered_data %>% select(country),
                                                     selected = "iso_code"),
                                         dateRangeInput("date1", label="Date Range:",
                                                        start = as.character(gathered_data %>% filter(country == "Afghanistan") %>%
                                                                               filter(date == min(date)) %>% distinct(date)),
                                                        end = as.character(gathered_data %>% filter(country == "Afghanistan") %>%
                                                                             filter(date == max(date)) %>% distinct(date)),
                                                        min = as.character(gathered_data %>%
                                                                             filter(date == min(date)) %>% distinct(date)),
                                                        max = as.character(gathered_data %>%
                                                                             filter(date == max(date)) %>% distinct(date)),
                                                        startview = "year", separator = " - ")),
                                       mainPanel(plotlyOutput("countryplot", width = "100%", height = "100%"))
                                     )),
                            tabPanel(title="Continent",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("continentlist",
                                                     label = h4("Continent"),
                                                     choices = gather2 %>% select(continent),
                                                     selected = "continent"),
                                         dateRangeInput("date", label="Date Range:",
                                                        start = as.character(gather2 %>% filter(continent == "Europe") %>%
                                                                               filter(date == min(date)) %>% distinct(date)),
                                                        end = as.character(gather2 %>% filter(continent == "Europe") %>%
                                                                             filter(date == max(date)) %>% distinct(date)),
                                                        min = as.character(gather2 %>%
                                                                             filter(date == min(date)) %>% distinct(date)),
                                                        max = as.character(gather2 %>%
                                                                             filter(date == max(date)) %>% distinct(date)),
                                                        startview = "year", separator = " - ")),
                                       mainPanel(plotlyOutput("continentplot", width = "100%", height = "100%")
                                       )
                                     ))
                 ),
                 navbarMenu(title = "US Vaccine Progress",
                            tabPanel(title = "US Vaccine Progress",
                                     tags$style(type="text/css",
                                                ".shiny-output-error { visibility: hidden; }",
                                                ".shiny-output-error:before { visibility: hidden; }"
                                     ),
                                     sidebarLayout(
                                       #U.S. vaccination
                                       sidebarPanel(
                                         # Options to select population
                                         radioButtons("pop",
                                                      label = h3("Population"),
                                                      choices = list("Total population" = "all",
                                                                     "18+" = "18+",
                                                                     "65+" = "65+"),
                                                      selected = "Total population"),

                                         # Options to select vaccine status
                                         radioButtons("vstatus",
                                                      label = h3("Vaccine Status"),
                                                      choices = list("At least one dose" = "at least one dose",
                                                                     "Fully vaccinated" = "fully vaccinated"),
                                                      selected = "At Least One Dose"),
                                         width=3
                                       ),
                                       mainPanel(
                                         "Data is from",
                                         tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC"),
                                         tags$br(),tags$br(),
                                         "Data as of: April 27, 2021 6:00am ET. Posted: Tuesday, April 27, 2021 2:15 PM ET",
                                         fluidRow(
                                           column(3, offset = 9,

                                                  radioButtons(inputId = "display_option_p1",
                                                               label = "Display:",
                                                               choices = c("Actual Percent", "Quantile"),
                                                               selected = "Actual Percent")
                                           )),
                                         plotOutput("us_vaccine_plot"),
                                         width=9,
                                         plotOutput("us_vaccine_barplot_pop", height = 1500),
                                         tags$br(), tags$br(),tags$br()
                                       )
                                     )
                            ),
                            tabPanel(title = "US Vaccine Brand Progress",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # Options to select vaccine brand
                                         radioButtons("vbrand",
                                                      label = h3("Vaccine Brand"),
                                                      choices = list("Pfizer" = "Pfizer",
                                                                     "Moderna" = "Moderna",
                                                                     "Janssen(J&J)" = "Janssen(J&J)"),
                                                      selected = "Pfizer"),
                                         width=3
                                       ),
                                       mainPanel(
                                         "Data is from",
                                         tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC"),
                                         tags$br(),tags$br(),
                                         "Data as of: April 27, 2021 6:00am ET. Posted: Tuesday, April 27, 2021 2:15 PM ET",
                                         fluidRow(
                                           column(3, offset = 9,

                                                  radioButtons(inputId = "display_option",
                                                               label = "Display:",
                                                               choices = c("Actual Percent", "Quantile"),
                                                               selected = "Quantile")
                                           )),
                                         plotOutput("us_vaccine_plot2"),
                                         width=9,
                                         plotOutput("us_vaccine_barplot_brand", height = 1500),
                                         tags$br(), tags$br(),tags$br()
                                       )
                                     )

                            )
                 ),

                 # About the Site
                 tabPanel(title = "About the site",
                          tags$br(),tags$br(),tags$h4("Background"),
                          "SARS-CoV-2 has impacted the world in an unprecedented way,
                      with the world having changed significantly since H2N2/H3N2
                      pandemics in the mid 1900’s and even more since the major comparison
                      point, the 1918 flu pandemic. The world is much more global now.
                      Adjusted for inflation, the value of goods shipped internationally in
                      2014 was six times higher than the value of shipped goods in 1969, the
                      last year of the H3N2 pandemic. In comparison to 1919, that number goes
                      all the way to 53 times as much as shipped in 2014 (Beltekian). As such,
                      returning to normalcy is a worldwide goal.",
                          tags$br(),tags$br(),
                          "Right now, the main indicator
                      we have is the vaccination rate for each country, each of which has its
                      own unique situation and challenges to account for, so viewing that data
                      quickly and clearly is an important piece to know how close we are to the
                      end of the pandemic.",

                          tags$br(),tags$br(),tags$h4("Code"),
                          "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/czang97/biost2094_project", "Github."),

                          tags$br(),tags$br(),tags$h4("Contributor"),
                          br(),
                          "Crystal Zang",
                          br(),
                          "Liling Lu",
                          br(),
                          "Alexis Cenname",
                          br(),
                          "Henry Thrope",
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          img(src = "logo.jpeg",height = 130, width=250 ))
)

# Define server ----
server <- function(input, output, session) {
  #Henry
  output$map <- renderLeaflet({
      leaflet(worldcountry) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = total3$CapitalLongitude,
                       lat = total3$CapitalLatitude,
                       radius = round(total3$max/total3$max2*30, digits = 2),
                       label = total3$CountryName,
                       popup = paste("<strong>", total3$CountryName, "</strong>", "<br>",
                                     "Population:", prettyNum(total3$max2, big.mark="," , preserve.width="none"), "<br>",
                                     "People Fully Vaccinated:", prettyNum(total3$max,big.mark=",", preserve.width="none"), "<br>",
                                     "Percent Fully Vaccinated:", round(total3$max/total3$max2*100, digits = 2), "%", "<br>",
                                     "Total Vaccines Administered:", prettyNum(total3$max3,big.mark=",", preserve.width="none", round = 0)))
  })

  #Liling -- World Vaccination
  # Update region selections
  observeEvent(input$level_select,{
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select",
                        choices = as.character(unique(combine_new$country)),
                        selected =  as.character(unique(combine_new$country)))
      print("Country")
    }

    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select",
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"),
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
      print("Continent")
    }
  }, ignoreInit = TRUE)

  # create dataframe with selected countries or continents
  country_reactive_db = reactive({
    if (input$level_select=="Country") {
      db = combine_new
      db$region = db$country
    }
    if (input$level_select=="Continent") {
      db = combine_continent
      db$region = db$continent
    }

    if (input$outcome_select=="People_vaccinated") {
      db$outcome = db$people_vaccinated
      db$daily_outcome = db$daily_vaccinations
    }

    if (input$outcome_select=="People_vaccinated(per million)") {
      db$outcome = db$people_vaccinated_per_million
      db$daily_outcome = db$daily_vaccinated_per_million
    }

    if (input$outcome_select=="fully_vaccinated") {
      db$outcome = db$people_fully_vaccinated
      db$daily_outcome = db$people_fully_vaccinated
    }

    if (input$outcome_select=="fully_vaccinated(per million)") {
      db$outcome = db$people_fully_vaccinated_per_million
      db$daily_outcome = db$people_fully_vaccinated_per_million
    }
    #print(head(db))
    db %>% filter(region %in% input$region_select)
  })

  # cumulative vaccine plots
  output$cumulative_plot <- renderPlotly({
    cumulative_vaccinated_plot(country_reactive_db(), input$minimum_date)
  })

  # daily vaccine plots
  output$daily_plot <- renderPlotly({
    daily_vaccines_plot(country_reactive_db(), input$minimum_date)
  })


  #Alexis -- Daily Statistics
  {observeEvent(input$countrylist, {
    updateDateRangeInput(session, "date1", start = as.character(gathered_data %>% filter(country==input$countrylist) %>%
                                                                  filter(date == min(date)) %>% distinct(date)),
                         end = as.character(gathered_data %>% filter(country==input$countrylist) %>%
                                              filter(date == max(date)) %>% distinct(date)))})}

  {observeEvent(input$continentlist, {
    updateDateRangeInput(session, "date", start = as.character(gather2 %>% filter(continent==input$continentlist) %>%
                                                                 filter(date == min(date)) %>% distinct(date)),
                         end = as.character(gather2 %>% filter(continent==input$continentlist) %>%
                                              filter(date == max(date)) %>% distinct(date)))})}

  output$countryplot <- renderPlotly(
    plot_ly(gathered_data %>%
              filter(country==input$countrylist, date>=input$date1[1] & date<=input$date1[2], case_vac != 0),
            x = ~date,
            y = ~case_vac,
            name = ~factored, type = 'scatter',
            mode = "lines+markers", color=~factored,
            colors = c("green", "black", "red")) %>%
      layout(hovermode = "x unified",
             title = as.character(input$countrylist),
             xaxis = list(title=FALSE), yaxis = list(title="log(Number of People)", type="log",
                                                     tickmode="auto", nticks = 3)))
  output$continentplot <- renderPlotly(plot_ly(gather2 %>%
                                                 filter(continent==input$continentlist, date>=input$date[1] & date<=input$date[2],total_cases != 0),
                                               x = ~date,
                                               y = ~total_cases,
                                               name = ~factored, type = 'scatter',
                                               mode = 'lines+markers', color=~factored,
                                               colors = c("green", "black", "red")) %>%
                                         layout(hovermode = "x unified",
                                                title = as.character(input$continentlist),
                                                xaxis = list(title=FALSE), yaxis = list(title="log(Number of People)", type="log",
                                                                                        tickmode="auto", nticks = 6)))

#Crystal
  output$us_vaccine_plot <- renderPlot({
    if (input$display_option_p1 == "Actual Percent"){
      p1 <- plot1(us_states_vaccine, pop=input$pop, vstatus = input$vstatus)
      p1
      # ggplotly(p1, tooltip = "text") %>%
      #    layout(legend = list(font = list(size=11)))
    }else if (input$display_option_p1 == "Quantile") {
      p1.q <- plot1.quantile(us_states_vaccine, pop = input$pop, vstatus = input$vstatus)
      p1.q
      # ggplotly(p1.q, tooltip = "text") %>%
      #   layout(legend = list(font = list(size=11)))
    }
  })

  output$us_vaccine_plot2 <- renderPlot({

    if (input$display_option == "Actual Percent"){
      plot2(us_states_brand, vbrand = input$vbrand)

      # plotly::ggplotly(plot2, tooltip = c("text")) %>%
      #  layout(legend = list(font = list(size=11)))
    }else if (input$display_option == "Quantile") {
      plot2.quantile(us_states_brand, input$vbrand)
    }

  })

  output$us_vaccine_barplot_pop <- renderPlot({
    barplot_us(pop=input$pop)
  })

  output$us_vaccine_barplot_brand <- renderPlot({

    barplot_brand(vaccine_brand)

  })


}

# Run the app ----
shinyApp(ui = ui, server = server)
