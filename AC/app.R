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

#Import & Clean Data


coronavirus_daily <- read.csv("C:/Users/Alexis Cenname/Desktop/Advanced R Computing/biost2094_project/data/coronavirus_daily.csv")
cleaned_data <- read.csv("C:/Users/Alexis Cenname/Desktop/Advanced R Computing/biost2094_project/data/clean_3.csv")


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


#Shiny Tab
ui <- navbarPage(title = "COVID-19 Vaccine",
                 # First Page
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
                          "Liling Liu",
                          br(),
                          "Alexis Cenname",
                          br(),
                          "Henry Thorpe",
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          img(src = "logo.jpeg",height = 130, width=250)),
                 tabPanel(title = "Worldwide Vaccine Progress"),
                 tabPanel(title = "Vaccine Progress Map"),
                 tabPanel(title = "US Vaccine Progress"),
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
                 ))



server <- function(input, output, session) {

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

}



# Run the app ----
shinyApp(ui = ui, server = server)






