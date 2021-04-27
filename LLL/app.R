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
for (pkg in c("tidyverse", "countrycode")) {library(pkg, character.only = TRUE)}
library(RColorBrewer)
library(plotly)
library(Hmisc)
#load data
combine_new <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/clean_3.csv")
combine_continent <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/combine_continent_new.csv")
#names(combine_new)[names(combine_new) ==  "population.x" ] <- "population"
#colnames(combine_new)
#combine_continent<-combine_new%>%
#  select(continent,date,people_fully_vaccinated,people_fully_vaccinated_per_million,
#         people_vaccinated,people_vaccinated_per_million,
#         daily_vaccinations, daily_vaccinated_per_million)%>%
#  group_by(date,continent)%>%
#  summarise(people_fully_vaccinated = sum(people_fully_vaccinated),
#            people_fully_vaccinated_per_million = sum(people_fully_vaccinated_per_million),
#            people_vaccinated=sum(people_vaccinated),
#            daily_vaccinations=sum(daily_vaccinations),
#            daily_vaccinated_per_million=sum(daily_vaccinated_per_million),
#            people_vaccinated_per_million=sum(people_vaccinated_per_million))
##combine_continent <- read_csv("/Users/liling.lu/pitt 2021-spring/2094/biost2094_project/data/combine_continent.csv")
## assign colours to countries to ensure consistency between plots
#write.csv(combine_new, "data/clean_3.csv", row.names = F)
#write.csv(combine_continent, "data/combine_continent_new.csv", row.names = F)
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
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
    geom_point()+
    scale_colour_manual(values=country_cols) +
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
                  img(src = "logo.jpeg",height = 130, width=250 )),
             tabPanel(title = "Vaccine Progress Map"),
             tabPanel(title = "US Vaccine Progress"),
             tabPanel(title = "Reaching Herd Immunity"),
             tabPanel(title = "Worldwide Vaccine Progress",
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
                                      choices = c("vaccines(total)", "vaccines(per million)","fully_vaccinated(total)","fully_vaccinated(per million)"),
                                      selected = c("vaccines(total)"),
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
             )
  )

# Define server ----
server <- function(input, output, session) {
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

    if (input$outcome_select=="vaccines(total)") {
      db$outcome = db$people_vaccinated
      db$daily_outcome = db$daily_vaccinations
    }

    if (input$outcome_select=="vaccines(per million)") {
      db$outcome = db$people_vaccinated_per_million
      db$daily_outcome = db$daily_vaccinated_per_million
    }

   if (input$outcome_select=="fully_vaccinated(total)") {
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

}

# Run the app ----
shinyApp(ui = ui, server = server)

