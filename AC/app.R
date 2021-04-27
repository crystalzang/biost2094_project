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

rm(list = ls())

for (pkg in c("tidyverse", "readr", "dplyr", "countrycode")) {library(pkg, character.only = TRUE)}

coronavirus_summary <- read.csv("C:/Users/Alexis Cenname/Desktop/Advanced R Computing/biost2094_project/data/coronavirus_summary.csv")
coronavirus_summary$iso_code <- countrycode(coronavirus_summary$country, 'country.name','iso3c')
coronavirus_summary <- select(coronavirus_summary, -country)
vaccine <- read.csv("C:/Users/Alexis Cenname/Desktop/Advanced R Computing/biost2094_project/data/vaccinations.csv")
name_list <- c("OWID_ENG", "OWID_NIR", "OWID_SCT", "OWID_WLS")
vaccine$iso_code <- with(vaccine, replace(iso_code, iso_code %in% name_list, "GBR"))
combine <- merge(vaccine, coronavirus_summary, by="iso_code", all.x=T)
coronavirus_daily <- read.csv("C:/Users/Alexis Cenname/Desktop/Advanced R Computing/biost2094_project/data/coronavirus_daily.csv")
vac_daily <- merge(coronavirus_daily, vaccine, by=c("country", "date"))

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
                          "Henry Thrope",
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          img(src = "logo.jpeg",height = 130, width=250)),
                 tabPanel(title = "Worldwide Vaccine Progress"),
                 tabPanel(title = "Vaccine Progress Map"),
                 tabPanel(title = "US Vaccine Progress"),
                 tabPanel(title = "Reaching Herd Immunity",
                          sidebarLayout(
                            sidebarPanel(

                              # Options to select population
                              selectInput("countrylist",
                                          label = h4("Country"),
                                          choices = vac_daily %>% drop_na(date, active_cases) %>% select(country),
                                          selected = "Afghanistan")),
                            mainPanel(plotOutput("countryplot"), plotOutput("countryplot2"), plotOutput("countryplot3"))
                          )
                 ))

server <- function(input, output) {

  output$countryplot <- renderPlot(ggplot(vac_daily %>% filter(country==input$countrylist) %>%
                                            drop_na(active_cases),
                                          aes(date, active_cases))
                                   + geom_point(color="red", size=5) + geom_line(group=1, color="black") + theme_bw() +
                                     theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1), legend.position = "none",
                                           axis.title.x = element_blank()) +
                                     labs(title = as.character(input$countrylist), y = "Active Cases"))
  output$countryplot2 <- renderPlot(ggplot(vac_daily %>% filter(country==input$countrylist) %>%
                                             drop_na(daily_vaccinations),
                                           aes(date, daily_vaccinations))
                                    + geom_point(color="green", size=5) + geom_line(group=1, color="black") + theme_bw() +
                                      theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1), legend.position = "none",
                                            axis.title.x = element_blank()) +
                                      labs(y = "Daily Vaccinations"))
  output$countryplot3 <- renderPlot(ggplot(vac_daily %>% filter(country==input$countrylist) %>%
                                             drop_na(total_vaccinations),
                                           aes(date, total_vaccinations))
                                    + geom_point(color="purple", size=5) + geom_line(group=1, color="black") + theme_bw() +
                                      theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1), legend.position = "none",
                                            axis.title.x = element_blank()) +
                                      labs(y = "Total Vaccinations")
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
