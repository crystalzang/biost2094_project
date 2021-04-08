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


vaccine_us <- read_csv("/Users/czang/Documents/2021Spring/R/biost2094_project/data/vaccine_us.csv")
vaccine_us <- vaccine_us%>%
  clean_names()

vaccine_us_new <- vaccine_us%>%
  select(state_territory_federal_entity,
         percent_of_total_pop_with_at_least_one_dose_by_state_of_residence,
         percent_of_total_pop_fully_vaccinated_by_state_of_residence,
         percent_of_65_pop_with_at_least_one_dose_by_state_of_residence,
         percent_of_65_pop_fully_vaccinated_by_state_of_residence,
         percent_of_18_pop_with_at_least_one_dose_by_state_of_residence, percent_of_18_pop_fully_vaccinated_by_state_of_residence, total_number_of_pfizer_doses_delivered, total_number_of_pfizer_doses_adminstered, total_number_of_janssen_doses_delivered, total_number_of_janssen_doses_administered,
         total_number_of_moderna_doses_delivered,
         total_number_of_moderna_doses_administered)%>%
  rename("state" = "state_territory_federal_entity",
         "pct_vaccinated" = "percent_of_total_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_fully_vaccinated" = "percent_of_total_pop_fully_vaccinated_by_state_of_residence",
         "pct_65_vaccinated" = "percent_of_65_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_65_fully_vaccinated"= "percent_of_65_pop_fully_vaccinated_by_state_of_residence",
         "pct_18_vaccinated" = "percent_of_18_pop_with_at_least_one_dose_by_state_of_residence",
         "pct_18_fully_vaccinated"= "percent_of_18_pop_fully_vaccinated_by_state_of_residence",
         "pfizer_deliver" = "total_number_of_pfizer_doses_delivered",
         "pfizer_admin"= "total_number_of_pfizer_doses_adminstered",
         "janssen_deliver" = "total_number_of_janssen_doses_delivered",
         "janssen_admin" = "total_number_of_janssen_doses_administered",
         "moderna_deliver" = "total_number_of_moderna_doses_delivered",
         "moderna_admin" = "total_number_of_moderna_doses_administered"
  )%>%
  mutate(janssen_pct_admin = (janssen_deliver-janssen_admin)/janssen_deliver,
         pfizer_pct_admin = (pfizer_deliver - pfizer_admin)/pfizer_deliver ,
         moderna_pct_admin =  (moderna_deliver-moderna_admin)/moderna_deliver)

vaccine_us_new <- apply(vaccine_us_new, 2, as.numeric)
vaccine_us_new <- as.data.frame(vaccine_us_new)
vaccine_us_new$state <- vaccine_us$state_territory_federal_entity
vaccine_us_new$state <-  tolower(vaccine_us_new$state)

vaccine_us_long <- vaccine_us_new%>%
  gather(key ="population" , value="percent_vaccinated", pct_vaccinated, pct_65_vaccinated, pct_18_vaccinated )%>%
  mutate(population = if_else(population == "pct_18_vaccinated", "18+",
                              if_else(population == "pct_65_vaccinated","65+",
                                      "all")))

#join with the us state dataset for the plot
us_states <- map_data("state")
us_states <- left_join(us_states, vaccine_us_long, by=c("region" = "state"))



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
                 tabPanel(title = "US Vaccine Progress",

                    sidebarLayout(
                          sidebarPanel(

                              # Options to select population
                              radioButtons("pop",
                                            label = h3("Population"),
                                            choices = list("Total population" = "all",
                                                             "18+" = "18+",
                                                             "65+" = "65+"),
                                            selected = "all")
                          ),
                          mainPanel(
                            "Data is from",
                            tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC")

                          )
                          )
                          ),
                 tabPanel(title = "Reaching Herd Immunity")
)


# Define server logic ----
server <- function(input, output) {
  output$plot <- renderPlot{
    ggplot(data = filter(us_states, population == input$pop),
           aes(x = long, y = lat,
               group = group, fill = pfizer_pct_admin))+
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  }
  }



# Run the app ----
shinyApp(ui = ui, server = server)

