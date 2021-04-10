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

#new
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")


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
         percent_of_18_pop_with_at_least_one_dose_by_state_of_residence, percent_of_18_pop_fully_vaccinated_by_state_of_residence, total_number_of_pfizer_doses_delivered, total_number_of_pfizer_doses_adminstered, total_number_of_janssen_doses_delivered, total_number_of_janssen_doses_administered,
         total_number_of_moderna_doses_delivered,
         total_number_of_moderna_doses_administered)%>%
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
         "janssen_deliver" = "total_number_of_janssen_doses_delivered",
         "janssen_admin" = "total_number_of_janssen_doses_administered",
         "moderna_deliver" = "total_number_of_moderna_doses_delivered",
         "moderna_admin" = "total_number_of_moderna_doses_administered"
  )%>%
  mutate( janssen_pct_admin = (janssen_deliver-janssen_admin)/janssen_deliver,
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
                                      "All")))%>%
  gather(key ="population2" , value="percent_fully_vaccinated", pct_fully_vaccinated, pct_65_fully_vaccinated, pct_18_fully_vaccinated )%>%
  select(-population2)%>%
  gather(key = "status", value = "percent", percent_vaccinated, percent_fully_vaccinated)%>%
  mutate(status = if_else(status == "percent_fully_vaccinated", "Fully Vaccinated", "At Least One Dose"))


#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_vaccine <- left_join(us_states, vaccine_us_long, by=c("region" = "state"))



vaccine_brand <- vaccine_us_new%>%
  mutate(population = people_vaccinated/ pct_vaccinated * 100,
         pct_janssen = janssen_admin/population * 100,
         pct_moderna = moderna_admin/population * 100,
         pct_pfizer = pfizer_admin/population * 100)%>%
  gather(key = "brand", value = "percent", pct_pfizer, pct_janssen, pct_moderna)%>%
  mutate(brand = if_else(brand == "pct_janssen", "Janssen",
                         if_else(brand == "pct_moderna", "Moderna", "Pfizer")))

#join with the us state dataset for the plot
us_states <- map_data("state")
us_states_brand <- left_join(us_states, vaccine_brand, by=c("region" = "state"))

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
                 navbarMenu(title = "US Vaccine Progress",
                    tabPanel(title = "US Vaccine Progress",
                        sidebarLayout(
                          sidebarPanel(

                              # Options to select population
                              radioButtons("pop",
                                            label = h3("Population"),
                                            choices = list("Total population" = "All",
                                                             "18+" = "18+",
                                                             "65+" = "65+"),
                                            selected = "All"),

                              # Options to select vaccine status
                              radioButtons("vstatus",
                                           label = h3("Vaccine Status"),
                                           choices = list("At least one dose" = "At Least One Dose",
                                                          "Fully vaccinated" = "Fully Vaccinated"),
                                           selected = "At Least One Dose"),

                          ),
                          mainPanel(
                            "Data is from",
                            tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC"),
                            plotOutput("us_vaccine_plot")
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
                                                             "Janssen" = "Janssen"),
                                              selected = "Pfizer")
                               ),
                               mainPanel(
                                 "Data is from",
                                 tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC"),

                                 plotOutput("us_vaccine_plot2")
                               )
                             )

                    )
                    ),

                 tabPanel(title = "Reaching Herd Immunity")
)


# Define server logic ----
server <- function(input, output) {
  output$us_vaccine_plot <- renderPlot({

    ggplot(data = filter(us_states_vaccine, population == input$pop, status == input$vstatus),
    aes(x = long, y = lat,
        group = group, fill = percent))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Stuatus in the U.S. Among ", input$pop, " Who Had ", input$vstatus),
         x =" ", y = " ")+
      theme_void()
  })

  output$us_vaccine_plot2 <- renderPlot({
    ggplot(data = filter(us_states_brand, brand == input$vbrand),
           aes(x = long, y = lat,
               group = group, fill = percent))+
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
      labs(title=paste0("Vaccination Stuatus in the U.S. By ", input$vbrand),
           x =" ", y = " ")+
      theme_void()

  })


  }



# Run the app ----
shinyApp(ui = ui, server = server)

