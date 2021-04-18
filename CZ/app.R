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
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")



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
         pct_pfizer = pfizer_admin/population * 100)

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

#########################################
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
                          img(src = "/Users/czang/Documents/2021Spring/R/biost2094_project/www/logo.jpeg",height = 130, width=250)),
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
                              width=3
                          ),
                          mainPanel(
                            "Data is from",
                            tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/about-vaccine-data.html", "CDC"),
                            fluidRow(
                              column(3, offset = 9,

                                     radioButtons(inputId = "display_option_p1",
                                                  label = "Display:",
                                                  choices = c("Actual Percent", "Quantile"),
                                                  selected = "Actual Percent")
                              )),
                            plotOutput("us_vaccine_plot"),
                            width=9
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

                                 fluidRow(
                                   column(3, offset = 9,

                                          radioButtons(inputId = "display_option",
                                                       label = "Display:",
                                                       choices = c("Actual Percent", "Quantile"),
                                                       selected = "Actual Percent")
                                   )),
                                 plotOutput("us_vaccine_plot2"),
                                 width=9

                               )
                             )

                    )
                    ),

                 tabPanel(title = "Reaching Herd Immunity")
)


# Define server logic ----
server <- function(input, output) {
  output$us_vaccine_plot <- renderPlot({

    plot1 <- ggplot(data = filter(us_states_vaccine, population == input$pop, status == input$vstatus),
    aes(x = long, y = lat,
        group = group, fill = percent,  text = paste0(region,": ",  percent, "% are vaccinated")))+
    geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
    labs(title=paste0("Vaccination Status in the U.S. Among ", input$pop, " Who Had ", input$vstatus),
         x =" ", y = " ")+
      theme_void()+
      scale_fill_gradient2(low = "white", mid = "#67A24D", high = "#3B6824", midpoint = 50,
                           limits=c(0, 100), breaks=seq(0,100,by=20))+
      theme(legend.title = element_text( size = 18),
            legend.text = element_text(size = 13),
            plot.title = element_text(size=18))+
      labs(fill = "Percent (%)")

    colour1 <- "#E5F2DF"
    colour2 <- "#95C182"
    colour3 <- "#519A37"
    colour4 <- "#2D6814"

    colours <- c(colour1, colour2, colour3, colour4)

    plot1.quantile <- ggplot(data = filter(us_states_vaccine, population == input$pop, status == input$vstatus),
                    aes(x = long, y = lat,
                        group = group, fill = percent_q,  text = paste0(region,": ",  percent, "% are vaccinated")))+
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
      labs(title=paste0("Vaccination Status in the U.S. Among ", input$pop, " Who Had ", input$vstatus),
           x =" ", y = " ")+
      theme_void()+
      theme(legend.title = element_text( size = 18),
            legend.text = element_text(size = 13),
            plot.title = element_text(size=18))+
      labs(fill = "Percent (%)")+
      scale_fill_manual(values = colours)

   # ggplotly(plot1, tooltip = "text") %>%
   #     layout(legend = list(font = list(size=11)))
    if (input$display_option_p1 == "Actual Percent"){
      plot1
    }else if (input$display_option_p1 == "Quantile") {
      plot1.quantile
    }
  })

  output$us_vaccine_plot2 <- renderPlot({
    plot2 <- ggplot(data = filter(us_states_brand, brand == input$vbrand),
           aes(x = long, y = lat,
               fill = percent,  text = paste0(region,": ",  percent, "% are vaccinated with ",  input$vbrand)))+
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
      labs(title=paste0("Vaccination Status in the U.S. By ", input$vbrand),
           x =" ", y = " ")+
      theme_void()+
      scale_fill_gradient2(low = "white", mid = "#67A24D", high = "#3B6824",
                           midpoint = 50,limits=c(0, 80), breaks=seq(0,80,by=20))+
      theme(legend.title = element_text( size = 18),
            legend.text = element_text(size = 13),
            plot.title = element_text(size=18))+
      labs(fill = "Percent (%)")

    colour1 <- "#E5F2DF"
    colour2 <- "#95C182"
    colour3 <- "#519A37"
    colour4 <- "#2D6814"
    colours <- c(colour1, colour2, colour3, colour4)

    plot2.quantile <- ggplot(data = filter(us_states_brand, brand == input$vbrand),
                    aes(x = long, y = lat,
                         fill = percent_q,  text = paste0(region,": ",  percent, "% are vaccinated with ",  input$vbrand)))+
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
      labs(title=paste0("Vaccination Status in the U.S. By ", input$vbrand),
           x =" ", y = " ")+
      theme_void()+
      theme(legend.title = element_text( size = 18),
            legend.text = element_text(size = 13),
            plot.title = element_text(size=18))+
      labs(fill = "Percentage Quantile") +
      scale_fill_manual(values = colours)



    if (input$display_option == "Actual Percent"){
      plot2
    }else if (input$display_option == "Quantile") {
      plot2.quantile
    }
    # ggplotly(plot2, tooltip = c("text")) %>%
    #   layout(legend = list(font = list(size=11)))
  })


  }



# Run the app ----
shinyApp(ui = ui, server = server)

