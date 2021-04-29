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
if(!require(tidyselect)) install.packages("tidyselect", repos = "http://cran.us.r-project.org")
if(!require(USAboundaries)) install.packages("USAboundaries", repos = "http://cran.us.r-project.org")
if(!require(tools)) install.packages("tools", repos = "http://cran.us.r-project.org")



source("/Users/czang/Documents/2021Spring/R/biost2094_project/CZ/04_vaccine_us_final.R")

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

#########################################
ui <- navbarPage(
  title = "COVID-19 Vaccine",
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

                 tabPanel(title = "Reaching Herd Immunity")
)


# Define server logic ----
server <- function(input, output) {
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

