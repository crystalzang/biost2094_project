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

# building the map

countries_map <- geojson_read("data/custom.geo.json", what = "sp")

# count data for map

vaccines <- read.csv("data/clean.csv")

current_date <- as.Date(max(vaccines$date),"%Y-%m-%d")

vax_today <- subset(vaccines, date==current_date)


country_vaccine_count <- vax_today %>% filter(alpha3 %in% countries$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% countries$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
country_vaccine_count = country_vaccine_count[order(cv_large_countries$alpha3),]

bins <-  c(0,10,50,100,500,1000,Inf)
vax_colors <- colorBin("Oranges", domain = country_vaccine_count$cases_per_million, bins = bins)
plot_map <- countries[countries$ADM0_A3 %in% country_vaccine_count$alpha3, ]

basemap <- leaflet(plot_map) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~country_vaccine_count$,
            title = "<small></small>")

# Putting the map into the ui

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
                   ),
                 tabPanel(title = "Worldwide Vaccine Progress"),
                 tabPanel(title = "Vaccine Progress Map",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),

                              absolutePanel(id = "controls", class = "panel panel-default",
                                            bottom = 75, left = 55, width = 400, fixed=TRUE,
                                            draggable = TRUE, height = "auto",

                                            sliderTextInput("plot_date",
                                                            label = h5("Date"),
                                                            choices = format(unique(cv_cases$date), "%d %b %y"),
                                                            selected = format(current_date, "%d %b %y"),
                                                            grid = FALSE,
                                                            animate=animationOptions(interval = 3000, loop = FALSE))

                              ),

                          )
                 ),
                 tabPanel(title = "US Vaccine Progress"),
                 tabPanel(title = "Reaching Herd Immunity")
)


# Define server logic ----
server <- function(input, output) {
  output$mymap <- renderLeaflet({
    basemap
  })

  observeEvent(input$plot_date, {
  leafletProxy("mymap") %>%
    clearMarkers() %>%
    clearShapes() %>%

    addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5.5),
                     fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                     label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                       textsize = "15px", direction = "auto")) %>%  addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_million))}
)}

# Run the app ----
shinyApp(ui = ui, server = server)

