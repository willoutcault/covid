require(tidyverse)
require(lubridate)
require(leaflet)
require(lubridate)
require(sp)
require(shinydashboard)
require(DT)
require(shinycssloaders)


header <- dashboardHeader(
    title = "NYS COVID-19 TRACKER"
)

body <- dashboardBody(
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
    fluidRow(
        column(width = 8,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 400, width = 500) %>% withSpinner(color="#3c8dbc")
               ),
               box(width = NULL,
                   DT::dataTableOutput("jobPositionsTable")
               )
        ),
        column(width = 4,
               box(width = NULL, status = "warning",
                   dateRangeInput("daterange", "Date range:",
                                  start  = Sys.Date()-7,
                                  end    = Sys.Date(),
                                  min    = "2020-03-01",
                                  max    = Sys.Date()),
                   actionButton("go", "Search")
              )
         )
    )
)


ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)


server <- function(input,output){
    
    
    covid.data <- read.csv("https://health.data.ny.gov/resource/xdss-u53e.csv?$limit=100000", sep=",", header = T)
    county.data <- rgdal::readOGR("https://raw.githubusercontent.com/willoutcault/covid/main/cugir-007865-geojson.json")
    covid.data$test_date <- as.Date(covid.data$test_date)

    county.data@data <- sp::merge(county.data@data, covid.data, by.x="name", by.y="county")
View(county.data@data)    
    
    
    output$map <- renderLeaflet({
        
        county.data.filtered <- county.data
        county.data.filtered@data <- county.data.filtered@data %>%
            filter(test_date >= input$daterange[1] & test_date <= input$daterange[2]) %>% 
            group_by(name) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        
        popup <- paste0("<strong>Name: </strong>",county.data.filtered@data$name,"<br>",
                        "<strong>Cases: </strong>", county.data.filtered@data$new_positives)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data=county.data, weight = 2, fillColor = "yellow", popup=popup)
        
    })
    
    output$jobPositionsTable <- renderDataTable({
        
        county.data.filtered <- county.data
        county.data.filtered@data <- county.data.filtered@data %>%
            filter(test_date >= input$daterange[1] & test_date <= input$daterange[2]) %>% 
            group_by(name) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        datatable(county.data.filtered@data, options = list(paging = FALSE, scrollY = "225px"), rownames = FALSE)
    })
    
    
    
    
}

shinyApp(ui = ui, server = server)