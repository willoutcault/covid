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
                   leafletOutput("map", height = 600, width = 1000) %>% withSpinner(color="#3c8dbc")
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
        ),
        column(width = 4,
               box(width = NULL, height = 650, status = "warning",
                   plotOutput("plot1",),
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
    
    aggregate <- function(daterange1, daterange2, covid.data){
        covid.data %>%
            filter(test_date >= daterange1 & test_date <= daterange2) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
    }
    
    
    
    
    output$map <- renderLeaflet({
        
        date.range <- input$daterange[2] - input$daterange[1]
        
        covid.data.filtered <- aggregate(input$daterange[1], input$daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(input$daterange[1]-date.range, input$daterange[2]-date.range, covid.data)
        
        covid.data.filtered$pct_change <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1)*100, 2)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        
        popup <- paste0("<strong>Name: </strong>",merged.spdf@data$name,"<br>",
                        "<strong>Cases: </strong>", merged.spdf@data$new_positives,"<br>",
                        "<strong>Old Cases: </strong>", merged.spdf@data$old_positives,"<br>",
                        "<strong>Percent Change: </strong>", merged.spdf@data$pct_change,"%")
        
        pal <- colorNumeric(
            palette = "Oranges",
            domain = merged.spdf@data$new_positives)
        
        leaflet(merged.spdf) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            setView(-76, 42.8, 7.4) %>%
            addPolygons(weight = 2, popup=popup, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 1, color = ~pal(new_positives))
        
        
    })
    
    
    
    
    
    
    output$plot1<- renderPlot({
        
        date.range <- input$daterange[2] - input$daterange[1]
        
        covid.data.filtered <- aggregate(input$daterange[1], input$daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(input$daterange[1]-date.range, input$daterange[2]-date.range, covid.data)
        
        covid.data.filtered$pct_change <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1)*100, 2)
        
        covid.data.filtered$pct_change_type <- ifelse(covid.data.filtered$pct_change < 0, "decrease", "increase")
    
        
        ggplot(covid.data.filtered, aes(x=reorder(county, pct_change), y=pct_change, label=pct_change)) + 
            geom_bar(stat='identity', aes(fill=pct_change_type))  +
            scale_fill_manual(name="Mileage", 
                              labels = c("Increase", "Decrease"), 
                              values = c("increase"="#f8766d", "decrease"="#00ba38")) + 
            labs(title= "Percent Change in COVID-19 Cases") + 
            coord_flip()
        
        
    }, height = 650)
    
    
    output$plot2<- renderPlot({
        
        
        
        
    }, height = 650)
    
    
    
}

shinyApp(ui = ui, server = server)
