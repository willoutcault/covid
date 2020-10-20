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
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 800, width = 1000) %>% withSpinner(color="#3c8dbc")
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

    
    
    output$map <- renderLeaflet({
        
        covid.data.filtered <- covid.data %>%
            filter(test_date >= input$daterange[1] & test_date <= input$daterange[2]) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        
        date.range <- input$daterange[2] - input$daterange[1]
        
        covid.data.filtered.old <- covid.data %>%
            filter(test_date >= (input$daterange[1]-date.range) & test_date <= (input$daterange[2]-date.range)) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        
        covid.data.filtered$pct_change <- round((covid.data.filtered$new_positives/covid.data.filtered.old$new_positives)
                                                - 1, 2)
            
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        popup <- paste0("<strong>Name: </strong>",merged.spdf@data$name,"<br>",
                        "<strong>Cases: </strong>", merged.spdf@data$new_positives,"<br>",
                        "<strong>Percent Change: </strong>", merged.spdf@data$pct_change)
        
        pal <- colorNumeric(
            palette = "Blues",
            domain = merged.spdf@data$new_positives)
        
        leaflet(merged.spdf) %>% 
            setView(-76, 42.8, 7.4) %>%
            addPolygons(weight = 2, popup=popup, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 1, color = ~pal(new_positives))
            
        
    })
    
    
    
    
    
    output$jobPositionsTable <- renderDataTable({
        
        covid.data.filtered <- covid.data %>%
            filter(test_date >= input$daterange[1] & test_date <= input$daterange[2]) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        merged.spdf.ss <- select(merged.spdf@data, name, total_number_of_tests, cumulative_number_of_tests,
                                 new_positives, cumulative_number_of_positives)
        
        merged.spdf.ss <- unique(covid.data.filtered)
        
        datatable(merged.spdf.ss, options = list(paging = FALSE, scrollY = "225px"), rownames = FALSE)
        
    })
    
    
    
    
    output$jobPositionsTable <- renderDataTable({
        

       # datatable(merged.spdf.ss, options = list(paging = FALSE, scrollY = "225px"), rownames = FALSE)
        
    })
    
    
    output$plot<- renderPlot({
        
        ggplot(count_location(jobdetails()), aes(x=reorder(Location, n),y=(n/sum(n)), label="count")) +
            geom_bar(stat='identity', width=.5, fill = "lightblue", color = "darkblue")  +
            scale_fill_manual(name="Cities Hiring") + 
            coord_flip()+
            labs(title="Top 10 Cities", 
                 subtitle="Based Off Total Open Positions") +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  text = element_text(size=13)) +
            scale_y_continuous(labels = scales::percent) +
            scale_x_discrete(position = "left")
        
    }, height = 188)
    
    
}

shinyApp(ui = ui, server = server)
