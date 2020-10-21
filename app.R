require(tidyverse)
require(ggplot2)
require(ggalt)
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
    includeCSS("https://raw.githubusercontent.com/willoutcault/covid/main/style.css"),
    fluidRow(
        column(width = 3,
               box(width = NULL, status = "warning",
                   dateRangeInput("daterange", "Date range:",
                                  start  = Sys.Date()-7,
                                  end    = Sys.Date(),
                                  min    = "2020-03-01",
                                  max    = Sys.Date())
               ),
               
               box(width = NULL, status = "warning",
                   plotOutput("plot1")
               ),
               
               box(width = NULL, status = "warning",
                   plotOutput("plot2")
               )
        ),
        column(width = 6,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 600, width = 1000) %>% withSpinner(color="#3c8dbc")
               ),
               box(width = NULL, solidHeader = TRUE,
                   plotOutput("plot3")
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
    
    aggregate.by.county <- function(daterange1, daterange2, covid.data){
        covid.data %>%
            filter(test_date >= daterange1 & test_date <= daterange2) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))}
        
    aggregate.by.date <- function(daterange1, daterange2, covid.data){
        covid.data %>%
            filter(test_date >= daterange1 & test_date <= daterange2) %>% 
            group_by(test_date) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))}
    
    
    
    output$map <- renderLeaflet({
        
        date.range <- input$daterange[2] - input$daterange[1]
        
        covid.data.filtered <- aggregate.by.county(input$daterange[1], input$daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate.by.county(input$daterange[1]-date.range, input$daterange[2]-date.range, covid.data)
        
        covid.data.filtered$pct_change <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1)*100, 2)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        
        popup <- paste0("<h1>",merged.spdf@data$name," County</h1>",
                        
                        "<strong>",format(input$daterange[1]-date.range, format="%B %d")," - ",
                        format(input$daterange[2]-date.range, format="%B %d")," :</strong>",
                        merged.spdf@data$old_positives, "<br>",
                        
                        "<strong>",format(input$daterange[1], format="%B %d")," - ",
                        format(input$daterange[2], format="%B %d")," :</strong>",
                        merged.spdf@data$new_positives, "<br>",
                        
                        
                        "<strong>Percent Change: </strong>", merged.spdf@data$pct_change,"%")
        
        pal <- colorNumeric(
            palette = "Oranges",
            domain = merged.spdf@data$new_positives)
        
        leaflet(merged.spdf) %>% 
            setView(-76, 42.8, 7.4) %>%
            addPolygons(weight = 2, popup=popup, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 1, color = ~pal(new_positives))
        
        
    })
    
    
    
    output$plot1<- renderPlot({
        
        covid.data.filtered <- aggregate.by.date(input$daterange[1], input$daterange[2], covid.data)
        
        covid.data.filtered <- covid.data.filtered[with(covid.data.filtered,order(-new_positives)),]
        
        covid.data.filtered <- covid.data.filtered[1:20,]
        
        covid.data.filtered <- na.omit(covid.data.filtered)
        
        theme_set(theme_classic())
        
        ggplot(covid.data.filtered, aes(x=reorder(test_date, test_date), y=new_positives, label=new_positives)) + 
            geom_bar(fill = "#0e668b", stat='identity', aes()) + 
            labs(title= "Positive COVID-19 Tests", y = "Positive Tests", x = NULL) + 
            theme(text = element_text(size=16)) +
            coord_flip()
        
        
    })
    
    
    
    
    output$plot2<- renderPlot({
        
        date.range <- input$daterange[2] - input$daterange[1]
        
        covid.data.filtered <- aggregate.by.county(input$daterange[1], input$daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate.by.county(input$daterange[1]-date.range, input$daterange[2]-date.range, covid.data)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        covid.data.filtered <- covid.data.filtered[with(covid.data.filtered,order(-new_positives)),]
        
        covid.data.filtered <- na.omit(covid.data.filtered)
        
        covid.data.filtered <- covid.data.filtered[1:10,]
        
        theme_set(theme_classic())
        
        gg <- ggplot(covid.data.filtered, aes(x=old_positives, xend=new_positives, y=county, group=county)) + 
            geom_dumbbell(color="#a3c4dc", 
                          size=0.75, 
                          point.colour.l="#0e668b") + 
            labs(x=NULL, 
                 y=NULL, 
                 title="Dumbbell Chart", 
                 subtitle="Pct Change: 2013 vs 2014") +
            theme(plot.background=element_rect(fill="#f7f7f7"),
                  panel.background=element_rect(fill="#f7f7f7"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major.y=element_blank(),
                  panel.grid.major.x=element_line(),
                  axis.ticks=element_blank(),
                  legend.position="top",
                  panel.border=element_blank(),
                  text = element_text(size=16))
        plot(gg)
        
    })
    
    output$plot3<- renderPlot({
        
        theme_set(theme_classic())
        
        ggplot(covid.data, aes(x=test_date)) + 
            geom_line(aes(y=county, value=new_positives)) + 
            labs(title="Time Series of Returns Percentage", 
                 subtitle="Drawn from Long Data format", 
                 caption="Source: Economics", 
                 y="Returns %", 
                 color=NULL) + 
            scale_color_manual(labels = c("psavert", "uempmed"), 
                               values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +
            theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),
                  panel.grid.minor = element_blank())
        
    })
    
}

shinyApp(ui = ui, server = server)
