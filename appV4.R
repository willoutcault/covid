require(tidyverse)
require(lubridate)
require(leaflet)
require(lubridate)
require(sp)
require(shinydashboard)
require(DT)
require(shinycssloaders)
require(sf)
require(plotly)
require(mapview)

covid.data <- read.csv("https://health.data.ny.gov/resource/xdss-u53e.csv?$limit=100000", sep=",", header = T)

county.data <- rgdal::readOGR("https://raw.githubusercontent.com/willoutcault/covid/main/cugir-007865-geojson.json")

polys_sf<-st_read("https://raw.githubusercontent.com/willoutcault/covid/main/cugir-007865-geojson.json") %>% 
    st_transform(crs="+init=epsg:4326")

header <- dashboardHeader(
    title = "NYS COVID-19 TRACKER"
)

body <- dashboardBody(
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
    fluidRow(
        column(width = 3,
               box(width = NULL, status = "warning",
                   selectInput("daterange", "Date Range: ", c("Week", "Month", "Overall"), selected = "Week"),
                   selectInput("county", "County: ", unique(covid.data$county), selected = "Week")
               )
        ),
        column(width = 6,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map") %>% withSpinner(color="#3c8dbc")
               ),
               box(width = NULL,
                   DT::dataTableOutput("jobPositionsTable2",  height = "100%")
               )
        ),
        column(width = 3,
               box(width = NULL,
                   DT::dataTableOutput("jobPositionsTable",  height = "100%")
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
    
    
    # Save test dates as dates
    covid.data$test_date <- as.Date(covid.data$test_date)
    
    # Aggregate by date function
    aggregate <- function(daterange1, daterange2, covid.data){
        covid.data %>%
            filter(test_date >= daterange1 & test_date <= daterange2) %>% 
            group_by(county) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
    }
    
    # Data filter based on leaflet coordinates
    
    
    output$map <- renderLeaflet({
        
        
        if (input$daterange=="Week"){
            daterange <- c(max(covid.data$test_date)-7, max(covid.data$test_date))
        }
        if (input$daterange=="Month"){
            daterange <- c(max(covid.data$test_date) %m-% months(1), max(covid.data$test_date))
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), max(covid.data$test_date))
        }
        
        date.range <- daterange[2] - daterange[1]
        
        covid.data.filtered <- aggregate(daterange[1], daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(daterange[1]-date.range, daterange[2]-date.range, covid.data)
        
        covid.data.filtered$pct_change <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1)*100, 2)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        merged.spdf@data$log_values <- log(merged.spdf@data$new_positives)
        
        popup <- paste0("<h3>",merged.spdf@data$name," County</h1>",
                        
                        "<strong>",format(daterange[1], format="%B %d")," - ",
                        format(daterange[2], format="%B %d")," :</strong>",
                        merged.spdf@data$new_positives, "<br>",
                        
                        "<strong>",format(daterange[1]-date.range, format="%B %d")," - ",
                        format(daterange[2]-date.range, format="%B %d")," :</strong>",
                        merged.spdf@data$old_positives, "<br>",
                        
                        "<strong>Percent Change: </strong>", merged.spdf@data$pct_change,"%")

        
        merged.spdf@data$log_values[which(!is.finite(merged.spdf@data$log_values))] <- 0
        
        pal <- colorNumeric(
            palette = "Oranges",
            domain = merged.spdf@data$log_values)
        
        
        leaflet(merged.spdf) %>% 
            setView(-73.8, 40.9, 8.3) %>%
            addPolygons(weight = 2, popup=popup, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 1, color = ~pal(log_values))
        
    })
    
    
    data_map <- reactive({
        
        
        xmin <- input$map_bounds$west
        xmax <- input$map_bounds$east
        ymax <- input$map_bounds$north
        ymin <- input$map_bounds$south
        
        filt_bbox <- sf::st_bbox(c(xmin = ifelse(is.na(xmin), -180, xmin), 
                                   ymin = ifelse(is.na(ymin),  -90,  ymin), 
                                   xmax = ifelse(is.na(xmax), +180, xmax), 
                                   ymax = ifelse(is.na(ymax),  +90, ymax)), 
                                 crs = st_crs(4326)) %>% 
            sf::st_as_sfc(.)
        
        find_data <- sf::st_within(polys_sf, filt_bbox)
        
        filt_data <- polys_sf[which(lengths(find_data) != 0), ]
        
        county.data <- county.data[county.data$county %in% filt_data$county,]
    })
    
    
    
    output$jobPositionsTable <- renderDataTable({
        if (input$daterange=="Week"){
            daterange <- c(max(covid.data$test_date)-7, max(covid.data$test_date))
        }
        if (input$daterange=="Month"){
            daterange <- c(max(covid.data$test_date) %m-% months(1), max(covid.data$test_date))
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), max(covid.data$test_date))
        }
        
        date.range <- daterange[2] - daterange[1]
        
        covid.data.filtered <- aggregate(daterange[1], daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(daterange[1]-date.range, daterange[2]-date.range, covid.data)
        
        covid.data.filtered$`%` <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1), 2)
        
        covid.data.filtered.ss <- select(covid.data.filtered, county, new_positives, `%`)
        
        colnames(covid.data.filtered.ss) <- c("County", "Cases", "%")
        
        covid.data.filtered.ss %>% 
            datatable(options = list(paging = FALSE, scrollY = "700px"), rownames = FALSE) %>% 
            formatStyle('%', color = styleInterval(c(-.01, .01), c('green', 'white', 'red'))) %>% 
            formatPercentage('%', digits = 0)
    })
    
    output$jobPositionsTable2 <- renderDataTable({
        if (input$daterange=="Week"){
            daterange <- c(max(covid.data$test_date)-7, max(covid.data$test_date))
        }
        if (input$daterange=="Month"){
            daterange <- c(max(covid.data$test_date) %m-% months(1), max(covid.data$test_date))
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), max(covid.data$test_date))
        }
        
        date.range <- daterange[2] - daterange[1]
        
        covid.data.filtered <- aggregate(daterange[1], daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(daterange[1]-date.range, daterange[2]-date.range, covid.data)
        
        covid.data.filtered$`%` <- round(((covid.data.filtered$new_positives/
                                               covid.data.filtered.old$new_positives) - 1), 2)
        
        covid.data.filtered.ss <- select(covid.data.filtered, county, new_positives, `%`)
        
        colnames(covid.data.filtered.ss) <- c("County", "Cases", "%")
        
        covid.data.filtered.ss %>% 
            datatable(options = list(paging = FALSE, scrollY = "700px"), rownames = FALSE) %>% 
            formatStyle('%', color = styleInterval(c(-.01, .01), c('green', 'white', 'red'))) %>% 
            formatPercentage('%', digits = 0)
    })
    
    
    
    
    
}

shinyApp(ui = ui, server = server)
