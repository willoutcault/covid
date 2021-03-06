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



header <- dashboardHeader(
    title = "NYS COVID-19 TRACKER"
)

body <- dashboardBody(
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
    fluidRow(
        column(width = 3,
               box(width = NULL, status = "warning",
                   selectInput("daterange", "Date Range: ", c("Week", "Month", "Overall"), selected = "Week")
               ),
               box(width = NULL, status = "warning",
                   selectInput("variable", "Variable: ", c("Daily Cases", "Overall Cases", "Daily Tests", "Overall Tests"), selected = "Daily Cases")
               )
        ),
        column(width = 6,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map") %>% withSpinner(color="#3c8dbc")
               )
        ),
        column(width = 3,
               box(width = NULL, status = "warning",
                   plotOutput("plot1",)
               )
        ),
        column(width = 9,
               box(width = NULL, status = "warning",
                   plotlyOutput("plot2")
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
    
    # Read Data
    covid.data <- read.csv("https://health.data.ny.gov/resource/xdss-u53e.csv?$limit=100000", sep=",", header = T)
    
    county.data <- rgdal::readOGR("https://raw.githubusercontent.com/willoutcault/covid/main/cugir-007865-geojson.json")
    
    polys_sf<-st_read("https://raw.githubusercontent.com/willoutcault/covid/main/cugir-007865-geojson.json") %>% 
        st_transform(crs="+init=epsg:4326")

    
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
            daterange <- c(Sys.Date()-7, Sys.Date())
        }
        if (input$daterange=="Month"){
            daterange <- c(Sys.Date() %m-% months(1), Sys.Date())
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), Sys.Date())
        }
        
        date.range <- daterange[2] - daterange[1]
        
        covid.data.filtered <- aggregate(daterange[1], daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(daterange[1]-date.range, daterange[2]-date.range, covid.data)
        
        covid.data.filtered$pct_change <- round(((covid.data.filtered$new_positives/
                                                      covid.data.filtered.old$new_positives) - 1)*100, 2)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        merged.spdf <- sp::merge(county.data, covid.data.filtered, by.x="name", by.y="county")
        
        popup <- paste0("<h1>",merged.spdf@data$name," County</h1>",
                        
                        "<strong>",format(daterange[1]-date.range, format="%B %d")," - ",
                        format(daterange[2]-date.range, format="%B %d")," :</strong>",
                        merged.spdf@data$old_positives, "<br>",
                        
                        "<strong>",format(daterange[1], format="%B %d")," - ",
                        format(daterange[2], format="%B %d")," :</strong>",
                        merged.spdf@data$new_positives, "<br>",
                        
                        
                        
                        "<strong>Percent Change: </strong>", merged.spdf@data$pct_change,"%")
        
        pal <- colorNumeric(
            palette = "Oranges",
            domain = merged.spdf@data$new_positives)

        
        leaflet(merged.spdf) %>% 
            setView(-73.8, 40.9, 8.3) %>%
            addPolygons(weight = 2, popup=popup, stroke = FALSE, smoothFactor = 0.2,
                        fillOpacity = 1, color = ~pal(new_positives))
        
        
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
    
    
    
    
    output$plot1<- renderPlot({
        
        if (input$daterange=="Week"){
            daterange <- c(Sys.Date()-7, Sys.Date())
        }
        if (input$daterange=="Month"){
            daterange <- c(Sys.Date() %m-% months(1), Sys.Date())
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), Sys.Date())
        }

        
        date.range <- daterange[2] - daterange[1]
        
        covid.data.filtered <- aggregate(daterange[1], daterange[2], covid.data)
        
        covid.data.filtered.old <- aggregate(daterange[1]-date.range, daterange[2]-date.range, covid.data)
        
        covid.data.filtered$old_positives <- covid.data.filtered.old$new_positives
        
        covid.data.filtered$case_count_diff <- covid.data.filtered$new_positives - covid.data.filtered$old_positives
        
        covid.data.filtered$case_count_diff_type <- ifelse(covid.data.filtered$case_count_diff < 0, "good", "bad")
        
        covid.data.filtered <- covid.data.filtered[covid.data.filtered$county %in% data_map()$name,]
        
        ggplot(covid.data.filtered, aes(x=reorder(county, case_count_diff), y=case_count_diff, label=case_count_diff)) + 
            geom_bar(stat='identity', aes(fill=case_count_diff_type))  +
            scale_fill_manual(name="Mileage", 
                              labels = c("More Cases", "Less Cases"), 
                              values = c("good"="#00ba38", "bad"="#f8766d")) + 
            labs(title= paste("This ",input$daterange," Versus Last ", input$daterange, sep="")) + 
            coord_flip()
        
        
    })
    
    
    output$plot2<- renderPlotly({
        
        if (input$daterange=="Week"){
            daterange <- c(Sys.Date()-7, Sys.Date())
        }
        if (input$daterange=="Month"){
            daterange <- c(Sys.Date() %m-% months(1), Sys.Date())
        }
        if (input$daterange=="Overall"){
            daterange <- c(as.Date("2020-03-01", format="%Y-%m-%d"), Sys.Date())
        }

        
        
        covid.data.filtered <- covid.data %>%
            filter(test_date >= daterange[1] & test_date <= daterange[2]) %>%
            group_by(county, test_date) %>%
            summarize("new_positives" = sum(new_positives),
                      "cumulative_number_of_positives" = sum(cumulative_number_of_positives),
                      "total_number_of_tests" = sum(total_number_of_tests),
                      "cumulative_number_of_tests" = sum(cumulative_number_of_tests))
        
        covid.data.filtered <- covid.data.filtered[covid.data.filtered$county %in% data_map()$name,]
        
        
        if(input$variable == "Daily Cases"){
            covid.data.filtered$y <- covid.data.filtered$new_positives
        }
        if(input$variable == "Overall Cases"){
            covid.data.filtered$y <- covid.data.filtered$cumulative_number_of_positives
        }
        if(input$variable == "Daily Tests"){
            covid.data.filtered$y <- covid.data.filtered$total_number_of_tests
        }
        if(input$variable == "Overall Tests"){
            covid.data.filtered$y <- covid.data.filtered$cumulative_number_of_tests
        }


        d <- setNames(covid.data.filtered, names(covid.data.filtered))
        plot_ly(d) %>%
            add_lines(x = ~test_date, y = ~y, color = ~county)
    })
    
    
    
}

shinyApp(ui = ui, server = server)
