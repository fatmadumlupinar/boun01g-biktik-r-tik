pti <- c("shiny","tidyverse","leaflet")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}

library(shiny)
library(tidyverse)
library(leaflet)
library(rsconnect)



#reading and preparing data

visitor_data<- read_csv("milliyetlere_gore_ziyaretci_sayisi.csv",guess_max = 100,
                        col_types = cols(
                          "Date" = col_date(format="%Y-%m")
                        ))
visitor_data<-visitor_data[1:150,]

countries_data <-read_csv("countries.csv") #source: https://www.kaggle.com/eidanch/counties-geographic-coordinates

visitor_data_tidy<-visitor_data %>% 
  pivot_longer(.,-Date,names_to="Country",values_to="Total_visitor")


#join

joined_data<-countries_data %>%
  left_join(visitor_data_tidy,by=c("name"="Country"))
joined_data_final<-joined_data[complete.cases(joined_data), ]

ui <- bootstrapPage(
  leafletOutput('map', height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('min_visitor', 'Minimum Visitors', 1, 10000000, 1000),
                dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01"),
                actionButton('show_about', 'About'),
                downloadButton("downloadData", "Download")
  ),
  tags$style(type = "text/css", "
             html, body {width:100%;height:100%}     
             #controls{background-color:white;padding:20px;}
             ")
  )


server <- function(input, output, session) {
  observeEvent(input$show_about, {

    showModal(modalDialog("Using a dataset, which is composed of the number of monthly tourist 
                          visits to Turkey according to their home countries, we created a Shiny app
                          where users can select time range and minimum number of visitors, and explore countries
                          whose people visit Turkey the most.",   title = 'About'))
  })
  rval_number_of_visitors <- reactive({
  
    joined_data_final%>%
      filter(Date>=input$date_range[1],
             Date<=input$date_range[2])%>%
      group_by(name)%>%
      summarise(Date,latitude,longitude,sum_Total_visitor=sum(Total_visitor))%>%
      filter(sum_Total_visitor>=input$min_visitor)

  })

  
  output$map <- renderLeaflet({
    
    rval_number_of_visitors() %>%
      leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView( 41.015137, 28.979530, zoom = 3) %>%
      addCircleMarkers(
        popup = ~ paste("Number of visitors :",as.character(sum_Total_visitor)), radius = ~(sum_Total_visitor)^(1/3)/14,
        color = "red", stroke=FALSE,opacity = 0.01, lng = ~longitude, lat = ~latitude, label = ~name,
        fillOpacity = 0.01
        
      )
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("visitorsbycountry_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rval_number_of_visitors(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)