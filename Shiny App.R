#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pti <- c("shiny","tidyverse","leaflet")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(leaflet)
library(knitr)
library(markdown)


#read data
air_bnb_data <- read.csv("https://github.com/pjournal/boun01g-biktik-r-tik/blob/gh-pages/AB_NYC_2019.csv?raw=true")
air_bnb_data

# Prepare data
air_bnb_data_filtered <- 
    air_bnb_data %>% 
    select(neighbourhood_group,neighbourhood,latitude,longitude,room_type,number_of_reviews,price,name) 
air_bnb_data_filtered
# Get neighbourhood list
neighbourhood_groups <- 
    air_bnb_data_filtered %>% 
    distinct(neighbourhood_group) %>% 
    unlist(.)

names(neighbourhood_groups) <- NULL

#get roomtype list

room_types <- 
    air_bnb_data_filtered %>% 
    distinct(room_type) %>% 
    unlist(.)

names(room_types) <- NULL





# Define UI for application that draws scatterplot
ui <- fluidPage(
    
    # Application title
    titlePanel("Airbnb Houses"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            sliderInput("price",
                        "Select price interval",
                        min = 0,
                        max = 10000,
                        value = c(50,200),
                        sep=""),
            selectInput(inputId="neighbourhood_group",label="Select Neighbourhood",choices=c("All",neighbourhood_groups),selected="All",multiple=TRUE),
            sliderInput("number_of_reviews","At least X reviews",
                        min=min(air_bnb_data_filtered$number_of_reviews),
                        max=max(air_bnb_data_filtered$number_of_reviews),
                        value=0),
            selectInput(inputId="room_type",label="Select room type",choices=room_types)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            #tabs
            tabsetPanel(type="tabs",
            tabPanel('Map',leafletOutput('map')),            
            tabPanel("Scatter Plot",plotOutput("scatterplot")),
            tabPanel("Table",tableOutput("table")),
            tabPanel("The Mosts",uiOutput("markdown"))
            
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scatterplot <- renderPlot({
        
        print(input$price)
        print(input$neighbourhood_group)
        print(input$number_of_reviews)
        print(input$room_type)
        
        
        if(!("All" %in% input$neighbourhood_group)){
            plot_df<-air_bnb_data_filtered    %>%
                filter(neighbourhood_group %in% input$neighbourhood_group) %>%
                filter(number_of_reviews>input$number_of_reviews)   %>%
                filter(price>=input$price[1] & price<=input$price[2]) %>%
                filter(room_type %in% input$room_type)
        }
        else{
            plot_df<-air_bnb_data_filtered     %>%
                filter(number_of_reviews>input$number_of_reviews) %>%
                filter(price>=input$price[1] & price<=input$price[2]) %>%
                filter(room_type %in% input$room_type)
            
        }
        
        ggplot(plot_df,aes(x=longitude,y=latitude,color=neighbourhood_group))+geom_point()+
         labs(color="Neighbourhood Group", x="Longitude",y="Latitude")
    })
    output$table <- renderTable({
        if(!("All" %in% input$neighbourhood_group)){
            plot_df<-air_bnb_data_filtered    %>%
                filter(neighbourhood_group %in% input$neighbourhood_group) %>%
                filter(number_of_reviews>input$number_of_reviews)   %>%
                filter(price>=input$price[1] & price<=input$price[2]) %>%
                filter(room_type %in% input$room_type)
        }
        else{
            plot_df<-air_bnb_data_filtered     %>%
                filter(number_of_reviews>input$number_of_reviews) %>%
                filter(price>=input$price[1] & price<=input$price[2]) %>%
                filter(room_type %in% input$room_type)
            
        }
        plot_df
    })
    
    output$map <- renderLeaflet({
      
      if(!("All" %in% input$neighbourhood_group)){
        plot_df<-air_bnb_data_filtered    %>%
          filter(neighbourhood_group %in% input$neighbourhood_group) %>%
          filter(number_of_reviews>input$number_of_reviews)   %>%
          filter(price>=input$price[1] & price<=input$price[2]) %>%
          filter(room_type %in% input$room_type)
      }
      else{
        plot_df<-air_bnb_data_filtered     %>%
          filter(number_of_reviews>input$number_of_reviews) %>%
          filter(price>=input$price[1] & price<=input$price[2]) %>%
          filter(room_type %in% input$room_type)
        
      }
      content <- paste("Price:", plot_df$price)
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(data = plot_df,lat = ~latitude, lng = ~longitude ,popup = ~name, clusterOptions = markerClusterOptions(), label = content)
    })
    output$markdown <-renderUI({
      HTML(markdown::markdownToHTML(knit("airbnb.Rmd",quiet=TRUE)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)