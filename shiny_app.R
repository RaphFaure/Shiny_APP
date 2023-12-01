library("shiny")
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(lubridate)
library(sf)

data <- st_read("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new_.geojson")

ui <- fluidPage(
  
  #Title 
  
  titlePanel("Weather Map of Brazil"),
  
  #Tools
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataChoice", "Choose the type of data to display:",
                   choices = c("Temperature" = "Temp_daily", "Precipitation" = "Precip_daily")),
      
      dateRangeInput("dateRange", "Select the date range:",
                     start = min(data$Data))
                     , end = max(data$Data))
    ,
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  data <- st_read("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new-simp_.geojson")
  
  reactiveData <- reactive({
    
    ##Data processing based on user choices
    
    data <- st_read("C:\\Users\\FX506\\Desktop\\CS\\césure\\2e partie\\etude_de_cas_PSR\\brazil-with-states-new-simp_.geojson")  
    
    data$Lat <- as.numeric(gsub(",", ".", data$Lat))
    
    data$Long <- as.numeric(gsub(",", ".", data$Long))
    
    
    data <- filter(data, Data >= input$dateRange[1], Data <= input$dateRange[2])
    
    
    
    ##Cond
    
    
    if (input$dataChoice == "Temp_daily") {
      
      data <- data %>%
        group_by(Reg, geometry) %>%
        summarise(Temp_moyenne = mean(Temp_daily, na.rm = TRUE))
      
    } 
    else if (input$dataChoice == "Precip_daily") {
      
      data <- data %>%
        group_by(Reg, geometry) %>%
        summarise(Precip_moyenne = mean(Precip_daily, na.rm = TRUE))
      
    }
    
    return(data)
  })
  
  output$map <- renderLeaflet({
    
    data <- reactiveData()
    
    # Initialization of the map
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    #Configuration of palettes
    
    pal <- if (input$dataChoice == "Temp_daily") {
      colorNumeric(palette = "inferno", domain = data$Temp_moyenne)
    } else {
      colorNumeric(palette = "Blues", domain = data$Precip_moyenne)
    }
    
    # Adding polygons for the regions.
    if (input$dataChoice == "Temp_daily" || input$dataChoice == "Precip_daily") {
      
      map <- map %>% addPolygons(data = data,
                                 fillColor = ~pal(if (input$dataChoice == "Temp_daily") Temp_moyenne else Precip_moyenne),
                                 weight = 2,
                                 opacity = 1,
                                 color = "white",
                                 dashArray = "3",
                                 fillOpacity = 0.7,
                                 smoothFactor = 0.5,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = "#666",
                                   dashArray = "",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE),
                                 layerId = ~Reg
                                )
      
    }
  })
    #Adding a feature with cliking on a region  
  
    observeEvent(input$map_shape_click, { 
      
      click <- input$map_shape_click
      
      if (!is.null(click)) {
        region_clicked <- click$id  
        
        data <- filter(data, Reg == region_clicked)
        
        
        if (input$dataChoice == "Temp_daily") {
          data <- data %>%
            group_by(city, Lat, Long) %>%
            summarise(Temp_moyenne = mean(Temp_daily, na.rm = TRUE)) #Temperature for each city
          
          leafletProxy("map", data = data) %>%
            clearMarkers() %>%
            addMarkers(lng = ~Long, lat = ~Lat, popup = ~paste(city, "Temp:", Temp_moyenne)) %>%
            setView(lng = click$lng, lat = click$lat, zoom = 7) #Markers for each city 
        }
        else {
          
          data <- data %>%
            group_by(city, Lat, Long) %>%
            summarise(Precip_moyenne = mean(Precip_daily, na.rm = TRUE)) #Precipitation for each city
          
          leafletProxy("map", data = data) %>%
            clearMarkers() %>%
            addMarkers(lng = ~Long, lat = ~Lat, popup = ~paste(city, "Precip:", Precip_moyenne)) %>%
            setView(lng = click$lng, lat = click$lat, zoom = 7) #Markers for each city 
        }
    }
    
    output$info <- renderPrint({
      
      click <- input$map_shape_click
      if (!is.null(click)) {
        paste("Région cliquée:", click$id)
      }
    })
    
    return(map)
  })
  
}      
  
  

shinyApp(ui, server)

