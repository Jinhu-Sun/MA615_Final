library(shiny)
library(ggplot2)
library(plotly)
library(gganimate)
library(transformr)
library(leaflet)
library(rworldmap)
library(sf)
library(dplyr)

populationData <- read.csv('cleaned.csv', skip = 1)

ui <- fluidPage(
  titlePanel("Interactive Map with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("countryName", "Enter a Country Name", "Cyprus")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map"),
                 h3("Key facts about Cyprus"),
                 p("1.Strategic Location: Cyprus is strategically located at the crossroads of Europe, Asia, and Africa."),
                 p("2.Cultural Diversity: Influenced by various civilizations, Cyprus has a diverse cultural heritage, with significant Greek and Turkish influences."),
                 p("3.Natural Beauty: The island is known for its beautiful beaches, mountainous landscapes, and mild Mediterranean climate."),
                 h3("Description of the island"),
                 p("Cyprus, an island in the Eastern Mediterranean, is a place rich in history and cultural diversity. Its strategic location at the crossroads of Europe, Asia, and Africa has made it a melting pot of civilizations for millennia. This has contributed to the island's unique cultural heritage, influenced by Greek, Turkish, and other cultures."),
                 p("Economically, Cyprus is known for its robust service sector, tourism, and shipping industries. Its Mediterranean climate, picturesque landscapes, and rich archaeological sites make it a popular tourist destination.")
        ),
        tabPanel("Key Demographics",
                 plotOutput("populationPlot"),
                 p("The population starts at a lower value, presumably around 700,000 before 1960, and increases steadily until it reaches a point just over 1,100,000 after 2020. "),
        ),
        tabPanel("Tourism",
                 h3("Tourism Highlights"),
                 img(src = "tour.jpg, height", height = "500px", width = "100%"),
                 p("The chart above came from https://www.cystat.gov.cy/en/SubthemeStatistics?id=51 I was not able to find the raw data within the data base. So, I will use a picture of the graph displaied on the website.")
        ),
        tabPanel("SWOT Analysis",
                 h3("SWOT Analysis for Cyprus"),
                 tags$img(src = "SWOTanalysisForCyprus.png, height", height = "500px", width = "100%"),
                 p("The chart above came from https://www.cystat.gov.cy/en/SubthemeStatistics?id=51 I was not able to find the raw data within the data base. So, I will use a picture of the graph displaied on the website.")
        )
      )
    )
  )
)




server <- function(input, output) {
  
  # Function to get country boundary
  getCountryBoundary <- function(countryName) {
    data("countriesCoarse", package = "rworldmap")
    country <- countriesCoarse@data %>% 
      filter(SOVEREIGNT == countryName) %>% 
      select(NAME)
    
    if (nrow(country) > 0) {
      countryBoundary <- countriesCoarse[countriesCoarse$SOVEREIGNT == countryName, ]
      return(countryBoundary)
    }
    return(NULL)
  }
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles()
  })
  
  # Update the map based on input
  observe({
    countryName <- input$countryName
    if (countryName != "") {
      countryBoundary <- getCountryBoundary(countryName)
      if (!is.null(countryBoundary)) {
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(data = countryBoundary) %>%
          fitBounds(lng1 = min(countryBoundary@bbox["minx"]),
                    lat1 = min(countryBoundary@bbox["miny"]),
                    lng2 = max(countryBoundary@bbox["maxx"]),
                    lat2 = max(countryBoundary@bbox["maxy"]))
      }
    }
  })
  
  # ... existing server logic ...
  
  output$populationPlot <- renderPlot({
    selectedCountry <- c("Cyprus")
    
    # Filter the dataset for the selected country
    if(selectedCountry %in% names(populationData)) {
      countryData <- populationData[[selectedCountry]]
    } else {
      stop("The selected country does not match any column names.")
    }
    years <- c(1960:2022)
    countryData <- data.frame(Year = years, Population = countryData)
    
    # Plotting the data
    p <- ggplot(countryData, aes(x = Year, y = Population)) +  # make sure you use 'Population' here
      geom_line() + 
      geom_point() +
      theme_minimal() +
      labs(title = paste("Population of", selectedCountry, "over the last 20 years"),
           x = "Year", 
           y = "Population")
    
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)