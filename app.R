library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
library(shiny)
library(plotly)

prices <- read_csv('csv/final.csv')
subzones <- st_read('kml/MP14_SUBZONE_WEB_PL.kml')

toGeocode <- prices %>%
  filter(flat_type == "4 ROOM") %>%
  mutate(year = substr(month, 1, 4)) %>%
  mutate(resale_price = resale_price / 1000) %>%
  mutate(location = paste0(block, " ", street_name))

toGeocode.unique <- toGeocode %>%
  group_by(location) %>%
  summarise(count = n())
geocodedLocations <- read_csv("csv/geocoded.csv")
geocodedPrices <- left_join(geocodedLocations, toGeocode, by = c("location" = "location")) %>%
  filter(lat != "unknown") %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(long))

buffers <- st_read("shp/All Stations Buffers.shp")
priceShapes <- st_as_sf(geocodedPrices, coords = c("lon", "lat"), crs = 4326)
intersection = st_intersection(priceShapes, buffers)

toPlot <- intersection %>%
  filter(NAME == "MOUNTBATTEN" | NAME == "YEW TEE" | NAME == "WOODLANDS MRT STATION" | NAME == "HOLLAND VILLAGE" | NAME == "TELOK BLANGAH" | NAME == "JURONG EAST" | NAME == "PAYA LEBAR MRT STATION" | NAME == "BUONA VISTA" | NAME == "MARYMOUNT" | NAME == "CHOA CHU KANG") %>%
  filter(flat_type == "4 ROOM") %>%
  mutate(year = as.numeric(substr(month, 1, 4))) %>%
  filter(year > 2000) %>%
  filter(year < 2018) %>%
  mutate(resale_price = resale_price / 1000)

ui <- fluidPage(
  titlePanel("Housing Prices vs Stations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("line",
                  label = "Choose an MRT line to display",
                  choices = c("Circle Line", "North East Line",
                              "East West Line", "North South Line"),
                  selected = "Circle"),
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = min(toPlot$year),
                  max = max(toPlot$year),
                  value = min(toPlot$year))
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    toPlot2 <- toPlot %>%
      filter(year == input$year)
    
    ggplot(toPlot2, aes(x = lon, y = lat)) +
      geom_sf(subzones, mapping = aes()) +
      geom_sf(buffers, mapping = aes()) +
      guides(fill=FALSE)
  })
}

shinyApp(ui = ui, server = server)