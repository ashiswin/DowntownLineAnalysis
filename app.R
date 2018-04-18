library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
library(shiny)
library(plotly)
library(Cairo)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
options(shiny.usecairo=TRUE)
subzones <- st_read('shp/Planning_Area_Census2010.shp')
buffers <- st_read("shp/CCLNEL Multiring Buffers.shp") %>%
  mutate(unique = paste0(OBJECTID, ".", distance))
priceShapes <- st_read("shp/CCLNEL Intersection.shp") %>%
  mutate(unique = paste0(OBJECTID, ".", distance)) %>%
  mutate(psm = resale_p_1 / floor_area) %>%
  mutate(year = as.numeric(substring(quarter, 1, 4))) %>%
  mutate(flat_type = replace(flat_type, flat_type == "MULTI GENERATION", "MULTI-GENERATION"))
stations <- st_read("shp/CCLNEL Stations Dissolved.shp")

colors <- c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
breaks <- c(0, 3, 7, 12, 16, 1200)
labels <- c("0", "3", "7", "12", "12+", "bla")
ui <- fluidPage(
  #tags$head(tags$style(HTML('#sidebar {width: 10px;}'))),
  titlePanel("Housing Prices vs Stations"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      id = "sidebar",
      class = "col-md-offset-2 col-md-10",
      selectInput("line",
                  label = "Choose an MRT line to display",
                  choices = c("Circle Line", "North East Line"),
                  selected = "Circle"),
      radioButtons("flatType", "Flat Type:",
                   choices = unique(priceShapes$flat_type)),
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = 2000,
                  max = max(priceShapes$year),
                  value = 2000)
    ),
    mainPanel(
      style = "margin-top: 0",
      plotOutput(outputId = "distPlot", height = 1000, width = 1280),
      width = 8
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    priceShapes.unique <- priceShapes %>%
      filter(train.st_2 == input$line) %>%
      filter(year == input$year) %>%
      filter(flat_type == input$flatType) %>%
      group_by(unique) %>%
      summarise(median_price = median(psm), count = n())
    
    buffers.medians <- buffers %>%
      left_join(as.data.frame(priceShapes.unique), by = c('unique' = 'unique')) %>%
      drop_na(median_price) %>%
      mutate(median_price = median_price / 1000)
    ggplot() +
      geom_sf(subzones, mapping = aes(), color = "#EEEEEE") +
      geom_sf(stations, mapping = aes(), color = "#CCCCCC") +
      xlim(21000, 37000) +
      ylim(26000, 41000) +
      geom_sf(buffers.medians, mapping = aes(fill = cut(median_price,
                                                        include.lowest = T,
                                                        breaks = breaks),
                                             alpha = cut(count,
                                                         breaks = c(0, 5, 25, 50, 100, 200, 10000))), color = NA) +
      coord_sf(datum = NA) +
      scale_fill_manual(values = colors, labels = labels, drop = F, guide = guide_legend(title = "Median House Price",
                                                              title.theme = element_text(size = 25, angle = 0),
                                                              title.position = "top",
                                                              label.position = "top",
                                                              label.theme = element_text(size = 16, angle = 0))) +
      scale_alpha_discrete(guide = "none") +
      theme_bw() +
      theme(legend.position=c(0.8, 0.1), legend.direction = "horizontal",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.key.height = unit(8, "mm"),
            legend.key.width = unit(24, "mm"))
  })
}

shinyApp(ui = ui, server = server)