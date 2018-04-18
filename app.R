library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
library(shiny)
library(Cairo)
options(shiny.usecairo=TRUE)

subzones <- st_read('shp/Planning_Area_Census2010.shp')
buffers <- st_read("shp/CCLNEL Multiring Buffers.shp") %>%
  mutate(unique = paste0(OBJECTID, ".", distance))
priceShapes <- st_read("shp/CCLNEL Intersection.shp") %>%
  mutate(unique = paste0(OBJECTID, ".", distance)) %>%
  mutate(psm = resale_p_1 / floor_area) %>%
  mutate(year = as.numeric(substring(quarter, 1, 4))) %>%
  mutate(flat_type = replace(flat_type, flat_type == "MULTI GENERATION", "MULTI-GENERATION")) %>%
  filter(flat_type != "MULTI-GENERATION" & flat_type != "EXECUTIVE")
stations <- st_read("shp/CCLNEL Stations Dissolved.shp")
stations.centroid <- st_centroid(stations) %>%
  select(NAME)

colors <- c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
breaks <- c(0, 0.2, 2, 2.6, 3.2, 1200)
labels <- c("0", "0.2", "2", "2.6", "3.2+", "4+", "bla")
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
      radioButtons("modelType", "Model:",
                   choices = c("Actual Prices", "Predicted Prices")),
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

server <- function(input, output, session) {
  observe({
    val <- input$modelType
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    if(input$modelType == "Predicted Prices") {
      updateSliderInput(session, "year", value = input$year, min = 2000, max = 2025, step = 1)
    }
    else {
      updateSliderInput(session, "year", value = input$year, min = 2000, max = max(priceShapes$year), step = 1)
    }
  })
  output$distPlot <- renderPlot({
    if(input$modelType == "Actual Prices") {
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
        geom_sf(subzones, mapping = aes(), fill = "#FFB848", color = "#EEEEEE") +
        geom_sf(stations, mapping = aes(), color = "#CCCCCC") +
        xlim(21000, 37000) +
        ylim(26000, 41000) +
        geom_sf(buffers.medians, mapping = aes(fill = cut(median_price,
                                                          include.lowest = T,
                                                          breaks = breaks),
                                               alpha = cut(count,
                                                           breaks = c(0, 5, 10, 10000))), color = NA) +
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
    }
    else {
      priceShapes.unique <- priceShapes %>%
        filter(train.st_2 == input$line) %>%
        filter(year == input$year) %>%
        filter(flat_type == input$flatType) %>%
        group_by(unique) %>%
        as.data.frame() %>%
        left_join(as.data.frame(stations.centroid), by = c("NAME", "NAME")) %>%
        rowwise()  %>%
        mutate(age = year - lease_comm) %>%
        mutate(years_since_line = year - as.numeric(lineData$completion[lineData$lines == train.st_2])) %>%
        mutate(actual_distance = st_distance(geometry.x, geometry.y)) %>%
        mutate(unique2 = paste0(block, " ", street_nam)) %>%
        ungroup()
        
      for(i in 1:nrow(priceShapes.unique)) {
        priceShapes.unique$years_since_line[i] <- priceShapes.unique$year[i] - lineData$completion[lineData$lines == priceShapes.unique$train.st_2[i]]
      }
      
      priceShapes.unique <- priceShapes.unique %>%
        group_by(block, street_nam) %>%
        summarize(min_dist = min(actual_distance)) %>%
        mutate(unique2 = paste0(block, " ", street_nam)) %>%
        left_join(priceShapes.unique, by = c("unique2", "unique2"))
      
      priceShapes.unique$f = predict(model, newdata = priceShapes.unique)
      
      priceShapes.unique <- priceShapes.unique %>%
        group_by(unique) %>%
        summarise(median_price = median(f), count = n())
      
      buffers.medians <- buffers %>%
        left_join(priceShapes.unique, by = c('unique' = 'unique')) %>%
        drop_na(median_price) %>%
        mutate(median_price = median_price / 1000)
      ggplot() +
        geom_sf(subzones, mapping = aes(), fill = "#FFB848", color = "#EEEEEE") +
        geom_sf(stations, mapping = aes(), color = "#CCCCCC") +
        xlim(21000, 37000) +
        ylim(26000, 41000) +
        geom_sf(buffers.medians, mapping = aes(fill = cut(median_price,
                                                          include.lowest = T,
                                                          breaks = breaks),
                                               alpha = cut(count,
                                                           breaks = c(0, 5, 10, 10000))), color = NA) +
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
    }
  })
}

shinyApp(ui = ui, server = server)