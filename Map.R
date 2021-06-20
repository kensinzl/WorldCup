library(jsonlite)
library(rvest)
library(stringr)
library(magrittr)
library(dplyr)
library(doParallel)
library(foreach)
library(readr)
library(tidygeocoder)
library(shinyWidgets)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rgdal)






# geojson <- readLines("/Users/zhaoliang/Downloads/WorldCup/countries.geojson", warn = FALSE) %>%
#   paste(collapse = "\n") %>%
#   fromJSON(simplifyVector = FALSE)


countries = readOGR("/Users/zhaoliang/Downloads/WorldCup/countries.geojson")
#pal = colorNumeric(palette = "Blues", domain = countries$pop_est)
pal = colorBin("Blues", domain = countries$gdp_md_est, bins = 6, pretty = FALSE)

leaflet(countries) %>%
# Base groups
addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(provider = providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("USA", "color", "legend","Outline"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
# Overlay
  addMarkers(-77.036873, 38.907192, popup = "Washington machine", label = "Washington machine", group = "USA") %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, color = ~pal(gdp_md_est), 
              highlight = highlightOptions(
                weight = 15,
                fillOpacity = 0.8,
                bringToFront = TRUE), 
              label = sprintf("<strong>%g</strong>", countries$pop_est) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "color")%>%
  addLegend("bottomright", pal = pal, values = ~gdp_md_est,
            title = "Est. GDP (2010)",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1, group = "legend"
  ) %>% 
  # we can set the sub_data_set for each addPolygons
  addPolygons(data = quakes[chull(quakes$long, quakes$lat),], lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") 

