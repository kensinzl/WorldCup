# Define server logic required to draw a histogram
# 2021-01-15, test one -> is a fix world map, when you choose the country, the map does not change
#leaflet(players_list)

# test two -> filteredData is reactive so the map will change
#leaflet(data = filteredData()) 

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(data = filteredData()) %>% 
    addTiles() %>% # Add default OpenStreetMap map tiles
    addProviderTiles(providers$CartoDB.Positron) %>% 
    # Set the rectangular bounds of the world map, its diagonal line with the two defined geo points. 
    # [lng1, lat1] - [lng2, lat2]. The map will centralize in this rectangular.
    #fitBounds(lng1 = -165.399182, lat1 = 67.804057, lng2 = 197.927184, lat2 = 0.477900) %>%
    addMarkers(~longitude, ~latitude, 
                 icon = flagIcon, 
                 label = ~Player, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text) %>%
    # Layers control
    addLayersControl(
        overlayGroups = c("France"),
        options = layersControlOptions(collapsed = FALSE)) %>%
    addPolygons(data = players_list %>% filter(country == "France"), lng = ~longitude, lat = ~latitude,
                  fill = F, weight = 2, color = "#FFFFCC", group = "France") 
  })
  
  # every country should have 23 players
  filteredData <- reactive({
    if (input$countries == "All countries") {
      players_list
    } else {
      players_list %>% filter(country == input$countries)
    }
  })
  
  filteredIcon <- reactive({
    if (input$countries == "All countries") {
      flagIcon
    } else {
      # flagIcon$iconUrl has 23 *32 items, but here will use 23 item to override the 23 * 32 items
      flagIcon$iconUrl <- rep(paste0(str_replace_all(input$countries, " ", "_"), ".png"), 23)
    }
    flagIcon
  })
  
  # do not forget the parenthesis for the reactive param
  # 1. reactive have to be as the return value which has to be used in somewhere, if not be used then do not be triggered.
  #    It is a LAZY function.
  #    That means if we changed the following code from observe into reactive, 
  # then this leafletProxy with other codes is not executed until we invoke its return value in somewhere.
  # This is reason why even we choose whatever the country, the flags is always Egypt, Iran and Morocco.
  # Because these countries are the top 23 rows in flagIcon.
  
  # 2. observe is not the LAZY function, it is triggered at the first time.
  observe({
      leafletProxy(mapId = "mymap", data = filteredData()) %>%
      clearMarkers() %>%
      addMarkers(~longitude, ~latitude,
                 icon = filteredIcon(),
                 label = ~Player,
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text)
  })
}
