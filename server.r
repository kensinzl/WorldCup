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
                 popup = ~popup_text)
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
      flagIcon$iconUrl <- rep(paste0("Country_flags/", str_replace_all(input$countries, " ", "_"), ".png"), 23)
    }
    flagIcon
  })
  
  # do not forget the parenthesis for the reactive param
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
