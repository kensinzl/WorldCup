dashboardPage(
  dashboardHeader(title = "World Cup Demo"),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(text = "Map", tabName = "map", icon = icon("map")),
      menuItem("Charts", icon = icon("bar-chart-o"),
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Here just show a NZ OverLeaf Map
      tabItem("map", 
              leafletOutput(outputId = "mymap", width = "100%", height = "800px"),
              absolutePanel(id = "controls", draggable = FALSE,
                            top = 30, right = 50, width = 280, height = "auto",
                            pickerInput(inputId = "countries", label = "Select a Country:",
                                        choices = list("All countries",
                                                       'Group A' = group_country_df[group_country_df$Group == 'Group A', ]$Country,
                                                       'Group B' = group_country_df[group_country_df$Group == 'Group B', ]$Country,
                                                       'Group C' = group_country_df[group_country_df$Group == 'Group C', ]$Country,
                                                       'Group D' = group_country_df[group_country_df$Group == 'Group D', ]$Country,
                                                       'Group E' = group_country_df[group_country_df$Group == 'Group E', ]$Country,
                                                       'Group F' = group_country_df[group_country_df$Group == 'Group F', ]$Country,
                                                       'Group G' = group_country_df[group_country_df$Group == 'Group G', ]$Country,
                                                       'Group H' = group_country_df[group_country_df$Group == 'Group H', ]$Country
                                                      ),
                                        choicesOpt = list(content = country_icon),
                                        options = list('live-search' = TRUE)
                                      )
                        )
              
              ),
      tabItem("subitem1", "Sub-item 1 tab content"),
      tabItem("subitem2", "Sub-item 2 tab content")
    )
  )
)