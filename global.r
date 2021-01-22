# library(tidyverse)
# library(rvest)
# library(XML)
# library(RCurl)

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

wiki_URL = "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads"
page_info = read_html(wiki_URL)

# html_nodes VS html_node difference, html_node just pick the first one
groups = page_info %>% html_nodes(css = "div.toc li.toclevel-1 > a") %>% html_text(trim=TRUE) %>%
  lapply( function(group) {
    ifelse(str_detect(group, pattern = "[1-8].Group.*"),
            str_replace(group, pattern = "[1-8].{1}", replacement = ""),
              NA ) }) %>% unlist %>% na.omit

countries = page_info %>% html_nodes(css = "div.toc li.toclevel-1 li.toclevel-2 a span:nth-child(2)") %>%
            html_text(trim=TRUE) %>% unlist %>% .[1:32]

#  lapply(groups, function(group) { rep(group, each=4) }), each and times is difference, each is everyone repeat but times is whole repeat
groups_four_rep = lapply(groups, function(group) { replicate(4, group) }) %>% unlist

# df <Group> <Country>
group_country_df = data.frame(Group = groups_four_rep, Country = countries)

# <No> <Pos.> <Player> <Date of birth (age)> <Caps> <Goals> <Club>
need_column = c("Pos.", "Player", "Date of birth (age)", "Club")
# df <Position> <Player> <Birthday> <Club>
country_player = page_info %>% html_nodes(css = "table") %>% html_table %>%
                    .[1:32] %>% do.call(rbind, .) %>% select(need_column) %>%
                      rename(Position = "Pos.", Birthday = "Date of birth (age)")
country_player$No = seq(1 : nrow(country_player))
country_player$country = rep(countries, each = 23)

# # player links 23*32=736
# # th a => Selects all <a> elements inside <th> elements
# # th > a => Selects all <a> elements where the parent is a <th> element
# # html_attrs VS html_attr difference
# # Recommd the html_attrs to run and see the result, it fetch all attributes from one node
# # , plus html_attr can not use the html_text because will cause the issue
# player_links = page_info %>% html_nodes("table.wikitable th > a") %>% html_attr("href")
# 
# # https://www.zsccy.xyz/md/2018-04-08-r%E8%AF%AD%E8%A8%80%E5%B9%B6%E8%A1%8C%E5%8C%96%E8%AE%A1%E7%AE%97%E4%B9%8Bforeach%E5%8C%85/
# print(">>>>>>>>>>>>> Parallel Satrt")
# print(date())
# # player image link
# # after test, using html_session roughly same speed compaired with directly hitting full https://en.wikipedia.org//wiki/Artem_Dzyuba
# getImageAndBirthPlace = function(link) {
#   player_page = html_session("https://en.wikipedia.org/") %>% jump_to(link)
#   image_link = player_page %>% html_nodes(css = "a.image img") %>% html_attr("src") %>% .[1]
#   birth_place = player_page %>% html_nodes(css = "td.birthplace") %>% html_text(trim=TRUE)
#   # clean data 1. remove [2] string in the birth place, 2. explicitly setting "" for pages without birth place
#   if(length(birth_place) > 0) {
#     birth_place = birth_place %>% str_replace(pattern = ".{1}[1-9].{1}", replacement = "")
#   } else {
#     birth_place = ""
#   }
#   c(image_link, birth_place)
# }
# 
# # 1. detect the number of CPU cores which is double value of physical number. eg: 4 cores will be 4 * 2
# # 2. using one less core. do not ask me why
# sink("Parallel.text")
# cluster = makeCluster(detectCores() - 1)
# registerDoParallel(cluster)
# p_link_birthPlace = foreach(player_link = player_links,
#                   .inorder = TRUE, # default
#                   .packages = c("rvest", "magrittr", "stringr"),
#                   .combine="rbind",
#                   .verbose = TRUE) %dopar% getImageAndBirthPlace(player_link)
# stopCluster(cluster)
# print(date())
# sink()
# print(">>>>>>>>>>>>> Parallel End")
# 
# p_link_birthPlace_df = data.frame(p_link_birthPlace) %>% rename(player_link = "X1", birth_place = "X2")
# p_link_birthPlace_df$No = seq(1 : nrow(p_link_birthPlace_df)) # No is key
# # after reviewing the result some player link may NA
# write_rds(p_link_birthPlace_df, "player_birthplace_imageLink.rds")
# #unlink("Parallel.text")
# player_birthplace_imageLink = read_rds("player_birthplace_imageLink.rds")
# 
# 
# 
# # https://cran.r-project.org/web/packages/tidygeocoder/vignettes/tidygeocoder.html
# # https://www.maps.ie/coordinates.html
# # https://www.gps-coordinates.net/
# print(date())
# # This way is working, but take 9 mins, so need parallel
# # player_birthplace_imageLink %>% geocode(address=birth_place, lat=latitude, long=longitude, method = 'cascade')
# 
# player_birthplace_imageLink$country = rep(countries, each = 23)
# country_player_list = split(player_birthplace_imageLink, player_birthplace_imageLink$country)
# 
# # test1: 4-5 mins
# print(date())
# cluster = makeCluster(detectCores() - 1)
# registerDoParallel(cluster)
# p_link_birthPlace_geo = foreach(player_info = country_player_list,
#                             .inorder = TRUE, # default
#                             .packages = c("tidygeocoder", "magrittr"),
#                             .combine="rbind",
#                             .verbose = TRUE) %dopar% {
#                               # after test, change the sub-list into data.frame
#                               data.frame(player_info) %>% geocode(address=birth_place, lat=latitude, long=longitude, method = 'cascade')
#                             }
# stopCluster(cluster)
# print(date())
# write_rds(p_link_birthPlace_geo, "p_link_birthPlace_geo.rds")
# p_link_birthPlace_geo = read_rds("p_link_birthPlace_geo.rds")
# 
# # merge two data.frames
# players_list = merge(p_link_birthPlace_geo, country_player, by=c("No","country"))
# 
# 
# 
# players_list = mutate(players_list,
#                      link = paste0("https://en.wikipedia.org", player_link),
#                      popup_text = paste0("<center>",
#                                          ifelse(!is.na(player_link), paste0("<img src = https:", player_link, " width='100'>"), ""),
#                                          "</br><b>", Player, "</b>",
#                                          "</br><b>Date of birth</b>: ", Birthday,
#                                          "</br><b>Place of birth</b>: ", birth_place,
#                                          "</br><b>Playing position</b>: ", Position,
#                                          "</br><b>Club</b>: ", Club,
#                                          "</br></center>"))
# 
# 
# 2021-01-15: Tested but seems not work well
# #Add a very slight random shock to the latitude and longitude coordinates so that the markers don't end up on top of each other.
# players_list = players_list %>%
#   mutate(latitude = jitter(latitude, amount = 0.02),
#          longitude = jitter(latitude, amount = 0.02))
# write_rds(players_list, "players_list.rds")


players_list = read_rds("players_list.rds")



# Setting up icons - Flags taken from https://www.iconfinder.com
# makeIcon - according to players_list rows' sequence to assign the URL
# then leafletProxy(mapId = "mymap", data = filteredData()) or leaflet(mapId = "mymap", data = players_list) also follow the row sequence
# this is the reason why the image not mess up
flagIcon = makeIcon(
  iconUrl = case_when(
    players_list$country == "Russia" ~ "Country_flags/Russia.png",
    players_list$country == "Saudi Arabia" ~ "Country_flags/Saudi_Arabia.png",
    players_list$country == "Egypt" ~ "Country_flags/Egypt.png",
    players_list$country == "Uruguay" ~ "Country_flags/Uruguay.png",
    players_list$country == "Portugal" ~ "Country_flags/Portugal.png",
    players_list$country == "Spain" ~ "Country_flags/Spain.png",
    players_list$country == "Morocco" ~ "Country_flags/Morocco.png",
    players_list$country == "Iran" ~ "Country_flags/Iran.png",
    players_list$country == "France" ~ "Country_flags/France.png",
    players_list$country == "Australia" ~ "Country_flags/Australia.png",
    players_list$country == "Peru" ~ "Country_flags/Peru.png",
    players_list$country == "Denmark" ~ "Country_flags/Denmark.png",
    players_list$country == "Argentina" ~ "Country_flags/Argentina.png",
    players_list$country == "Iceland" ~ "Country_flags/Iceland.png",
    players_list$country == "Croatia" ~ "Country_flags/Croatia.png",
    players_list$country == "Nigeria" ~ "Country_flags/Nigeria.png",
    players_list$country == "Brazil" ~ "Country_flags/Brazil.png",
    players_list$country == "Switzerland" ~ "Country_flags/Switzerland.png",
    players_list$country == "Costa Rica" ~ "Country_flags/Costa_Rica.png",
    players_list$country == "Serbia" ~ "Country_flags/Serbia.png",
    players_list$country == "Germany" ~ "Country_flags/Germany.png",
    players_list$country == "Mexico" ~ "Country_flags/Mexico.png",
    players_list$country == "Sweden" ~ "Country_flags/Sweden.png",
    players_list$country == "South Korea" ~ "Country_flags/South_Korea.png",
    players_list$country == "Belgium" ~ "Country_flags/Belgium.png",
    players_list$country == "Panama" ~ "Country_flags/Panama.png",
    players_list$country == "Tunisia" ~ "Country_flags/Tunisia.png",
    players_list$country == "England" ~ "Country_flags/England.png",
    players_list$country == "Poland" ~ "Country_flags/Poland.png",
    players_list$country == "Senegal" ~ "Country_flags/Senegal.png",
    players_list$country == "Colombia" ~ "Country_flags/Colombia.png",
    players_list$country == "Japan" ~ "Country_flags/Japan.png"
  ),
  iconWidth = 25, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)




