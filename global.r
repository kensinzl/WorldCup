library(tidyverse)
library(rvest)
library(XML)
library(RCurl)

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

# player links 23*32=736
# th a => Selects all <a> elements inside <th> elements
# th > a => Selects all <a> elements where the parent is a <th> element
# html_attrs VS html_attr difference
# Recommd the html_attrs to run and see the result, it fetch all attributes from one node
# , plus html_attr can not use the html_text because will cause the issue
player_links = page_info %>% html_nodes("table.wikitable th > a") %>% html_attr("href")

# https://www.zsccy.xyz/md/2018-04-08-r%E8%AF%AD%E8%A8%80%E5%B9%B6%E8%A1%8C%E5%8C%96%E8%AE%A1%E7%AE%97%E4%B9%8Bforeach%E5%8C%85/
print(">>>>>>>>>>>>> Parallel Satrt")
print(date())
# player image link
# after test, using html_session roughly same speed compaired with directly hitting full https://en.wikipedia.org//wiki/Artem_Dzyuba
getImageAndBirthPlace = function(link) {
  player_page = html_session("https://en.wikipedia.org/") %>% jump_to(link)
  image_link = player_page %>% html_nodes(css = "a.image img") %>% html_attr("src") %>% .[1]
  birth_place = player_page %>% html_nodes(css = "td.birthplace") %>% html_text(trim=TRUE)
  # clean data 1. remove [2] string in the birth place, 2. explicitly setting "" for pages without birth place
  if(length(birth_place) > 0) {
    birth_place = birth_place %>% str_replace(pattern = ".{1}[1-9].{1}", replacement = "")
  } else {
    birth_place = ""
  }
  c(image_link, birth_place)
}

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
player_birthplace_imageLink = read_rds("player_birthplace_imageLink.rds")



# https://cran.r-project.org/web/packages/tidygeocoder/vignettes/tidygeocoder.html
# https://www.maps.ie/coordinates.html
# https://www.gps-coordinates.net/
print(date())
# This way is working, but take 9 mins, so need parallel
# player_birthplace_imageLink %>% geocode(address=birth_place, lat=latitude, long=longitude, method = 'cascade')

player_birthplace_imageLink$country = rep(countries, each = 23)
country_player_list = split(player_birthplace_imageLink, player_birthplace_imageLink$country)

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
p_link_birthPlace_geo = read_rds("p_link_birthPlace_geo.rds")

# merge two data.frames
players_list = merge(p_link_birthPlace_geo, country_player, by=c("No","country"))



players_list = mutate(players_list,
                     link = paste0("https://en.wikipedia.org", player_link),
                     popup_text = paste0("<center>",
                                         ifelse(!is.na(player_link), paste0("<img src = https:", player_link, " width='100'>"), ""),
                                         "</br><b>", Player, "</b>",
                                         "</br><b>Date of birth</b>: ", Birthday,
                                         "</br><b>Place of birth</b>: ", birth_place,
                                         "</br><b>Playing position</b>: ", Position,
                                         "</br><b>Club</b>: ", Club,
                                         "</br></center>"))
 
 
## 2021-01-15: Tested but seems not work well
# #Add a very slight random shock to the latitude and longitude coordinates so that the markers don't end up on top of each other.
## players_list = players_list %>%
##   mutate(latitude = jitter(latitude, amount = 0.02),
##          longitude = jitter(latitude, amount = 0.02))


write_rds(players_list, "players_list.rds")
players_list = read_rds("players_list.rds")



# Setting up icons - Flags taken from https://www.iconfinder.com
# makeIcon - according to players_list rows' sequence to assign the URL
# then leafletProxy(mapId = "mymap", data = filteredData()) or leaflet(mapId = "mymap", data = players_list) also follow the row sequence
# this is the reason why the image not mess up
# here use www folder which is stored for the asset
flagIcon = makeIcon(
  iconUrl = case_when(
    players_list$country == "Russia" ~ "Russia.png",
    players_list$country == "Saudi Arabia" ~ "Saudi_Arabia.png",
    players_list$country == "Egypt" ~ "Egypt.png",
    players_list$country == "Uruguay" ~ "Uruguay.png",
    players_list$country == "Portugal" ~ "Portugal.png",
    players_list$country == "Spain" ~ "Spain.png",
    players_list$country == "Morocco" ~ "Morocco.png",
    players_list$country == "Iran" ~ "Iran.png",
    players_list$country == "France" ~ "France.png",
    players_list$country == "Australia" ~ "Australia.png",
    players_list$country == "Peru" ~ "Peru.png",
    players_list$country == "Denmark" ~ "Denmark.png",
    players_list$country == "Argentina" ~ "Argentina.png",
    players_list$country == "Iceland" ~ "Iceland.png",
    players_list$country == "Croatia" ~ "Croatia.png",
    players_list$country == "Nigeria" ~ "Nigeria.png",
    players_list$country == "Brazil" ~ "Brazil.png",
    players_list$country == "Switzerland" ~ "Switzerland.png",
    players_list$country == "Costa Rica" ~ "Costa_Rica.png",
    players_list$country == "Serbia" ~ "Serbia.png",
    players_list$country == "Germany" ~ "Germany.png",
    players_list$country == "Mexico" ~ "Mexico.png",
    players_list$country == "Sweden" ~ "Sweden.png",
    players_list$country == "South Korea" ~ "South_Korea.png",
    players_list$country == "Belgium" ~ "Belgium.png",
    players_list$country == "Panama" ~ "Panama.png",
    players_list$country == "Tunisia" ~ "Tunisia.png",
    players_list$country == "England" ~ "England.png",
    players_list$country == "Poland" ~ "Poland.png",
    players_list$country == "Senegal" ~ "Senegal.png",
    players_list$country == "Colombia" ~ "Colombia.png",
    players_list$country == "Japan" ~ "Japan.png"
  ),
  iconWidth = 25, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)


# sort the group_country_df via Group from A ~ H
# have to use www folder 
sort_name_df = group_country_df[order(group_country_df$Group, decreasing = FALSE), ]
country_icon = case_when(
  sort_name_df$Country == "Russia" ~ sprintf("<a><img src='Russia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Saudi Arabia" ~ sprintf("<a><img src='Saudi_Arabia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Egypt" ~ sprintf("<a><img src='Egypt.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Uruguay" ~ sprintf("<a><img src='Uruguay.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Portugal" ~ sprintf("<a><img src='Portugal.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Spain" ~ sprintf("<a><img src='Spain.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Morocco" ~ sprintf("<a><img src='Morocco.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Iran" ~ sprintf("<a><img src='Iran.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "France" ~ sprintf("<a><img src='France.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Australia" ~ sprintf("<a><img src='Australia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Peru" ~ sprintf("<a><img src='Peru.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Denmark" ~ sprintf("<a><img src='Denmark.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Argentina" ~ sprintf("<a><img src='Argentina.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Iceland" ~ sprintf("<a><img src='Iceland.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Croatia" ~ sprintf("<a><img src='Croatia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Nigeria" ~ sprintf("<a><img src='Nigeria.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Brazil" ~ sprintf("<a><img src='Brazil.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Switzerland" ~ sprintf("<a><img src='Switzerland.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Costa Rica" ~ sprintf("<a><img src='Costa_Rica.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Serbia" ~ sprintf("<a><img src='Serbia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Germany" ~ sprintf("<a><img src='Germany.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Mexico" ~ sprintf("<a><img src='Mexico.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Sweden" ~ sprintf("<a><img src='Sweden.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "South Korea" ~ sprintf("<a><img src='South_Korea.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Belgium" ~ sprintf("<a><img src='Belgium.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Panama" ~ sprintf("<a><img src='Panama.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Tunisia" ~ sprintf("<a><img src='Tunisia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "England" ~ sprintf("<a><img src='England.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Poland" ~ sprintf("<a><img src='Poland.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Senegal" ~ sprintf("<a><img src='Senegal.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Colombia" ~ sprintf("<a><img src='Colombia.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country),
  sort_name_df$Country == "Japan" ~ sprintf("<a><img src='Japan.png' width=30px></img>&emsp;%s</a>", sort_name_df$Country))

country_icon = c("<img src='earch.png' width=30px></img>" , country_icon)


