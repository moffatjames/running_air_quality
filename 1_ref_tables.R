library(stringr)
library(rvest)


# Identifying site options - Scraping the website for defaults
url <- "http://www.airqualityontario.com/history/"

webpage <- read_html(url)

webpage %>%  
  html_nodes('p')

cities <- webpage %>% 
  html_node('#s') %>% 
  as.character() %>% 
  str_split('option value=\"') %>% 
  as_vector()

station_names <-
  cities[str_which(cities,'[:digit:]">[a-zA-Z0-9 .]+</option')] %>% 
  str_match_all(pattern = '>[a-zA-Z0-9 .]+</option') %>% 
  unlist() %>%  
  str_remove("</option") %>% 
  str_remove('>')

station_ids <-  str_match_all(cities,"[:digit:]{5}") %>% unlist() 

Stations <- tibble(Station_Name =station_names, Station_ID = station_ids)

pollutants <- webpage %>% 
  html_node('#p') %>% 
  as.character() %>%
  str_split("option id") %>% 
  as_vector()

pollutant_name <-
  pollutants[str_which(pollutants,'p0',negate = T)] %>%
  str_match_all(pattern = '[a-zA-Z0-9 -().]+</option') %>%
  unlist() %>%  
  str_remove("</option")%>% 
  str_replace(pattern ="level",replacement = "Ground-level")

pollutant_code <-
  pollutants[str_which(pollutants,'p0',negate = T)] %>%
  str_match_all(pattern = 'p[:digit:]{1,3}') %>% 
  unlist() %>% 
  str_replace(pattern = "p",replacement = "p=")

pollution_codes <- tibble(pollutant = pollutant_name, poll_code = pollutant_code) 

rm(webpage,cities,station_names,station_ids,pollutants,pollutant_name,pollutant_code)

