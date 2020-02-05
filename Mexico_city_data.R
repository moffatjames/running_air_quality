library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)
library(OpenStreetMap)

## first will pull all NO2 data

data.files <- list.files("data",full.names = T)

NO2_MC <- data.files[str_which(data.files, "NO2")]
PM25_MC <- data.files[str_which(data.files, "PM25")]
O3_MC <- data.files[str_which(data.files,"O3")]
SO2_MC <- data.files[str_which(data.files, "SO2")]

no2_2013 <- read_xls(NO2_MC[1])

mc_reshape <- function(file) {
  read_xls(file) %>% 
  rename("hour" = "HORA",
         "date" = "FECHA") %>% 
    mutate(date = ymd(date)) %>% 
    gather(station,value, -c(hour,date))
  
}

NO2 <- map_dfr(NO2_MC,.f = mc_reshape) %>% rename("NO2" = "value")
PM2.5 <- map_dfr(PM25_MC, .f = mc_reshape) %>%  rename("PM25" = "value")
O3 <- map_dfr(O3_MC, .f = mc_reshape) %>%  rename("O3" = "value")
SO2 <- map_dfr(SO2_MC, .f = mc_reshape) %>%  rename("SO2" = "value")

MC_data <- NO2 %>% left_join(PM2.5) %>% left_join(O3) %>% left_join(SO2)
saveRDS(MC_data,file = "data/MC_data2013-2017.Rds")


MC_data %>% 
   filter(!is.na(PM25)) %>% 
  count(station)

mexico <- c("Álvaro Obregón"
,"Azcapotzalco"
,"Benito Juárez"
,"Coyoacán"
,"Cuajimalpa de Morelos"
,"Cuauhtémoc"
,"Gustavo A. Madero"
,"Iztacalco"
,"Iztapalapa"
,"Magdalena Contreras"
,"Miguel Hidalgo"
,"Milpa Alta"
,"Tláhuac"
,"Tlalpan"
,"Venustiano Carranza"
,"XochimilcoÁlvaro Obregón"
,"Azcapotzalco"
,"Benito Juárez"
,"Coyoacán"
,"Cuajimalpa de Morelos"
,"Cuauhtémoc"
,"Gustavo A. Madero"
,"Iztacalco"
,"Iztapalapa"
,"Magdalena Contreras"
,"Miguel Hidalgo"
,"Milpa Alta"
,"Tláhuac"
,"Tlalpan"
,"Venustiano Carranza"
,"Xochimilco")

mc_cities <- st_read("data/muni_2012gw", "Muni_2012gw")
CDMX <- mc_cities %>% 
  #filter(NOM_MUN %in% mexico) %>% 
  filter(CVE_ENT == "09")

#plot(st_geometry(CDMX))

MC_crs <- st_crs(CDMX)
MC_bbox <- st_bbox(CDMX)

mc_stations <- read_xls("data/MC_stations.xls",sheet = 2) 
 sfc_mc <-  st_as_sf(mc_stations,coords = c("Longitude","Latitude"),crs = MC_crs) %>% filter(Code != "PED")
#mc_stations <- st_transform(mc_stations, st_crs(CDMX))


View(mc_stations)
tmap_mode("view")
tm_basemap(leaflet::providers$OpenTopoMap) +
tm_shape(CDMX) +
  tm_polygons(alpha = 0.7) +
tm_shape(sfc_mc) +
  tm_bubbles("Code",size = 1,col = "red",border.lwd = 1, border.col = "black") +
  tm_text("Code",size = 1.5, xmod = -4.1, ymod =-1.1) + 
  tm_compass(position = c("left","bottom")) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_legend(title = "Mexico city Metropolitan Area\nand select monitoring stations")



mc_map <- get_map("mexico city")

qmap("mexico city", zoom = 14, maptype = 53428, api_key = api_key,
     source = "osm")

bbox_mx <- st_bbox(CDMX)

map <- read_osm(bbox_mx)
