library(tidyverse)
library(lubridate)
library(readxl)

## first will pull all NO2 data

data.files <- list.files("data",full.names = T)

NO2_MC <- data.files[str_which(data.files, "NO2")]
PM25_MC <- data.files[str_which(data.files, "PM25")]
O3_MC <- data.files[str_which(data.files,"O3")]

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

MC_data <- NO2 %>% left_join(PM2.5) %>% left_join(O3)
saveRDS(MC_data,file = "data/MC_data2013-2017.Rds")


MC_data %>% 
   filter(!is.na(PM25)) %>% 
  count(station)