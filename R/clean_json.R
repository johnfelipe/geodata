library(geojsonio)
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)



# arreglo topojson --------------------------------------------------------

# json_file <- read_json('ESP_adm4.json')
#
# leng_json <- length(json_file$objects$ESP_adm4$geometries)
# for(i in 1:leng_json){
#   json_file$objects$ESP_adm4$geometries[[i]]$id <- json_file$objects$ESP_adm4$geometries[[i]]$properties$ID_4
#   json_file$objects$ESP_adm4$geometries[[i]]$properties <- json_file$objects$ESP_adm4$geometries[[i]]$properties[c('NAME_4', 'NAME_1')]
#   names(json_file$objects$ESP_adm4$geometries[[i]]$properties) <- c('name', "division")
#   json_file
# }
#
# json_file <- rjson::toJSON(json_file)
#
# writeLines(json_file,'inst/geodata/esp/esp-municipalities.topojson')
#
# library(tidyverse)
# library(geojsonio)
#
# tj <- rgdal::readOGR("inst/geodata/esp/esp-municipalities.topojson")
# nms <- as.data.frame(topojson_read("inst/geodata/esp/esp-municipalities.topojson"))
# nms <- nms %>%
#   select(-geometry) %>%
#   dplyr::mutate(.id = 0:(nrow(.)-1))
#
# data_map <- fortify(tj) %>%
#   dplyr::mutate(.id = as.numeric(id)) %>%
#   dplyr::select(-id)
#
#
# info_cent <- data_map %>% group_by( .id) %>% summarise(lat = median(lat), lon = median(long))
# data_centroide <- nms %>% left_join(info_cent)
# data_centroide <- data_centroide[,c("id", "name", "division", "lat", "lon")]
# write_csv(data_centroide, "inst/geodata/esp/esp-municipalities.csv")
#
# # Test --------------------------------------------------------------------
#
# topoData <- readLines("inst/geodata/esp/esp-municipalities.topojson") %>% paste(collapse = "\n")
# library(leaflet)
# lf <- leaflet() %>%
#   leaflet(options = leafletOptions(zoomControl = TRUE))
#
# lf %>%
#   addTopoJSON(topoData,
#               weight = 1,
#               color = '#000000',
#               fill = FALSE)
