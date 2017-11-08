#' # library(geojsonio)# library(readr)# library(dplyr)# library(ggplot2)# library(jsonlite)## Limpieza archivo topojson --------------------------------------------------
#' json_file <- read_json('VEN_adm1.json')
#'
#'
#' leng_json <- length(json_file$objects$VEN_adm1$geometries)
#' for(i in 1:leng_json){
#'     json_file$objects$VEN_adm1$geometries[[i]]$id <- json_file$objects$VEN_adm1$geometries[[i]]$properties$HASC_1
#'     json_file$objects$VEN_adm1$geometries[[i]]$properties <- json_file$objects$VEN_adm1$geometries[[i]]$properties[c('NAME_1')]
#'     names(json_file$objects$VEN_adm1$geometries[[i]]$properties) <- 'name'
#'     json_file
#'     }
#'
#'
#' json_file <- rjson::toJSON(json_file)
#' dir.create('inst/geodata/ven')
#' writeLines(json_file,'inst/geodata/ven/ven-states.topojson')
#'
#'
#' # Creacion de centroides --------------------------------------------------
#'
#'
#'
#' tj <- topojson_read('inst/geodata/ven/ven-states.topojson')
#'
#' data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
#'
#' data_cent <- data_map %>% group_by(.id) %>% summarise(lon= median(long), lat = median(lat))
#' data_cent$.id <- as.character(data_cent$.id)
#' data_info <-tj@data
#' data_info$`.id` <- as.character(rownames(data_info))
#' data_cent <- left_join(data_cent, data_info)
#' data_cent <- data_cent %>% select(id, name, lat, lon)
#' write_csv(data_cent, 'inst/geodata/ven/ven-states.csv')
#'
#'
#'
#'
#' #
#'
#'
#' json_file <- read_json('VEN_adm2.json')
#'
#'
#' leng_json <- length(json_file$objects$VEN_adm2$geometries)
#' for(i in 1:leng_json){
#'   json_file$objects$VEN_adm2$geometries[[i]]$id <- json_file$objects$VEN_adm2$geometries[[i]]$properties$ID_2
#'   json_file$objects$VEN_adm2$geometries[[i]]$properties <- json_file$objects$VEN_adm2$geometries[[i]]$properties[c('NAME_2', 'NAME_1')]
#'   names(json_file$objects$VEN_adm2$geometries[[i]]$properties) <- c('name', 'depto')
#'   json_file
#' }
#'
#'
#' json_file <- rjson::toJSON(json_file)
#'
#' writeLines(json_file,'inst/geodata/ven/ven-districs.topojson')
#'
#'
#' #VEN_adm2
#'
#' #'inst/geodata/col/col-adm2-districs-altnames.csv'
#'
#' tj <- topojson_read('inst/geodata/ven/ven-districs.topojson')
#'
#' data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
#'
#' data_cent <- data_map %>% group_by(.id) %>% summarise(lon= median(long), lat = median(lat))
#'
#' data_cent$.id <- as.character(data_cent$.id)
#' data_info <-tj@data
#' data_info$`.id` <- as.character(rownames(data_info))
#' data_cent <- left_join(data_cent, data_info)
#' data_cent <- data_cent %>% select(id, name, lat, lon)
#' write_csv(data_cent, 'inst/geodata/ven/ven-districs.csv')
#' #


