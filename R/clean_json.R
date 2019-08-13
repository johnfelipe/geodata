#' # #
# library(geojsonio)# library(readr)## library(dplyr)# library(ggplot2)#
# library(jsonlite)#

#' ## Limpieza archivo topojson --------------------------------------------------#json_file <- read_json('COL_adm1.json')###
# json_file <- read_json('colombia.topojson')
# leng_json <- length(json_file$objects$COL_adm1$geometries)#
# for(i in 1:leng_json){
#   json_file$objects$COL_adm1$geometries[[i]]$id <- json_file$objects$COL_adm1$geometries[[i]]$properties$id
#   json_file$objects$COL_adm1$geometries[[i]]$properties <- json_file$objects$COL_adm1$geometries[[i]]$properties[c('NAME_1')]#
#   names(json_file$objects$COL_adm1$geometries[[i]]$properties) <- 'name'#
#   json_file   }###
# json_file <- rjson::toJSON(json_file)
# # dir.create('inst/geodata/TTO')#
# writeLines(json_file,'colombia_aaa.topojson')###
#' # # Creacion de centroides --------------------------------------------------
#' # ####
#' # tj <- topojson_read('inst/geodata/TTO/TTO-districs.topojson')##
#' # data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)##
#' # data_cent <- data_map %>% group_by(.id) %>% summarise(lon= median(long), lat = median(lat))#
#' # data_cent$.id <- as.character(data_cent$.id)#
#' # data_info <-tj@data#
#' # data_info$`.id` <- as.character(rownames(data_info))#
#' # data_cent <- left_join(data_cent, data_info)#
#' # data_cent <- data_cent %>% select(id, name, lat, lon)#
#' # write_csv(data_cent, 'inst/geodata/TTO/TTO-districs.csv')#
#'
#' #'
#' #' #
#' #'
#' #'
#' #' json_file <- read_json('TTO_adm2.json')
#' #'
#' #'
#' #' leng_json <- length(json_file$objects$TTO_adm2$geometries)
#' #' for(i in 1:leng_json){
#' #'   json_file$objects$TTO_adm2$geometries[[i]]$id <- json_file$objects$TTO_adm2$geometries[[i]]$properties$HASC_2
#' #'   json_file$objects$TTO_adm2$geometries[[i]]$properties <- json_file$objects$TTO_adm2$geometries[[i]]$properties[c('NAME_2', 'NAME_1')]
#' #'   names(json_file$objects$TTO_adm2$geometries[[i]]$properties) <- c('name', 'depto')
#' #'   json_file
#' #' }
#' #'
#' #'
#' #' json_file <- rjson::toJSON(json_file)
#' #'
#' #' writeLines(json_file,'inst/geodata/TTO/TTO-departaments.topojson')
#' #'
#' #'
#' #' #TTO_adm2
#' #'
#' #' #'inst/geodata/col/col-adm2-provinces.csv'
#' #' #'
#' tj <- topojson_read('inst/geodata/tto/tto-regions.topojson')
#' #
#' data_map <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
#'
#' data_cent <- data_map %>% group_by(.id) %>% summarise(lon= median(long), lat = median(lat))
#'
#' data_cent$.id <- as.character(data_cent$.id)
#' data_info <-tj@data
#' data_info$`.id` <- as.character(rownames(data_info))
#' data_cent <- left_join(data_cent, data_info)
#' data_cent <- data_cent %>% select(id, name, lat, lon)
#' write_csv(data_cent, 'inst/geodata/tto/tto-regions.csv')
#' #
#'
#'

# library(devtools)
# load_all()
# document()
# install()
# library(geodata)

# mapName <- 'col_departments'
# tmp <-  topojson_read(geodataTopojsonPath(mapName))
#
# #tmp <- topojson_read('col.topojson')
# x <- geojson_list(tmp)
# # View(tmp)
# #
# # # world regions
# wr <- read_csv('inst/geodata/col/col-adm1-regions.csv')
#
# # asia
# asia <- wr %>% filter(region == 'Andina')
# asia$id <- ifelse(asia$name == 'ANTIOQUIA',gsub('5', '05', asia$id), asia$id)
# cen <- read_csv('inst/geodata/col/col-adm1-departments.csv')
# cen <- cen %>% filter(id %in% asia$id)
# # write_csv(cen, 'myfule.csv')
# tmp2 <- filter(tmp, id %in% asia$id)
#
# topojson_write(tmp2, "myfile.topojson")
# tmp <-  topojson_read("myfile.topojson")
# topoData <- readLines("myfile.topojson") %>% paste(collapse = "\n")
# lf <- leaflet() %>%
#   leaflet(options = leafletOptions(zoomControl = TRUE))
# b_box <- geojson::bbox_get(topoData)
# #
# lf %>%
#   addTopoJSON(topoData,
#               weight = 1,
#               color = '#000000',
#               fill = FALSE)%>%
#    setView(lng = mean(c(b_box[1],b_box[3])),lat = mean(c(b_box[2], b_box[4])), zoom = 2)

#
