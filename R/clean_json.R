# library(geojsonio)
# library(readr)
# library(dplyr)
# library(ggplot2)
# library(jsonlite)


clean_json <- function(geoName, geoId, geoProperties, newnamesProperties, jsonName, saveFile = TRUE, savePath = NULL) {
  json_file <- read_json(paste0(geoName, '.json'))
  leng_json <- length(json_file$objects[[geoName]][["geometries"]])
  for(i in 1:leng_json){
    json_file$objects[[geoName]][["geometries"]][[i]]$id <- json_file$objects[[geoName]][["geometries"]][[i]][["properties"]][[geoId]]
    json_file$objects[[geoName]][["geometries"]][[i]][["properties"]] <- json_file$objects[[geoName]][["geometries"]][[i]][["properties"]][geoProperties]
    names(json_file$objects[[geoName]][["geometries"]][[i]][["properties"]]) <- newnamesProperties
    json_file
  }

  json_file <- rjson::toJSON(json_file)

  if (saveFile) {
    if (is.null(savePath)) {
      writeLines(json_file,(paste0(path.package("geodata"),"/inst/geodata/", geoName, "/", jsonName, ".topojson")))
    } else {
      writeLines(json_file, paste0(savePath, "/" , jsonName, ".topojson"))
    }
  } else {
    json_file
  }


}


centroids_json <- function(topojsonPath, colsAdditonals) {
  tj <- rgdal::readOGR(topojsonPath)
  nms <- as.data.frame(topojson_read(topojsonPath))
  nms <- nms %>%
    select(-geometry) %>%
    dplyr::mutate(.id = 0:(nrow(.)-1))

  data_map <- fortify(tj) %>%
    dplyr::mutate(.id = as.numeric(id)) %>%
    dplyr::select(-id)


  info_cent <- data_map %>%
    group_by( .id) %>%
    summarise(lat = median(lat), lon = median(long))
  data_centroide <- nms %>%
    left_join(info_cent)

  if (is.null(colsAdditonals)) {
    data_centroide <- data_centroide[,c("id", "name", "continent",  "lat", "lon")]
  } else {
    data_centroide <- data_centroide[,c("id", "name", "lat", "lon", colsAdditonals)]
  }
  data_centroide
}

# change from topojson to rds
# map(seq_along(list.files("inst/geodata/")), function(i) {
#   file_tj <- list.files(paste0("inst/geodata/", list.files("inst/geodata/")[i], "/"), pattern = "topojson")
#   tj <- read_lines(paste0("inst/geodata/", list.files("inst/geodata/")[i], "/", file_tj))
#   saveRDS(tj, paste0("inst/geodata/", list.files("inst/geodata/")[i], "/",  list.files("inst/geodata/")[i], ".rds"))
# })


