#' @export
availableGeodata <- function(){
  names(geodataMeta())
}

#' @export
geodataMeta <- function(mapName = NULL, load_data = FALSE, debug = FALSE){
  if(!is.null(mapName)) load_data <- TRUE
  dir <- system.file("meta",package="geodata", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- map(files,function(x){
    #x <- files[[1]]
    if(debug) message(basename(x))
    ll <- yaml.load_file(x)
    map(ll, function(y){
      #y <- ll[[1]]
      y$geoname = basename(file_path_sans_ext(x))
      if(!"basename" %in% names(y))
        stop("No basename in yaml: ", y)
      if(load_data){
        codesFilename <- system.file(file.path("geodata",y$geoname,paste0(y$basename, ".csv")),package = "geodata")
        if(debug) message("-- codes: ",codesFilename)
        y$codes = noWrnMsg(read_csv(codesFilename, col_types = cols(id = 'c', name='c')))
        regionFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-regions.csv"))
        if(file.exists(system.file(regionFilename, package = "geodata"))){
          if(debug) message("regions: ",regionFilename)
          r <- noWrnMsg(read_csv(system.file(regionFilename, package = "geodata"), col_types = cols(id = 'c')))
          y$regions <- r
        }
        altnamesFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-altnames.csv"))
        if(file.exists(system.file(altnamesFilename, package = "geodata"))){
          if(debug) message("altnames: ", altnamesFilename)
          r <- noWrnMsg(read_csv(system.file(altnamesFilename, package = "geodata"), col_types = cols(id = 'c')))
          y$altnames <- r
        }
      }
      y
    })
  }) %>% purrr::flatten()
  if(!is.null(mapName)){
    return(l[[mapName]])
  }
  l
}

#' @export
geodataProjections <- function(mapName){
  l <- geodataMeta(mapName)
  names(l$projections)
}

#' @export
geodataProjectionOptions <- function(mapName, projection, withDefaults = TRUE){
  l <- geodataMeta(mapName)
  if(!projection %in% names(l$projections))
    stop(mapName, "does not support this projection")
  projection <- l$projections[[projection]]
  if(!withDefaults) return(names(projection))
  projection
}

#' @export
geodataCsv <- function(mapName){
  geodataMeta(mapName)$codes
}

#' @export
geodataTopojsonPath <- function(mapName){
  y <- geodataMeta(mapName, load_data = FALSE)
  system.file(file.path("geodata",y$geoname,paste0(y$basename, ".topojson")),package = "geodata")
}

#' @export
geodataCsvPath <- function(mapName){
  y <- geodataMeta(mapName, load_data = FALSE)
  system.file(file.path("geodata",y$geoname,paste0(y$basename, ".csv")),package = "geodata")
}


