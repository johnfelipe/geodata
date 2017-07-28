#' @export
availableGeodata <- function(){
  names(geodataMeta())
}

#' @export
geodataMeta <- function(mapName = NULL, load_data = FALSE){
  if(!is.null(mapName)) load_data <- TRUE
  dir <- system.file("meta",package="geodata", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- map(files,function(x){
    #x <- files[[1]]
    ll <- yaml.load_file(x)
    map(ll, function(y){
      #y <- ll[[1]]
      y$geoname = basename(file_path_sans_ext(x))
      if(!"basename" %in% names(y))
        stop("No basename in yaml: ", y)
      if(load_data){
        codesFilename <- system.file(file.path("geodata",y$geoname,paste0(y$basename, ".csv")),package = "geodata")
        y$codes = noWrnMsg(read_csv(codesFilename, col_types = cols(id = 'c', name='c')))
        regionFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-regions.csv"))
        if(file.exists(system.file(regionFilename, package = "geodata"))){
          r <- noWrnMsg(read_csv(system.file(regionFilename, package = "geodata"), col_types = cols(id = 'c')))
          y$regions <- r
        }
        altnamesFilename <- file.path("geodata",y$geoname,paste0(y$basename, "-altnames.csv"))
        if(file.exists(system.file(altnamesFilename, package = "geodata"))){
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
