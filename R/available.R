#' @export
availableGeodata <- function(){
  names(geodataMeta())
}

#' @export
geodataMeta <- function(mapName = NULL){
  dir <- system.file("geodata",package="geodata", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- map(files,function(x){
    ll <- yaml.load_file(x)
    map(ll, function(y){
      y$geoname = basename(file_path_sans_ext(x))
      y
    })
  }) %>% flatten()
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
