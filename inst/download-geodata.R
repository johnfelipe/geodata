
# Check if resources exist
basename <- "col-adm1-departments"
required <- c(".csv",".topojson")
optional <- c("-regions.csv","-altnames.csv")

geodata <- geodataMeta() %>% map(`[`,c("basename","geoname")) %>%
  keep(~!is.null(.$basename))

basepath <- "https://rawgit.com/jpmarindiaz/geo-collection/master"
reqExists <- geodata %>% map(function(x){
  files <- paste0(file.path(basepath,x$geoname, x$basename),required)
  map_lgl(unlist(files), url_exists, printUrl = TRUE)
})

stopifnot(all(unlist(reqExists)))


# Download files
geonames <- geodata %>% map_chr("geoname") %>% unique()
file.path("inst","geodata",geonames) %>% map(dir.create)
geodata %>% map(function(x){
  reqfiles <- paste0(file.path(basepath,x$geoname, x$basename),required)
  optfiles <- paste0(file.path(basepath,x$geoname, x$basename),optional)
  optfiles <- optfiles[map_lgl(unlist(optfiles), url_exists)]
  files <- c(reqfiles,optfiles)
  map(files, function(y){
    message("Reading: ",y)
    dest <- file.path("inst","geodata",basename(dirname(y)),basename(y))
    download.file(y, destfile = dest,method="curl")
  })
})



