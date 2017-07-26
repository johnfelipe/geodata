


basePath <- "https://rawgit.com/jpmarindiaz/geo-collection/master"

l <- availableGeodata()

l <- map(l,function(x){
  x$path <- file.path(basePath,x$geoname, x$file)
  x$codesPath <- file.path(basePath,x$geoname,x$codes)
  x
})
