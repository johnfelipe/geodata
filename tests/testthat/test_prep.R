context("Resources")


test_that("Resource exists",{

  geoCodesFiles <- geodataMeta() %>% map("codesPath")
  codeFileFound <- map_lgl(geoCodesFiles, function(x) !httr::http_error(x))
  codeFileFound %>% keep(. == FALSE)
  expect_true(all(codeFileFound))

  geoFiles <- geodataMeta() %>% map("path")
  geoFileFound <- map_lgl(geoFiles, function(x) !httr::http_error(x))
  geoFileFound %>% keep(. == FALSE)
  expect_true(all(codeFileFound))

  # Check all regions have proper codes

  dm <- geodata:::geodataMeta()
  dmWithRegions <- dm %>% keep(~!is.null(.$regions))
  dmReg <- dmWithRegions %>% map(function(x){
    x$codes = readgeodataCode(x)
    x
  })

  # all codes have names: id, name, alternativeNames
  map(dmReg, "codes") %>% map(names)
  expect_equal (map(dmReg, "codes") %>% map(names) %>% reduce(intersect),
                c("id","name","alternativeNames"))

  dmRegIdsNoCodes <- dmReg %>% map(function(geodata){
    df_regions <- map(geodata$regions, function(x){data_frame(ids=x$ids)}) %>% bind_rows(.id = "region")
    df_regions %>% filter(!ids %in% geodata$codes$id)
  })

  whichRegionsWithWrongCode <- dmRegIdsNoCodes %>% keep(~nrow(.) != 0) %>% map(~ unique(.$region))
  nms <- unlist(whichRegionsWithWrongCode)
  dmRegIdsNoCodes
  message("Regions with wrong code", paste(names(nms), nms))
  expect_true(length(nms) == 0)

})

context("Opts")


test_that("opts",{

  mapName <- "world_countries"
  geodata <- geodataMeta(mapName)

  opt1 <- getOpts(geodata)

  # take the first projection by default
  expect_equal(opt1$projectionOpts, geodata$projections[[1]])

  #defaultOpts <- getDefaultOpts()
  #expext_equal(opt1, defaultOpts)

  opts <- list(
    projectionOpts = list(
      scale = 0.15,
      center = c(0,0),
      translate = c(0,0),
      rotate = c(0,0),
      distance = 1.5,
      clipAngle = 60,
      tilt = 25
    )
  )
  getOpts(geodata, opts)


  opts <- list(
    projectionName = "orthographic",
    projectionOpts = list(
      scale = 2,
      center = c(10,10),
      translate = c(10,10),
      rotate = c(10,10),
      distance = 1.5,
      clipAngle = 80,
      tilt = 15
    )
  )
  expect_error(getOpts(geodata, opts = list(projectionName = "xxx")), "Projection not defined for the given geodata")

  opt2 <- getOpts(geodata, opts)
  expect_equal(opts$projectionName, opt2$projectionName)
  expect_equal(opts$projectionOpts$scale, opt2$projectionOpts$scale)
  expect_equal(opts$projectionOpts$center, opt2$projectionOpts$center)
  expect_equal(sort_list(opt2$projectionOpts), sort_list(opts$projectionOpts))

  # Make sure chorolegend is categoric/numeric depending on input data
  d <- data_frame(countryName = c("Colombia","United States","Germany", "Flatland"),
                  countryCode = c("COL","USA","DEU", "XXX"),
                  value = c(1,2,3, 4),
                  continent = c("America","America","Europe", "XXX"))
  geodata <- geodataMeta("world_countries")
  data <- geodata:::makeGeoData(geodata, data = d,
                              regionCols = "countryName",
                              valueCol = "value")
  dopts <- getOpts(geodata, opts = NULL, data = data)
  expect_equal(dopts$legend$choropleth$type, "numeric")

  data <- geodata:::makeGeoData(geodata, data = d,
                              regionCols = "countryName",
                              groupCol = "continent")
  dopts <- getOpts(geodata, opts = NULL, data = data)
  expect_equal(dopts$legend$choropleth$type, "categorical")

  opts <- list(
    legend = list(left=0,top=60,orient="horizontal", title = "HEELLO"),
    title = list(text="Hello world"),
    notes = list(text="Lorem sdfa;lfk fd[osfdsa f alkre re e re re er r a sfdfaa",
                 top = 70),
    tooltip = list(choropleth = list(template = "NAMEEE: {country}"))
    ,styles = "svg{background-color:#eee}"
  )
  getOpts(geodata, opts = opts)

  opts=list(title = list(text="Hola",left=90))
  getOpts(geodata, opts = opts)

})


context("Parameters")

test_that("params",{

  opts = NULL
  regionCols = NULL
  codeCol = NULL
  groupCol = NULL
  valueCol = NULL
  regions = NULL
  bubbles = NULL

  mapName <- "world_countries"
  geodata <- geodataMeta(mapName)

  expect_true(is.null(makeGeoData(geodata, data = NULL)))

  data <- data_frame(countryName = c("Colombia","United States","Germany", "Flatland"),
                     countryCode = c("COL","USA","DEU", "XXX"),
                     value = c(1,2,3, 4),
                     valueChr = c('1','2','3','4'),
                     continent = c("America","America","Europe", "XXX"))

  expect_error(makeGeoData(geodata, data = data,
                           regionCols = NULL, codeCol = NULL),
               "Need to provide regionCols or codeCol")

  expect_error(makeGeoData(geodata, data = data,
                           regionCols = "xxx"))
  expect_error(makeGeoData(geodata, data = data,
                           codeCol = "xxx"))
  expect_error(makeGeoData(geodata, data = data,
                           codeCol = "countryCode"),
               "Need to povide a groupCol or a valueCol")
  expect_error(makeGeoData(geodata, data = data,
                           codeCol = "countryCode",
                           valueCol = "valueChr"),
               "valueCol must be numeric")

  geodata <- geodataMeta(mapName)
  g1 <- makeGeoData(geodata, data = data,
                    regionCols = "countryName", valueCol = "value")
  expect_false(any(is.na(g1$..code)))
  g2 <- makeGeoData(geodata, data = data,
                    codeCol = "countryCode", valueCol = "value")
  expect_false(any(is.na(g2$..code)))

  ## TEST make geo data with no NA's

  expect_equal(names(g1), names(g2))
  expect_equal(names(g1), c("..code","..name","..info","..value","..group",names(data)))
  expect_equal(g1 %>% select(..code, ..name), g2 %>% select(..code, ..name))

  mapName <- "col_municipalities"
  data <- read_csv(system.file("data/co/alcaldias-partido-2011.csv", package = "geodata"))
  geodata <- geodataMeta(mapName)

  g <- makeGeoData(geodata, data = data,
                   regionCols = c("Municipio","Departamento"), groupCol = "Aval Partidos 2011")
  gRegion <- makeGeoData(geodata, data = data,
                         regionCols = c("Municipio","Departamento"), groupCol = "Aval Partidos 2011",
                         region = c("Catatumbo", "Valle de Aburrá"))
  all(gRegion$..code %in% c(geodata$regions$Catatumbo$ids,geodata$regions$`Valle de Aburrá`$ids))

  # Provide bubbles
  # geodata("co_departments")
  # geodata("co_municipalities")
  # geodata("world_countries")
  #
  # getAvailableRegions("co_municipalities")
  # getAvailableProjections("co_municipalities")



})
