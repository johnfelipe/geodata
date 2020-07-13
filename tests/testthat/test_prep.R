context("Resources")


test_that("Resource exists",{

  # Check folder and yaml structure

  folders <- list.files(system.file("geodata",package = "geodata"))
  yamls <- list.files(system.file("meta",package = "geodata"))
  yamls <- file_path_sans_ext(yamls)
  expect_true(setequal(folders, yamls))

  dm <- geodataMeta(load_data = FALSE, debug = FALSE)

  # All codes csv's exists
  missingCodes <- purrr::map(dm,"codes") %>% purrr::keep(is.null) %>% names
  missingCodes
  expect_true(length(missingCodes) == 0)

  dm <- geodataMeta(load_data = TRUE)

  # Check Topojsons OK
  ## Check they all have id and name props

  mapName <- "latam_countries"
  topojsonPath <- file.path("geodata",dm$latam_countries$geoname,paste0(dm$latam_countries$basename,".topojson"))
  topojson <- system.file(topojsonPath, package = "geodata")
  tp <- topojson_read(topojson)
  tp_s4 <- sf::as_Spatial(tp)
  tpdata <- tp_s4@data
  expect_true(all(c("id","name") %in% names(tpdata)))


  dm <- geodataMeta(load_data = TRUE)

  # All CSVs with names c("id","name","lat","lon")
  incompleteCSVs <- purrr::map(dm, "codes") %>%
    purrr::map(names) %>%
    purrr::keep(~ !all(c("id","name","lat","lon") %in% .))
  expect_true(length(incompleteCSVs) == 0)


  # Check all codes have names: id, name, lat, lon
  expect_equal(purrr::map(dm, "codes") %>% purrr::map(names) %>% purrr::reduce(intersect),
               c("id","name","lat","lon"))

  # Check all regions have proper codes
  dmWithRegions <- dm %>% purrr::keep(~!is.null(.$regions))
  dmReg <- dmWithRegions %>% purrr::map("regions")

  # all regions have names: region, id
  expect_equal(purrr::map(dmReg, names) %>% purrr::reduce(intersect),
                c("region","id"))

  dmRegIdsNoCodes <- dmWithRegions %>% purrr::map(function(dm){
    df_regions <- dm$regions
    df_regions %>% dplyr::filter(!id %in% dm$codes$id)
  })

  whichRegionsWithWrongCode <- dmRegIdsNoCodes %>% purrr::keep(~nrow(.) != 0) %>% purrr::map(~ unique(.$region))
  nms <- unlist(whichRegionsWithWrongCode)
  dmRegIdsNoCodes
  message("Regions with wrong code", paste(names(nms), nms))
  expect_true(length(nms) == 0)

})


context("geodataMeta")

test_that("geodataMeta loads properly",{

  geodatas <- availableGeodata()
  map <- "bra_states"
  dmap_bra <- geodataMeta(map)
  #map(geodatas, geodataMeta)
  dmap <- geodataMeta("col_municipalities")

  # All topojson exist
  expect_true(
    all(purrr::map_lgl(availableGeodata(), ~file.exists(geodataTopojsonPath(.))))
    )
  expect_true(
    all(purrr::map_lgl(availableGeodata(), ~file.exists(geodataCsvPath(.))))
  )

})


context("Topojson")

test_that("Good topojson",{
  # Test that topojson have scope
})
