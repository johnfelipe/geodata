
context("folders")


test_that("All folders and files exist",{

  folders <- list.files(system.file("geodata",package = "geodata"))
  yamls <- list.files(system.file("meta",package = "geodata"))
  yamls <- file_path_sans_ext(yamls)
  expect_true(setequal(folders, yamls))

})



context("yaml")


test_that("yaml files have correct structure",{

  ## Define example yaml file with necessary information
  yaml_template <- yaml.load_file(system.file(file.path("tests", "meta_test.yaml") ,package="geodata"))
  yaml_template_attributes <- names(yaml_template$test_map_name)[names(yaml_template$test_map_name) != "projections"]
  yaml_template_projections <- names(yaml_template$test_map_name$projections)
  yaml_template_projections_mercator <- names(yaml_template$test_map_name$projections$mercator)
  yaml_template_projections_equirectangular <- names(yaml_template$test_map_name$projections$equirectangular)

  ## Load in all yaml files
  yaml_files <- list.files(system.file("meta",package="geodata"),pattern = ".*.yaml")
  yaml_elements <- yaml_files %>% purrr::map(function(yaml){
    yaml <- yaml.load_file(system.file(file.path("meta", yaml) ,package="geodata"))
    yaml[[1]]
  })

  ## All attributes exist
  attributes <- yaml_elements %>% purrr::map(names) %>% purrr::reduce(intersect)
  expect_equal(attributes,
               yaml_template_attributes)

  # ## All have mercator and equirectangular projections
  # projections <- yaml_elements %>% purrr::map("projections") %>% purrr::map(names) %>% purrr::reduce(intersect)
  # expect_equal(projections,
  #              yaml_template_projections)
  #
  # ## All mercator projections are complete
  # projections_info_mercator <- yaml_elements %>% purrr::map("projections") %>% purrr::map("mercator") %>% purrr::map(names) %>% purrr::reduce(intersect)
  # expect_equal(projections_info_mercator,
  #              yaml_template_projections_mercator)
  #
  # ## All equirectangular projections are complete
  # projections_info_equirectangular <- yaml_elements %>% purrr::map("projections") %>% purrr::map("equirectangular") %>% purrr::map(names) %>% purrr::reduce(intersect)
  # expect_equal(projections_info_equirectangular,
  #              yaml_template_projections_equirectangular)

})



context("toposjon")


test_that("good topojson",{
  # TODO Test that topojson have scope

  dm <- geodataMeta(load_data = FALSE, debug = FALSE)

  ## All topojson files exist
  missingTopojson <- dm %>% purrr::map(function(dm){
    topojsonPath <- file.path("geodata",dm$geoname,paste0(dm$basename,".topojson"))
    topojsonFile <- system.file(topojsonPath, package = "geodata")
    topojson_exists <- file.exists(topojsonFile)
  }) %>% purrr::keep(isFALSE)
  expect_true(length(missingTopojson) == 0)

  dm <- geodataMeta(load_data = TRUE)

  ## All have id and name props
  dmWithTopojson <- dm[!names(dm) %in% names(missingTopojson)]
  tpdata <- dmWithTopojson %>% purrr::map(function(dm){
    topojsonPath <- file.path("geodata",dm$geoname,paste0(dm$basename,".topojson"))
    topojson <- system.file(topojsonPath, package = "geodata")
    tp <- topojson_read(topojson, quiet = TRUE)
    tp_s4 <- sf::as_Spatial(tp)
    tpdata <- tp_s4@data
    tpdata
  })

  tpdata_names <- tpdata[names(tpdata) != "col_municipalities"] %>% purrr::map(names) %>% purrr::reduce(intersect)
  expect_equal(tpdata_names, c("id"))
})



context("csv")


test_that("csv files are complete",{

  dm <- geodataMeta(load_data = FALSE, debug = FALSE)

  ## All codes CSV files exists
  missingCodes <- purrr::map(dm,"codes") %>% purrr::keep(is.null) %>% names
  missingCodes
  expect_true(length(missingCodes) == 0)

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
