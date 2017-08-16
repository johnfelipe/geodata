context("Resources")


test_that("Resource exists",{

  # Check folder and yaml structure

  folders <- list.files(system.file("geodata",package = "geodata"))
  yamls <- list.files(system.file("meta",package = "geodata"))
  yamls <- file_path_sans_ext(yamls)
  expect_true(setequal(folders, yamls))

  # All CSVs with names c("id","name","lat","lon")
  incompleteCSVs <- map(dm, "codes") %>%
    map(names) %>%
    keep(~ !all(c("id","name","lat","lon") %in% .))
  expect_true(length(incompleteCSVs) == 0)

  # Check Topojsons OK
  ## Check they all have id and name props

  # Check all regions have proper codes

  dm <- geodataMeta(load_data = TRUE)
  dmWithRegions <- dm %>% keep(~!is.null(.$regions))
  dmReg <- dmWithRegions %>% map("regions")


  # all codes have names: id, name, lat, lon
  expect_equal(map(dm, "codes") %>% map(names) %>% reduce(intersect),
               c("id","name","lat","lon"))

  # all regions have names: region, id
  expect_equal(map(dmReg, names) %>% reduce(intersect),
                c("region","id"))

  dmRegIdsNoCodes <- dmWithRegions %>% map(function(dm){
    df_regions <- dm$regions
    df_regions %>% filter(!id %in% dm$codes$id)
  })

  whichRegionsWithWrongCode <- dmRegIdsNoCodes %>% keep(~nrow(.) != 0) %>% map(~ unique(.$region))
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
    all(map_lgl(availableGeodata(), ~file.exists(geodataTopojsonPath(.))))
    )
  expect_true(
    all(map_lgl(availableGeodata(), ~file.exists(geodataCsvPath(.))))
  )

})


context("Topojson")

test_that("Good topojson",{
  # Test that topojson have scope
})
