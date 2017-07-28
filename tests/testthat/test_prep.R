context("Resources")


test_that("Resource exists",{

  # Check all regions have proper codes

  dm <- geodataMeta(load_data = TRUE)
  dmWithRegions <- dm %>% keep(~!is.null(.$regions))
  dmReg <- dmWithRegions %>% map("regions")
  dmReg <- dmWithRegions %>% map("regions")

  # all codes have names: id, name, lat, lon
  expect_equal (map(dm, "codes") %>% map(names) %>% reduce(intersect),
                c("id","name"))
                #c("id","name","lat","lon"))
# all regions have names: region, id
  expect_equal (map(dmReg, names) %>% reduce(intersect),
                c("id","name","lat","lon"))

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
})


context("Topojson")

test_that("Good topojson",{
  # Test that topojson have scope
})
