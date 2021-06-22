

test_that("list with centroids and topojson information",{
  info <- availableGeodata()[1]
  purrr::map(info, ~geoinfo(.x))
})
