#' @export
geocode <- function(d, col = NULL, mapName = NULL,
                    result = c("code", "latlon", "all")){
  if(is.null(col)){
    geoname <- which_geoname_column(names(d))
    col <- geoname$column
  }
  if(is.null(mapName)){
    mapName <- geoname$mapName
  }
  # dgeo <- d[col] %>% setNames("_geoname")
  codes <- geodataCodes(mapName = mapName)
  alt_names <- geodataAltnames(mapName = mapName)
  meta <- geodataMeta(mapName = mapName, load_data = FALSE)

  ## Fill id
  codes_col <- meta$codes_name_col
  did <- complete_geoid(d, name_col = col,
                      codes = codes, codes_col = meta$codes_name_col,
                      alt_names = alt_names)
  dgeo <- dplyr::bind_cols(d,did) %>% dplyr::left_join(codes)
  dgeo
}

complete_geoid <- function(d, name_col, codes, codes_col, alt_names){
  dname <- d[name_col]
  names(alt_names) <- c("id", codes_col)
  cods <- codes %>% dplyr::select(one_of("id",codes_col)) %>%
    dplyr::bind_rows(alt_names) %>%
    purrr::set_names(c("id", "..name")) %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::mutate(..name = tolower(..name)) %>%
    dplyr::select(2,1)
  data.frame(id = match_replace(tolower(d[[name_col]]), cods), stringsAsFactors = FALSE)
}







