
# Find name or id
#' @export
geoType <- function(data, map_name) {

  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  d <- homodatum::fringe_d(f)

  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)
  vs <- NULL
  values <- intersect(d[[1]], centroides[["id"]])

  if (identical(values, character(0))||identical(values, numeric(0))) {
    values <- intersect(d[[1]], centroides[["name"]])
    if(!identical(values, character())) vs <- "Gnm"
  } else {
    vs <- "Gcd"
  }
  vs
}


# fake data
#' @export
fakeData <- function(map_name = NULL, by = "name", ...) {
  if (is.null(map_name)) return()
  centroides <- suppressWarnings(geodataMeta(map_name)$codes)
  names_centroides <- names(centroides)
  diff_names <- setdiff(names_centroides, c("id", "name", "lat", "lon"))
  nsample <- nrow(centroides)
  if (nsample > 30) nsample <- 30
  centroides <- centroides[sample(1:nrow(centroides), nsample),]
  if (by == "name" & !(identical(diff_names, character()))) {
    d <- data.frame(name = centroides[[by]],
                    name_addition = centroides[[diff_names]], sample_value = runif(nsample, 33, 333),
                    stringsAsFactors = FALSE)
  } else {
    d <- data.frame(name = sample(centroides[[by]], nsample), sample_value = runif(nsample, 33, 333),
                    stringsAsFactors = FALSE)
  }
  d
}


# fake data points
#' @export
fakepoints <- function(map_name = NULL, ...) {
  if (is.null(map_name)) return()
  lfmap <- geodataMeta(map_name)
  centroides <- data_centroid(lfmap$geoname, lfmap$basename)

  nsample <- nrow(centroides)
  if (nsample > 30) nsample <- 30
  d <- data.frame(lon = sample(centroides$lon, nsample),
                  lat = sample(centroides$lat, nsample),
                  dim_sample = abs(rnorm(nsample, 33, 333)),
                  stringsAsFactors = FALSE)
  d
}


#' standar dataset
#' @export
standar_values <- function(data) {
  l <- map(colnames(data), function(i) iconv(tolower(data[[i]]), to = "ASCII//TRANSLIT"))
  names(l) <- names(data)
  l <- l %>% bind_rows()
  l
}

# find geo code or geo name
#' @export
find_geoinfo <- function(data, centroides) {


  centroides <- centroides %>% select(-lat, -lon)
  centroides <- standar_values(centroides)
  dic_info <- data.frame(names_centroides = c("id", "name", "name_addition", "code_addition"),
                         ftype = c("Gcd", "Gnm", "Gnm", "Gcd"), stringsAsFactors = FALSE)

  info_data <- paste0("^", map(colnames(centroides),
                               function (i) {unique(centroides[[i]])
                               }) %>% unlist(), "$", collapse = "|")

  data <- standar_values(data)


  l <- sapply(colnames(data), function(x) {
    search_info <- !identical(grep(info_data, as.matrix(data[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]
  if (identical(r, character(0))) {
    return() # if is null is not finding consciousness
  } else{
    r # return names of date with geo code o geoname
  }

}


# guess ftypes changed cat by Gnm or Gcd
#' @export
guess_ftypes <- function(data, map_name) {
  #data <- sample_data("Glt-Gln-Num-Cat-Num-Num-Cat")
  if (is.null(map_name))
    stop("Please type a map name")
  if (is.null(data)) return()

  f <- fringe(data)
  d <- homodatum::fringe_d(f)
  dic <- homodatum::fringe_dic(f)
  dic$id <- names(d)

  centroides <- suppressWarnings(geodataMeta(map_name)$codes)
  centroides$id <- iconv(tolower(centroides$id), to = "ASCII//TRANSLIT")
  dif_names <- setdiff(names(centroides), c("id", "name", "lat", "lon"))


  if (!identical(dif_names, character())) {
    col_names <- c("name", dif_names)
    centroides$name <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")
    centroides[[dif_names]] <- iconv(tolower(centroides[[dif_names]]), to = "ASCII//TRANSLIT")
  } else {
    col_names <- c("name")
    centroides$name <- iconv(tolower(centroides$name), to = "ASCII//TRANSLIT")
  }
  var_geo <- find_geoinfo(as.data.frame(data), centroides)
  d <- data[var_geo]
  d <- standar_values(d)

  info_gcd <- paste0("^", centroides$id, "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gcd, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    if(!all(is.na(suppressWarnings(as.numeric(centroides$id))))) {
      max_gcd <- max(centroides$id)
      d_gcd <- r %>%
        map(function(i){max(d[[i]], na.rm = TRUE) <= max(centroides$id)}) %>% unlist()
      r <- r[d_gcd]
    }
    ld<- map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gcd"
    })
  }


  info_gnm <- paste0("^", map(col_names,
                              function (i) {unique(centroides[[i]])
                              }) %>% unlist(), "$", collapse = "|")
  l <- sapply(colnames(d), function(x) {
    search_info <- !identical(grep(info_gnm, as.matrix(d[,x])), integer(0)) == TRUE
  })
  r <- names(l)[l == TRUE]

  if (!identical(r, character(0))) {
    ld<- map(r, function(i) {
      dic$hdType[dic$label == i] <<- "Gnm"
    })
  }

  dic

}
