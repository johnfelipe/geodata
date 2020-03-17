which_in <- function (x, y) x[x %in% y]

noWrnMsg <- function(x){
  suppressWarnings(suppressMessages(x))
}

match_replace <- function (v, dic, na = NA, force = TRUE){
  matches <- dic[[2]][match(v, dic[[1]])]
  out <- matches
  if(!is.na(na)){
    na_to_chr(out, na)
  }
  if (!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}


`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if (class(x) == "character" && nchar(x) == 0)
    return(y)
  else x
}

is.empty <- function (x)
{
  !as.logical(length(x))
}

file_path_sans_ext <- function (x){
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

url_exists <- function(x, printUrl = FALSE){
  r <- !httr::http_error(x)
  if(printUrl)
    message(r, ": ", x)
  r
}
