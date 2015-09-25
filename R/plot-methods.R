#' @export
plot.site <- function(x, database = NULL, ...) {
  if(is.null(database)){
    plot(x$long, x$lat, xlab = "Longitude", ylab = "Latitude", ...)
  }  else {
    plot(x$long, x$lat, xlab = "Longitude", ylab = "Latitude", type = 'n')
#    map(database, add = TRUE)
    points(x$long, x$lat, ...)
    
  }
}

#' @export
plot.dataset <- function(x, ...) {
    site <- get_site(x)
    plot(site, ...)
}

#' @export
plot.dataset_list <- function(x, ...) {
  site <- get_site(x)
  plot(site, ...)
}

#' @export
plot.download <- function(x, ...) {
  site <- get_site(x)
  plot(site, ...)
}

#' @export
plot.download_list <- function(x, ...) {
  site <- get_site(x)
  plot(site, ...)
}
