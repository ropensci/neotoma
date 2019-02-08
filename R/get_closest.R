#' Find the closest dataset records to a site, dataset or long/lat pair in Neotoma
#'
#' Passing in a download object the function outputs a Bacon or Clam formatted file to a
#' user defined destination for age modelling with existing age-depth modeling software.
#'
#' @importFrom utils write.csv write.table
#' @importFrom assertthat assert_that
#' @import sf
#' @param x A vector long/lat pair, or a dataset, site or download.
#' @param n The maximum number of records to return (in the case of ties the return may be larger)
#' @param buffer The size of the buffer for dataset search (in kilometers)
#' @param ... optional arguments to pass into \code{get_dataset}.
#' 
#' @details The function uses the \code{sf} package to generate a circular buffer around a point of interest.  
#' From there a square bounding box is sent to Neotoma using the \code{get_dataset()} function.  To use the buffering 
#' function we must convert from long/lat to UTM coordinates, which we do by guessing the UTM zone of the point of interest.
#' Details can be found in the function's R code hosted on GitHub: \url{https://github.com/ropensci/neotoma/blob/master/R/get_closest.R}
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}, Andria Dawson \email{andria.dawson@gmail.com}
#' 
#' @return This command returns a \code{dataset} or \code{dataset_list}, or NULL if no records exist within the bounding box.
#' 
#' @examples \dontrun{
#' #  The point of pulling chronology tables is to re-build or examine the chronological 
#' #  information that was used to build the age-depth model for the core.
#' # Find the closest records to Madison, WI:
#' get_closest(x = c(-89.4012, 43.0731), n = 10, buffer = 5000, datasettype = "pollen")
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

get_closest <- function(x, n, buffer, ...) {
  UseMethod('get_closest')
}

#' @export
get_closest.default <- function(x, n, buffer, ...) {
  
  assertthat::assert_that(length(x) == 2, 
                          msg = "You must pass an x, y object (e.g., c(10, -20)).")
  assertthat::assert_that(x[1] >= -180 & x[1] <= 180, 
                          msg = "You must pass an array of long/lat with long values from -180 - 180.")
  assertthat::assert_that(x[2] >= -90 & x[2] <= 90, 
                          msg = "You must pass an array of long/lat with lat values from -90 - 90.")

  utm_w <- seq(-180, 180, by = 6)
  
  proj <- paste0("+proj=utm +zone=",findInterval(x[1], utm_w))
  
  if (x[2] < 0) proj <- paste0(proj, " +south")
  
  bbox <- sf::st_point(x) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(4326) %>% 
    sf::st_transform(crs = proj) %>%
    sf::st_buffer(buffer * 1000) %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    as.numeric()
    
  buff_sets <- suppressMessages(get_dataset(loc = bbox, ...))
  
  if (is.null(buff_sets)) {
    message(paste0("There are no sites within ", buffer, "km of coordinates [", x[1], ", ", x[2], "]"))
    return(NULL)
  } else {
    sites <- get_site(buff_sets)
  }
  
  # From: http://www.r-bloggers.com/r-subsetting-a-list-based-on-a-condition/
  deg2rad <- function(deg) return(deg*pi/180)
  gcd.slc <- function(long1, lat1, long2, lat2) {
    R <- 6371000 # Earth mean radius [km]
    d <- acos(sin(deg2rad(lat1))*sin(deg2rad(lat2)) + cos(deg2rad(lat1)) * 
                cos(deg2rad(lat2)) * cos(deg2rad(long2) - deg2rad(long1))) * R
    return(d) # Distance in km
  }
  
  dists <- sapply(1:nrow(sites), 
                  function(i) gcd.slc(x[1], x[2], sites$long[i], sites$lat[i]))
  
  if(all(dists > buffer)) {
    message(paste0("There are no sites within ", buffer, "m of coordinates [", x[1], ", ", x[2], "]"))
    return(NULL)
  }
  
  message("The API call was successful, you have returned ", sum(dists < buffer, na.rm = TRUE),
          " records.")
  
  if (length(which(dists < buffer)) <= n) {
    data_out <- buff_sets[which(dists < buffer)]
  } else {
    set <- which((rank(dists, ties.method = 'min') <= n) & (dists < buffer))
    data_out <- buff_sets[set]
  }
  
  if (length(data_out) == 1) {
    class(data_out) <- c("dataset", "list")
  } else {
    class(data_out) <- c("dataset_list", "list")
  }
  
  return(data_out)

}

#' @export
get_closest.site <- function(x, n, buffer, ...) {
  coords <- c(x$long, x$lat)
  get_closest(coords, n, buffer, ...)
}

#' @export
get_closest.dataset <- function(x, n, buffer, ...) {
  coords <- c(get_site(x)$long, get_site(x)$lat)
  get_closest(coords, n, buffer, ...)
}

#' @export
get_closest.download <- function(x, n, buffer, ...) {
  coords <- c(get_site(x)$long, get_site(x)$lat)
  get_closest(coords, n, buffer, ...)
}