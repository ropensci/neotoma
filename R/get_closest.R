#' Find the closest dataset records to a site, dataset or long/lat pair in Neotoma
#'
#' Passing in a download object the function outputs a Bacon or Clam formatted file to a
#' user defined destination for age modelling with existing age-depth modeling software.
#'
#' @importFrom utils write.csv write.table
#' @param x A vector long/lat pair, or a dataset, site or download.
#' @param n The number of records to return.
#' @param buffer The size of the buffer for dataset search (in kilometers)
#' @param ... optional arguments to pass into \code{get_dataset}.
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}, Andria Dawson \email{andria.dawson@gmail.com}
#' 
#' @return This command returns a \code{dataset} or \code{dataset_list}, or NULL if no records exist within the bounding box.
#' 
#' @examples \dontrun{
#' #  The point of pulling chronology tables is to re-build or examine the chronological 
#' #  information that was used to build the age-depth model for the core.
#' # Find the closest records to Madison, WI:
#' get_closest(x = c(-89.4012, 43.0731), n = 10, buffer = 5, datasettype = "pollen")
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
  
  # The bbox becomes the `loc` parameter for the search:
  bbox <- c(x - buffer, x + buffer)
  
  buff_sets <- get_dataset(loc = bbox, ...)
  
  if (is.null(buff_sets)) {
    return(NULL)
  } else {
    sites <- get_site(buff_sets)
  }
  
  # From: http://www.r-bloggers.com/r-subsetting-a-list-based-on-a-condition/
  deg2rad <- function(deg) return(deg*pi/180)
  gcd.slc <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    d <- acos(sin(deg2rad(lat1))*sin(deg2rad(lat2)) + cos(deg2rad(lat1)) * 
                cos(deg2rad(lat2)) * cos(deg2rad(long2) - deg2rad(long1))) * R
    return(d) # Distance in km
  }
  
  dists <- sapply(1:nrow(sites), function(i) gcd.slc(x[1], x[2], sites$long[i], sites$lat[i]))
  
  data_out <- buff_sets[dists < (sort(dists)[n + 1])]
  
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