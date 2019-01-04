#' @title Extracts the depth values from a `download` object
#' @description Using a \code{download} object, return the sample depths (if available).
#'
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param obj A \code{download} object.
#' @param ... arguments passed to other methods.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a vector of depths.
#' 
#' @examples \dontrun{
#' # Provide a vector of depths to generate a new age model:
#' # The dataset id 684 is for Devils Lake, a record published by Louis Maher Jr.
#'
#' pollen.data <- get_download(684)
#' pollen.chron <- get_chroncontrol(pollen.data)[[1]]
#'
#' age_sds <- pollen.chron$chron.control$age - focal$chron.control$age.young,
#' get_curves <- ifelse(regexpr("Radiocarbon",
#'                              pollen.chron$chron.control$control.type) > -1, 
#'                      'intcal13', 'normal')
#'
#' new_chron <- Bchron::Bchronology(ages   = pollen.chron$chron.control$age,
#'                                  ageSds = age_sds
#'                                  positions = pollen.chron$chron.control$depth,
#'                                  calCurves = , 
#'                                  predictPositions = depths(pollen.data))
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/sites
#' @keywords IO connection
#' @export
`depths` <- function(obj, ...){
  UseMethod('depths', object = obj)
}

#' @export
##' @rdname depths
depths.default <- function(obj, ...){
  stop("Input data must be a `download` or a `download_list")
}

##' @export
##' @rdname depths
`depths.download` <- function(obj, ...) {
  ret <- obj[[1]]$sample.meta$depth
  ret
}

##' @export
##' @rdname depths
`depths.download_list` <- function(obj, ...) {
  ret <- lapply(obj, function(x) x$sample.meta$depth)
  
  if (length(ret) == 1) { ret <- ret[[1]] }
  
  ret
}
