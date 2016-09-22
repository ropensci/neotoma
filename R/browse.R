#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a \code{download} or \code{dataset} object, open up a browser window in the users default browser. Passing a \code{download_list} or \code{dataset_list} will open Neotoma Explorer with the first object and return a warning.
#'
#' @importFrom utils browseURL
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a NULL value, opens a browser.
#' 
#' @examples \dontrun{
#' # Where are the XRF data?
#'
#' xrf.data <- get_dataset(datasettype='X-ray fluorescence (XRF)')
#' browse(xrf.data)
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/sites
#' @keywords IO connection
#' @export
browse <- function(x){
  UseMethod('browse', object = x)
}

#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A numeric value with the dataset ID.
#' 
#' @export
browse.default <- function(x){
  if (length(x) > 1) {
    x <- paste0(x, collapse = ",")
    utils::browseURL(paste0('http://apps.neotomadb.org/Explorer/?datasetids=', x))
  } else {
    utils::browseURL(paste0('http://apps.neotomadb.org/Explorer/?datasetid=', x))
  }
  NULL
}


#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A \code{dataset} object.
#' 
#' @export
browse.dataset <- function(x){
  browse(x$dataset.meta$dataset.id)
  NULL
}

#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A \code{dataset_list} object.
#' 
#' @export
browse.dataset_list <- function(x){
  
  input <- sapply(x, function(y)y$dataset.meta$dataset.id)
  
  browse(input)
  NULL
}

#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A \code{download} object.
#' 
#' @export
browse.download <- function(x){

  browse(x$dataset$dataset.meta$dataset.id)
  NULL
}

#' @title Open a browser window to display a Neotoma dataset within the Neotoma Explorer
#' @description Using a numeric value, \code{download}, \code{download_list}, \code{dataset} or \code{dataset_list} object, open up a browser window in the users default browser. Very large objects
#'
#' @param x A \code{download_list} object.
#' 
#' @export
browse.download_list <- function(x){

  input <- sapply(x, function(y)y$dataset.meta$dataset.id)
  
  browse(input)
  NULL
}