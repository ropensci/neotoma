#' @title Return Site Information.
#' @description Return site information from the Neotoma Paleoecological Database.
#'
#' \code{get_site} returns site information from the Neotoma Paleoecological Database
#'    based on parameters defined by the user.
#'
#' @import RJSONIO RCurl
#' @param sitename A character string representing the full or partial site name.
#' @param altmin Minimum site altitude  (in m).
#' @param altmax Maximum site altitude (in m).
#' @param loc A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator.
#' @param gpid A character string or numeric value, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: http://api.neotomadb.org/apdx/geopol.htm
#' @param dataset An optional list object returned by \code{get_dataset}.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A data frame:
#'
#'  \item{\code{siteid}}{Unique database record identifier for the site.}
#'  \item{\code{sitename}}{Name of the site.}
#'  \item{\code{long}}{Mean longitude, in decimal degrees, for a site (-180 to 180).}
#'  \item{\code{lat}}{Mean latitude, in decimal degrees, for a site (-90 to 90).}
#'  \item{\code{elev}}{Elevation in meters.}
#'  \item{\code{description}}{Free form description of a site, including such information as physiography and vegetation around the site.}
#'  \item{\code{long_acc}}{If the site is described by a bounding box this is the box width.}
#'  \item{\code{lat_acc}}{If the site is described by a bounding box this is the box height.}
#'
#' @examples \dontrun{
#' #  What is the distribution of site elevations in Neotoma?
#' all.sites <- get_site()  #takes a bit of time.
#'
#' plot(density(all.sites$elev, from = 0, na.rm=TRUE),
#' main = 'Altitudinal Distribution of Neotoma Sites', xlab = 'Altitude (m)', log='x')
#'
#' #  Get site information from a dataset:
#' nw.datasets <- get_dataset(loc = c(-140, 50, -110, 65), datasettype='pollen',taxonname='Pinus*')
#' nw.sites <- get_site(nw.datasets)
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/sites
#' @keywords Neotoma Palaeoecology API
#' @export
get_site <- function(x, ...){
  UseMethod('get_site')
}

#' @export
get_site.default <- function(sitename, altmin, altmax, loc, gpid, download = NULL){

  base.uri <- 'http://api.neotomadb.org/v1/data/sites'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())

  #  Pass the parameters to param_check to make sure everything is kosher.
  error_test <- param_check(cl)
  if(error_test$flag == 1){
    stop(paste0(unlist(error_test$message), collapse='\n  '))
  }

  if ('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)
    if (all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
          all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
      cl$loc <- paste(cl$loc, collapse = ',')
    }
  }

  neotoma.form <- getForm(base.uri, .params = cl)
  aa <- try(fromJSON(neotoma.form, nullValue = NA))

  if (aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call. = FALSE)
  }
  if (aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ',
        length(aa), 'records.\n')
  }

  if (class(aa) == 'try-error'){
     output <- neotoma.form
  } else {
    names(aa) <- sapply(aa, `[[`, "SiteName")
    # This is much faster by direct calling of the data frame method
    # of rbind
    output <- do.call(rbind.data.frame, aa)
    # but we need to fix-up some characters that R changed to factors
    output$SiteName <- as.character(output$SiteName)
    output$SiteDescription <- as.character(output$SiteDescription)

    output <- data.frame(siteid = output$SiteID,
                sitename = output$SiteName,
                long = rowMeans(output[, c('LongitudeWest', 'LongitudeEast')],
                                na.rm = TRUE),
                lat = rowMeans(output[,c('LatitudeNorth', 'LatitudeSouth')],
                               na.rm = TRUE),
                elev = output$Altitude,
                description = output$SiteDescription,
                long_acc = abs(output$LongitudeWest - output$LongitudeEast),
                lat_acc = abs(output$LatitudeNorth - output$LatitudeSouth))
  }

  class(output) <- c('site', 'data.frame')
  output

}

#' @export
get_site.download <- function(download){

  site <- ldply(download, .fun=function(x)x$metadata$site.data)
  class(site) <- c('site', 'data.frame')
  site
}

#' @export
get_site.dataset <- function(dataset){
  site <- ldply(dataset, .fun='[[', 'site.data')
  class(site) <- c('site', 'data.frame')
  site
}
