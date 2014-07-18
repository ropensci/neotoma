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
#' @param dataset A list object returned by \code{get_dataset}.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A table:
#'
#' \describe{
#'  \item{siteid}{Unique database record identifier for the site.}
#'  \item{sitename}{Name of the site.}
#'  \item{long}{Mean longitude, in decimal degrees, for a site (-180 to 180).}
#'  \item{lat}{Mean latitude, in decimal degrees, for a site (-90 to 90).}
#'  \item{elev}{Elevation in meters.}
#'  \item{description}{Free form description of a site, including such information as physiography and vegetation around the site.}
#'  \item{long_acc}{If the site is described by a bounding box this is the box width.}
#'  \item{lat_acc}{If the site is described by a bounding box this is the box height.}
#' }
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
get_site <- function(sitename, altmin, altmax, loc, gpid, dataset = NULL){

  base.uri <- 'http://api.neotomadb.org/v1/data/sites'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())

  # Parameter check on siteid:
  if ('siteid' %in% names(cl)){
    if (!is.numeric(siteid)) stop('siteid must be numeric.')
  }

  # Parameter check on altitudes.  This gets reused, we could turn it into a
  # higher level function to save reading lame code:
  if (all(c('altmin', 'altmax') %in% names(cl))){
    # If the user defines a minimum and maximum altitude, make sure that the
    # minimum is smaller than the max.
    if (cl$altmin > cl$altmax){
      altmin <- min(c(cl$altmin, cl$altmax))
      altmax <- max(c(cl$altmin, cl$altmax))
      warning('altmin must be smaller than atmax, values were switched in call.')
    }
  }

  # Parameter check on 'loc', ought to be a comma separated list of
  # lonW, latS, lonE, latN when it is passed out, but it's probably
  # better to pass in a vector.  Although it might be better to associate
  # it with a spatial object existing in R like an extent or bbox.
  if ('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)

    if (class(cl$loc) == 'numeric' & length(cl$loc == 4)){

      # The latitudes must be from -90 to 90
      # The longitudes must be from -180 to 180
      if (all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
           all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
        cl$loc <- paste(cl$loc, collapse = ',')
      } else {
        stop(paste0('loc must be in the form c(lonW, latS, lonE, latN).',
                    '\nLongitudes from -180 to 180, latitudes from -90 to 90.'))
      }
    } else {
      stop('The loc must be a numeric vector: lonW, latS, lonE, latN.\n')
    }
  }

  if ('gpid' %in% names(cl)){
    if (is.character(gpid)){
      data(gp.table)
      gprow <- match(x=gpid, table=gp.table$GeoPoliticalName)
      if (is.na(gprow)){
        stop('Cannot find a match for the gpid provided.')
      }
      gpid <- gp.table$GeoPoliticalID[gprow]
    } else {
      if (!is.numeric(gpid)){
        stop('The gpid must be either a character string or an integer.')
      }
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
                long = rowMeans(output[, c('LongitudeWest', 'LongitudeEast')], na.rm = TRUE),
                lat = rowMeans(output[, c('LatitudeNorth', 'LatitudeSouth')], na.rm = TRUE),
                elev = output$Altitude,
                description = output$SiteDescription,
                long_acc = abs(output$LongitudeWest - output$LongitudeEast),
                lat_acc = abs(output$LatitudeNorth - output$LatitudeSouth))
  }
  output

}