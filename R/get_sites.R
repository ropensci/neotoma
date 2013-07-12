#' Return Site Information.
#' 
#' \code{get_sites} returns site information from the Neotoma Paleoecological Database 
#'    based on parameters defined by the user.
#' 
#' @import RJSONIO RCurl plyr
#' @param siteid The numerical site ID.
#' @param sitename A character string representing the full or partial site name.
#' @param altmin Minimum site altitude  (in m).
#' @param altmax Maximum site altitude (in m).
#' @param loc A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator.
#' @param gpid A character string, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: http://api.neotomadb.org/apdx/geopol.htm
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A table:
#'    
#' \itemize{
#'  \item{SiteID}{Unique database record identifier for the site.}
#'  \item{SiteName}{Name of the site.}
#'  \item{Altitude}{Altitude in meters.}
#'  \item{LatitudeNorth}{North bounding latitude, in decimal degrees, for a site.}
#'  \item{LatitudeSouth}{South bounding latitude, in decimal degrees, for a site.}
#'  \item{LongitudeEast}{East bounding longitude, in decimal degrees, for a site.}
#'  \item{LongitudeWest}{West bounding longitude, in decimal degrees, for a site.}
#'  \item{SiteDescription}{Free form description of a site, including such information as physiography and vegetation around the site.}
#' }
#'    
#' Extended response variables when only a single site is returned:
#' \itemize{
#'  \item{CollectionUnitID}{Unique database record identifier for the collection unit.}
#'  \item{Handle}{Code name for the collection unit. This code may be up to 10 characters, but an effort is made to keep these to 8 characters or less. Data are frequently distributed by collection unit, and the handle is used for file names.}
#'  \item{CollType}{The collection type. Types include cores, sections, excavations, and animal middens. Collection Units may be modern collections, surface float, or isolated specimens. Composite Collections Units include different kinds of Analysis Units, for example a modern surface sample for ostracodes and an associated water sample.}
#'  \item{Datasets}{An array of objects that describe datasets associated with a site.}
#' }
#' @examples \dontrun{
#' #  What is the distribution of site elevations in Neotoma?
#' all.sites <- get_sites()  #takes a bit of time.
#' 
#' plot(density(all.sites$Altitude, from = 0, na.rm=TRUE),
#' main = 'Altitudinal Distribution of Neotoma Sites', xlab = 'Altitude (m)', log='x')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export 
get_sites <- function(siteid, sitename, altmin, altmax, loc, gpid){
  
  base.uri <- 'http://api.neotomadb.org/v1/data/sites'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on siteid:
  if('siteid' %in% names(cl)){
    if(!is.numeric(siteid)) stop('siteid must be numeric.')
  }
  
  #  Parameter check on altitudes.  This gets reused, we could turn it into a
  #  higher level function to save reading lame code:
  if(all(c('altmin', 'altmax') %in% names(cl))){
    #  If the user defines a minimum and maximum altitude, make sure that the
    #  minimum is smaller than the max.
    if(cl$altmin > cl$altmax){
      altmin <- min(c(cl$altmin, cl$altmax))
      altmax <- max(c(cl$altmin, cl$altmax))
      warning('altmin must be smaller than atmax, values were switched in call.')
    }
  }
  
  # Parameter check on 'loc', ought to be a comma separated list of 
  # lonW, latS, lonE, latN when it is passed out, but it's probably
  # better to pass in a vector.  Although it might be better to associate
  # it with a spatial object existing in R like an extent or bbox.
  if('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)
    
    if(class(cl$loc) == 'numeric' & length(cl$loc == 4)){
      
      #  The latitudes must be from -90 to 90
      #  The longitudes must be from -180 to 180
      if(all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
           all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
        cl$loc <- paste(cl$loc, collapse = ',')
      }
      else{
        stop('loc must be in the form c(lonW, latS, lonE, latN).\nLongitudes from -180 to 180, latitudes from -90 to 90.')
      }
    }
    else{
      stop('The loc must be a numeric vector: lonW, latS, lonE, latN.\n')
    }
  }
  
  #  Parameter check on 'gpid', the name needs to be in the big table in
  #  data object geopol:
  if('gpid' %in% names(cl)){
    #if(!cl$gpid %in% geopol[,5]){
    #  stop('Unrecognized geopolitical entity.  Check for acceptible names in data(geopol).')
    #}
  }
  
  neotoma.form <- getForm(base.uri, .params = cl)
  aa <- try(fromJSON(neotoma.form, nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    if('siteid' %in% names(cl) & length(names(cl)) == 1){
      
    }
    else{
      names(aa) <- sapply(aa, function(x)x$SiteName)
      output <- suppressMessages( do.call('rbind', lapply(aa, data.frame, stringsAsFactors = FALSE)))
    }
  }
  output
}
