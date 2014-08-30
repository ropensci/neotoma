#' @title Obtain dataset information from the Neotoma Paleoecological Database.
#' @description A function to access the Neotoma API and return datasets corresponding to the parameters defined by the user.
#'
#' @import RJSONIO RCurl
#' @param siteid A numeric value corresponding to the site ID.
#' @param datasettype A character string corresponding to one of the allowed dataset types in the Neotoma Database.  Allowed types include: \code{"geochronologic"}, \code{"loss-on-ignition"}, \code{"pollen"}, \code{"plant macrofossils"}, \code{"vertebrate fauna"}, \code{"mollusks"}, and \code{"pollen surface sample"}.
#' @param piid Numeric value for the Principle Investigator's ID number.
#' @param altmin Numeric value indicating the minimum altitude for the site (can be used alone or with \code{altmax}).
#' @param altmax Numeric value indicating the maximum altitude for the site (can be used alone or with \code{altmin}).
#' @param loc A numeric vector \code{c(lonW, latS, lonE, latN)} representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator
#' @param gpid A character string or numeric value, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: \url{http://api.neotomadb.org/apdx/geopol.htm}
#' @param taxonids A numeric identifier for the taxon.  See \code{\link{get_table}} and use \code{get_tables('Taxa')} for a list of acceptable values.
#' @param taxonname A character string corresponding to a valid taxon identity in the Neotoma Database.  See \code{\link{get_table}} and use \code{get_table('Taxa')} for a list of acceptable values.
#' @param ageold The oldest date acceptable for the search (in years before present).
#' @param ageyoung The youngest date acceptable for the search.
#' @param ageof If a taxon ID or taxon name is defined this parameter must be set to \code{"taxon"}, otherwise it may refer to \code{"sample"}, in which case the age bounds are for any samples within datasets or \code{"dataset"} if you want only datasets that are within the bounds of ageold and ageyoung.
#' @param subdate Date of dataset submission, either YYYY-MM-DD or MM-DD-YYYY.
#' @param download An object of class \code{download} obtained using the command \code{\link{get_download}}.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return More details on the use of these parameters can be obtained from
#'    \url{http://api.neotomadb.org/doc/resources/datasets.}
#'
#'    A list, with each item corresponding to an individual record.  Each list item
#'    (each dataset record) includes the following components:
#'
#'  \item{ \code{DatasetID} }{Unique database record identifier for the dataset.}
#'  \item{ \code{DatasetName}  }{Name of the dataset; not commonly used.}
#'  \item{ \code{CollUnitHandle}  }{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
#'  \item{ \code{CollUnitID}  }{Unique database record identifier for the collection unit.}
#'  \item{ \code{CollType}  }{The collection type. Types include cores, sections, excavations, and animal middens.}
#'  \item{ \code{DatasetType}  }{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
#'  \item{ \code{AgeOldest}  }{The oldest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{AgeYoungest}  }{The youngest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{ \code{SubDates}  }{An array of objects that describe dataset submission events.  If multiple submissions occured then this is a table.}
#'  \item{ \code{DatasetPIs}  }{An array of objects that describe Principal Investigators associated with a dataset.}
#'  \item{ \code{Site}  }{An object describing the site where the dataset samples were taken.}
#'
#' @examples \dontrun{
#' # Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' # that are on the west coast of North America:
#' t8kyr.datasets <- get_dataset(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' # Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.
#' gpids <- get_table(table.name='GeoPoliticalUnits')
#' canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]
#'
#' v2kyr.datasets <- get_dataset(datasettype='vertebrate fauna', gpid=canID, ageold = 2000)
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
#'

get_dataset <- function(x, ...){
  UseMethod('get_dataset')
}

#' @export
get_dataset.default <- function(siteid, datasettype, piid, altmin, altmax, loc, gpid,
                        taxonids, taxonname, ageold, ageyoung, ageof, subdate){
  # The issue here is that these objects
  # have multiple tables of multiple lengths.

  base.uri <- 'http://api.neotomadb.org/v1/data/datasets'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())

  #  Pass the parameters to param_check to make sure everything is kosher.
  error_test <- param_check(cl)
  if(error_test$flag == 1){
    stop(paste0(unlist(error_test$message), collapse='\n  '))
  }

  # Parameter check for the datasettype, make sure
  # it's one of the accepted types:

  if ('datasettype' %in% names(cl)){
    settypes <- c('geochronologic', 'loss-on-ignition', 'pollen',
                  'plant macrofossils', 'vertebrate fauna', 'mollusks',
                  'pollen surface sample')

    set <- pmatch(cl$datasettype, settypes, nomatch = NA)
    if (is.na(set)) {
      stop(paste0('datasettype must be one of: geochronologic, ',
                                'loss-on-ignition, pollen, \n',
                                'plant macrofossils, vertebrate fauna, ',
                                'mollusks, pollen surface sample'))
    }
  }


  neotoma.form <- getForm(base.uri, .params = cl, binary = FALSE,
                          .encoding = 'utf-16')

  aa <- try(fromJSON(neotoma.form, nullValue = NA))

  if (aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call. = FALSE)
  }
  if (aa[[1]] == 1){
    output <- aa[[2]]
    if (length(aa[[2]]) > 1){
      message(paste('The API call was successful, you have returned ',
                    length(output), ' records.\n', sep = ''))
    } else {
      message(paste('The API call was successful, you have returned ',
                    length(output), ' record.\n', sep = ''))
    }
  }


  if (class(output) == 'try-error') {
    new.output <- neotoma.form
  } else {
    new.output <- lapply(output, function(x) {
      new.output <- list()
      new.output$site.data <- data.frame(siteid = x$Site$SiteID,
                                         sitename = x$Site$SiteName,
                                         long = mean(unlist(x$Site[c('LongitudeWest', 'LongitudeEast')]),
                                                         na.rm = TRUE),
                                         lat = mean(unlist(x$Site[c('LatitudeNorth', 'LatitudeSouth')]),
                                                        na.rm = TRUE),
                                         elev = x$Site$Altitude,
                                         description = x$Site$SiteDescription,
                                         long_acc = abs(x$Site$LongitudeWest - x$Site$LongitudeEast),
                                         lat_acc = abs(x$Site$LatitudeNorth - x$Site$LatitudeSouth),
                                         row.names = x$Site$SiteName,
                                         stringsAsFactors = FALSE)
      new.output$dataset <- data.frame(dataset.id = x$DatasetID,
                                       dataset.name = x$DatasetName,
                                       collection.type = x$CollUnitType,
                                       collection.handle = x$CollUnitHandle,
                                       dataset.type = x$DatasetType,
                                       stringsAsFactors = FALSE)
      new.output$pi.data <- do.call(rbind.data.frame, x$DatasetPIs)
      rownames(new.output$pi.data) <- NULL

      sub.test <- try(do.call(rbind.data.frame, x$SubDates))

      if(length(sub.test) > 0){
        colnames(sub.test) <- c("SubmissionDate",  "SubmissionType")
      }

      new.output$submission <- sub.test


      new.output$access.date = Sys.time()

      new.output})

  }

  class(new.output) <- c('dataset', 'list')

  new.output

}

#' @export
get_dataset.site <- function(site){
  # The issue here is that these objects
  # have multiple tables of multiple lengths.

  siteIDs <- site$siteid

  pull_site <- function(siteid){

    base.uri <- 'http://api.neotomadb.org/v1/data/datasets/?siteid='
    aa <- try(fromJSON(paste0(base.uri, siteid), nullValue = NA))

    if (aa[[1]] == 0){
      stop(paste('Server returned an error message:\n', aa[[2]]), call. = FALSE)
    }
    if (aa[[1]] == 1){
      output <- aa[[2]]
    }

    if (class(output) == 'try-error') {
      new.output <- neotoma.form
    } else {
      new.output <- lapply(output, function(x) {
        new.output <- list()
        new.output$site.data <- data.frame(siteid = x$Site$SiteID,
                                           sitename = x$Site$SiteName,
                                           long = mean(unlist(x$Site[c('LongitudeWest', 'LongitudeEast')]),
                                                       na.rm = TRUE),
                                           lat = mean(unlist(x$Site[c('LatitudeNorth', 'LatitudeSouth')]),
                                                      na.rm = TRUE),
                                           elev = x$Site$Altitude,
                                           description = x$Site$SiteDescription,
                                           long_acc = abs(x$Site$LongitudeWest - x$Site$LongitudeEast),
                                           lat_acc = abs(x$Site$LatitudeNorth - x$Site$LatitudeSouth),
                                           row.names = x$Site$SiteName,
                                           stringsAsFactors = FALSE)
        new.output$dataset <- data.frame(dataset.id = x$DatasetID,
                                         dataset.name = x$DatasetName,
                                         collection.type = x$CollUnitType,
                                         collection.handle = x$CollUnitHandle,
                                         dataset.type = x$DatasetType,
                                         stringsAsFactors = FALSE)
        new.output$pi.data <- do.call(rbind.data.frame, x$DatasetPIs)
        rownames(new.output$pi.data) <- NULL

        sub.test <- try(do.call(rbind.data.frame, x$SubDates))

        if(length(sub.test) > 0){
          colnames(sub.test) <- c("SubmissionDate",  "SubmissionType")
        }

        new.output$submission <- sub.test


        new.output$access.date = Sys.time()

        new.output})

    }

    class(new.output) <- c('dataset', 'list')

    new.output[[1]]
  }

  new.output <- lapply(site$siteid, pull_site)

  class(new.output) <- c('dataset', 'list')

  new.output

}

#' @export
get_dataset.download <- function(download){
  # Just pull the dataset out of the download.
  output <- lapply(download, `[[`, "metadata")
  class(output) <- c('dataset', 'list')
  output
}
