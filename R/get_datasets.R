
#' Obtain full datasets from the Neotoma Paleoecological Database.
#'
#' A function to access the Neotoma API and return datasets corresponding to the
#'    parameters defined by the user.
#'
#' @import RJSONIO RCurl
#' @param siteid A numeric value corresponding to the site ID.
#' @param datasettype A character string corresponding to one of the allowed dataset types in the Neotoma Database.  Allowed types include: 'geochronologic', 'loss-on-ignition', 'pollen', 'plant macrofossils', 'vertebrate fauna', 'mollusks', and 'pollen surface sample'.
#' @param piid Numeric value for the Principle Investigator's ID number.
#' @param altmin Numeric value indicating the minimum altitude for the site (can be used alone or with altmax).
#' @param altmax Numeric value indicating the maximum altitude for the site (can be used alone or with altmin).
#' @param loc A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator
#' @param gpid A character string or numeric value, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: http://api.neotomadb.org/apdx/geopol.htm
#' @param taxonids A numeric identifier for the taxon.  Use \code{get.tables('Taxa')} for a list of acceptable values.
#' @param taxonname A character string corresponding to a valid taxon identity in the Neotoma Database.  Use \code{get.tables('Taxa')} for a list of acceptable values.
#' @param ageold The oldest date acceptable for the search (in years before present).
#' @param ageyoung The youngest date acceptable for the search.
#' @param ageof If a taxon ID or taxon name is defined this parameter must be set to "taxon", otherwise it may refer to "sample", in which case the age bounds are for any samples within datasets or "dataset" if you want only datasets that are within the bounds of ageold and ageyoung.
#' @param subdate Date of dataset submission, either YYYY-MM-DD or MM-DD-YYYY.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return More details on the use of these parameters can be obtained from
#'    \url{http://api.neotomadb.org/doc/resources/datasets.}
#'
#'    A list, with each item corresponding to an individual record.  Each list item
#'    (each dataset record) includes the following components:
#'
#' \itemize{
#'  \item{DatasetID}{Unique database record identifier for the dataset.}
#'  \item{DatasetName}{Name of the dataset; not commonly used.}
#'  \item{CollUnitHandle}{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
#'  \item{CollUnitID}{Unique database record identifier for the collection unit.}
#'  \item{CollType}{The collection type. Types include cores, sections, excavations, and animal middens.}
#'  \item{DatasetType}{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
#'  \item{AgeOldest}{The oldest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{AgeYoungest}{The youngest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{SubDates}{An array of objects that describe dataset submission events.  If multiple submissions occured then this is a table.}
#'  \item{DatasetPIs}{An array of objects that describe Principal Investigators associated with a dataset.}
#'  \item{Site}{An object describing the site where the dataset samples were taken.}
#' }
#' @examples \dontrun{
#' # Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' # that are on the west coast of North America:
#' t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' # Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.
#' gpids <- get_table(table.name='GeoPoliticalUnits')
#' canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]
#'
#' v2kyr.datasets <- get_datasets(datasettype='vertebrate fauna', gpid=canID, ageold = 2000)
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
#'
get_datasets <- function(siteid, datasettype, piid, altmin, altmax, loc, gpid, taxonids, taxonname, ageold, ageyoung, ageof, subdate){
  #  The issue here is that these objects have multiple tables of multiple lengths.

  base.uri <- 'http://api.neotomadb.org/v1/data/datasets'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())

  if('piid' %in% names(cl)){
    # piid must be the numeric PI id number in the Neotoma database.
    if(!is.numeric(cl$piid)) stop('piid must be a numeric value.')
  }

  #  Parameter check for the datasettype, make sure it's one of the
  #  accepted types:
  if('datasettype' %in% names(cl)){
    settypes <- c('geochronologic', 'loss-on-ignition', 'pollen',
                  'plant macrofossils', 'vertebrate fauna', 'mollusks',
                  'pollen surface sample')

    set <- pmatch(cl$datasettype, settypes, nomatch=NA)
    if(is.na(set)) stop('datasettype must be one of: geochronologic, loss-on-ignition, pollen,\nplant macrofossils, vertebrate fauna, mollusks, pollen surface sample')
  }

  if('ageof' %in% names(cl)){
    if(!cl$ageof %in% c('sample', 'taxon', 'dataset')){
      stop('ageof parameter must be one of: sample, taxon or dataset')
    }
    else{
      if(any(c('taxonid', 'taxonname') %in% names(cl)) & !cl$ageof == 'taxon'){
        stop('When taxonid or taxonname is invoked, ageof must be taxon')
      }
    }
  }
  
  if('gpid' %in% names(cl)){
    if(is.character(gpid)){
      data(gp.table)
      gprow <- match(x=gpid, table=gp.table$GeoPoliticalName)
      if(is.na(gprow)){
        stop('Cannot find a match for the gpid provided.')
      }
      gpid <- gp.table$GeoPoliticalID[gprow]
    }
    else{
      if(!is.numeric(gpid)){
        stop('The gpid must be either a character string or an integer.')
      }
    }
  }

  #  Parameter check on altitudes:
  if(all(c('altmin', 'altmax') %in% names(cl))){
    #  If the user defines a minimum and maximum altitude, make sure that the
    #  minimum is smaller than the max.
    if(cl$altmin > cl$altmax){
      altmin <- min(c(cl$altmin, cl$altmax))
      altmax <- max(c(cl$altmin, cl$altmax))
      warning('altmin must be smaller than altmax, values were switched in call.')
    }
  }

  #  Parameter check on ages:
  if(all(c('ageold', 'ageyoung') %in% names(cl))){
    #  If the user defines a minimum and maximum age, make sure that the
    #  minimum is smaller than the max.
    if(cl$ageyoung > cl$ageold){
      cl$ageyoung <- min(c(eval(cl$ageold), eval(cl$ageyoung)))
      cl$ageold <- max(c(eval(cl$ageold), eval(cl$ageyoung)))
      cat(cl)
      warning('ageyoung must be smaller than ageold, values were switched in call.')
    }
    else{
      cl$ageold <- eval(cl$ageold)
      cl$ageyoung <- eval(cl$ageyoung)
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
  #if('gpid' %in% names(cl)){
  #  if(!cl$gpid %in% geopol[,5]){
  #    stop('Unrecognized geopolitical entity.  Check for acceptible names in data(geopol).')
  #  }
  #}

  neotoma.form <- getForm(base.uri, .params = cl, binary=FALSE,
                          .encoding='utf-8', )

  aa <- try(fromJSON(neotoma.form, nullValue = NA))

  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    output <- aa[[2]]
    cat('The API call was successful, you have returned ', length(output), 'records.\n')
  }


  if(class(output) == 'try-error') output <- neotoma.form
  else{
    #  This is a bit frustrating, I can't quite figure it out.
    # The things that are multiple lengths are:
    # Dataset PIs &
    # SubDates
    #  I'd like to put this out in a nice table format

    output <- lapply(output, function(x) {x$Site <- data.frame(x$Site); x})

    output <- lapply(output,
                     function(x) {
                       sub.test <- try(do.call(rbind.data.frame, x$SubDates))  
                       x$SubDates <- sub.test
                       if(length(x$SubDates) > 0){
                         names(x$SubDates) <- c("SubmissionDate","SubmissionType")
                       }
                       x})

    output <- lapply(output,
                      function(x) {
                          x$DatasetPIs <- do.call(rbind.data.frame, x$DatasetPIs)
                          rownames(x$DatasetPIs) <- NULL
                          x
                      })

  }

  output

}
