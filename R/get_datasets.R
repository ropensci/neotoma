#' @title Obtain dataset information from the Neotoma Paleoecological Database.
#' @description Deprecated - see \code{get_dataset}.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getForm
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

#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords IO connection
#' @export
#'
get_datasets <- function(siteid, datasettype, piid, altmin, altmax, loc, gpid, taxonids, taxonname, ageold, ageyoung, ageof, subdate){
  #  The issue here is that these objects have multiple tables of multiple lengths.

  .Deprecated('get_dataset', package='neotoma')
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
    if(!any(c('ageyoung','ageold') %in% names(cl))){
      stop('When ageof in invoked you also need to provide an age range using ageyounger or ageolder.')
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

  if('taxonname' %in% names(cl)){
    if(!class(cl$taxonname) == 'character'){
      stop('The taxonname must be a character.')
    }
  }
  
  neotoma.form <- getForm(base.uri, .params = cl, binary=FALSE,
                          .encoding='utf-8', )

  aa <- try(fromJSON(neotoma.form, nullValue = NA))

  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    output <- aa[[2]]
    if(length(aa[[2]]) > 1){
      message(paste('The API call was successful, you have returned ', length(output), ' records.\n', sep=''))
    }
    else{
      message(paste('The API call was successful, you have returned ', length(output), ' record.\n', sep=''))
    }
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
