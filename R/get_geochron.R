#' Function to return geochronological data from records.
#'
#' Using the dataset ID, return all geochronological data associated with the dataID.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.   This function will only download one dataset at a time.
#'
#' @importFrom RJSONIO fromJSON
#' @param datasetid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{get_dataset}.
#' @param verbose logical; should messages on API call be printed?
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either an object of class \code{"try-error"}' (see \code{\link{try}}) definined by the error returned
#'    from the Neotoma API call, or a geochronology table, a data frame with the following components:
#'
#'  \item{ \code{sample.id} }{A unique identifier for the geochronological unit.}
#'  \item{ \code{age.type.id} }{Numeric. One of five possible age types.}
#'  \item{ \code{age.type} }{String.  The age type, one of calendar years, radiocarbon years, etc.}
#'  \item{ \code{age} }{Dated age of the material.}
#'  \item{ \code{e.older} }{The older error limit of the age value.  Commonly 1 standard deviation.}
#'  \item{ \code{e.young} }{The younger error limit of the age value.}
#'  \item{ \code{delta13C} }{The measured or assumed delta13C value for radiocarbon dates, if provided.}
#'  \item{ \code{material.dated} }{A table describing the collection, including dataset information, PI data compatable with \code{\link{get_contact}} and site data compatable with \code{\link{get_site}}.}
#'  \item{ \code{geo.chron.type.id} }{Numeric identification for the type of geochronological analysis.}
#'  \item{ \code{geo.chron.type} }{Text string, type of geochronological analysis, i.e., Radiocarbon dating, luminesence.}
#'  \item{ \code{notes} }{Text string}
#'  \item{ \code{infinite} }{Boolean, does the dated material return an "infinte" date?}
#'
#'  A full data object containing all the relevant geochronological data available for a dataset.
#' @examples \dontrun{
#' #  Search for sites with "Pseudotsuga" pollen that are older than 8kyr BP and
#' #  find the relevant geochronological data associated with the samples.
#' #  Are some time periods better dated than others?
#' t8kyr.datasets <- get_dataset(taxonname='*Pseudotsuga*', loc=c(-150, 20, -100, 60),
#'                               ageyoung = 8000)
#'
#' #  Returns 74 records (as of 01/08/2014), get the dataset IDs for all records:
#' dataset.ids <- sapply(t8kyr.datasets, function(x) x$dataset.meta$dataset.id)
#' geochron.records <- get_geochron(dataset.ids)
#'
#' #  Standardize the taxonomies for the different records using the WS64 taxonomy.
#'
#' get_ages <- function(x){
#'   any.ages <- try(x$age[x$age.type.id == 4])
#'   if(class(any.ages) == 'try-error') output <- NA
#'   if(!class(any.ages) == 'try-error') output <- unlist(any.ages)
#'   output
#' }
#'
#' radio.chron <- unlist(sapply(geochron.records, get_ages))
#'
#' hist(radio.chron, breaks=seq(0, 40000, by = 500),
#'      main = 'Distribution of radiocarbon dates for Pseudotsuga records',
#'      xlab = 'Radiocarbon date (14C years before 1950)')
#' }
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords IO connection
#' @export
get_geochron <- function(x, ...){
  UseMethod('get_geochron')
}

#' @importFrom RJSONIO fromJSON
#' @export
get_geochron.default <- function(datasetid, verbose = TRUE){

    # Updated the processing here. There is no need to be fiddling with
    # call. Use missing() to check for presence of argument
    # and then process as per usual
    if (missing(datasetid)) {
        stop(paste(sQuote("datasetid"), "must be provided."))
    } else {
        if (!is.numeric(datasetid))
            stop('datasetid must be numeric.')
    }

    # Get sample is a function because we can now get
    # one or more geochronologies at a time.
    get_sample <- function(x){
      
      dataset <- get_dataset(datasetid = x)
      
      base.uri <- 'http://api.neotomadb.org/v1/apps/geochronologies/'
      
      # query Neotoma for data set
      aa <- try(fromJSON(paste0(base.uri, '?datasetid=', x), nullValue = NA))

      # Might as well check here for error and bail
      if (inherits(aa, "try-error"))
          return(aa)

      # if no error continue processing
      if (isTRUE(all.equal(aa[[1]], 0))) {
        # The API did not return a record, or returned an error.
          stop(paste('Server returned an error message:\n', aa[[2]]),
               call. = FALSE)
      }

      if (isTRUE(all.equal(aa[[1]], 1) & length(aa[[2]]) == 0)) {
        # The API returned a record, but the record did not
        # have associated geochronology information.
        stop('No geochronological record is associated with this sample',
             call. = FALSE)
      }

      if (isTRUE(all.equal(aa[[1]], 1) & length(aa[[2]]) > 0)) {
        # The API returned a record with geochron data.
        aa <- aa[[2]]

        if (verbose) {
            message(strwrap(paste0("API call was successful.")))
        }

        # If there are actual stratigraphic samples
        # with data in the dataset returned.

        pull.rec <- function(x){

          data.frame(sample.id = x$SampleID,
                     depth   = x$Depth,
                     thickness = x$Thickness,
                     age.type = x$AgeType,
                     age = x$Age,
                     e.older = x$ErrorOlder,
                     e.young = x$ErrorYounger,
                     delta13C = x$Delta13C,
                     lab.no = x$LabNumber,
                     material.dated = x$MaterialDated,
                     geo.chron.type = x$GeochronType,
                     notes = x$Notes,
                     infinite = x$Infinite,
                     stringsAsFactors = FALSE)
        }

        out <- list(x, do.call(rbind.data.frame, lapply(aa, pull.rec)))
        
        class(out) <- c('geochronologic', 'data.frame')
      }

      out
    }

    lapply(datasetid, function(x)try(get_sample(x)))

}

#' @export
get_geochron.dataset <- function(x, verbose = TRUE){
  
  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual
  
  datasetid <- x$dataset.meta$dataset.id
  
  if(!x$dataset.meta$dataset.type %in% 'geochronologic'){
    stop(paste0('The dataset ID ', dataset$dataset.meta$dataset.id,
                   ' is not associated with a geochronology object, not count data.'))
  } else {
    geochron <- get_geochron(datasetid)
    
  }
  
  geochron
  
}

#' @export
get_geochron.dataset_list <- function(x, verbose = TRUE){
  
  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual
  
  datasetid <- unlist(lapply(x, FUN=function(x)x$dataset$dataset.id))
  
  dataset.types <- unlist(lapply(x, FUN=function(x)x$dataset$dataset.type))
  
  if(any(!dataset.types%in%'geochronologic')){
    if(all(!dataset.types%in%'geochronologic')){
      stop('This set contains no geochronological datasets.  Use get_download instead.')
    } else {
      message('This dataset contains records that are not geochronological datasets.  Only geochronological datasets will be returned.')
      datasetid <- datasetid[dataset.types %in% 'geochronologic']
    }
  }
  
  aa <- get_geochron(datasetid)
  
  aa
}

#' @export
get_geochron.site <- function(x, verbose = TRUE){
  
  dataset <- get_dataset(x)
  
  datasetid <- unlist(lapply(x, FUN=function(x)x$dataset$dataset.id))
  
  dataset.types <- unlist(lapply(x, FUN=function(x)x$dataset$dataset.type))
  
  if(any(!dataset.types%in%'geochronologic')){
    if(all(!dataset.types%in%'geochronologic')){
      stop('This set contains no geochronological datasets.  Use get_download instead.')
    } else {
      message('This dataset contains records that are not geochronological datasets.  Only geochronological datasets will be returned.')
      datasetid <- datasetid[dataset.types %in% 'geochronologic']
    }
  }
  
  aa <- get_geochron(datasetid)
  
  aa

}

