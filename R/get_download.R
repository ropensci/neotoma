
#' Function to return full dataset records.
#'
#' Using the dataset ID, return all records associated with the data.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.   This function will only download one dataset at a time.
#'
#' @import RJSONIO RCurl plyr
#' @param datasetid A single numeric dataset ID, as returned by \code{get_datasets}.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either a 'try-error' definined by the error returned
#'    from the Neotoma API call, or a list comprising the following items:
#'
#' \itemize{
#'  \item{metadata}{A table describing the collection, including the dataset ID, dataset name, collection type, collection handle and dataset type.}
#'  \item{pi.data}{A list of PI data, compatable with \code{get_contacts}.}
#'  \item{site.data}{Site information, compatable with \code{get_sites}}
#'  \item{sample.meta}{Dataset information for the core, primarily the age-depth model and chronology.}
#'  \item{taxon.list}{The list of taxa contained within the dataset, unordered, including information that can be used in \code{get_taxa}}
#'  \item{counts}{The assemblage data for the dataset, arranged with each successive depth in rows and the taxa as columns.  All taxa are described in \code{taxon.list}, the chronology is in \code{sample.data}}
#' }
#'
#'    A full data object containing all the relevant assemblage information and metadata neccessary to understand a site.
#'    The data object is a list of lists and data.frames that describe an assemblage, the constituent taxa, the chronology, site and PIs who contributed the data.
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
#' GOLDKBG <- get_download(t8kyr.datasets[[1]]$DatasetID)
#'
#' taxa.no <- nrow(GOLDKBG$taxon.list)
#'
#' #  Of the 60 taxa in the record, plot the pollen curve for Abies over time:
#'
#' pollen <- GOLDKBG$taxon.list$VariableElement == 'pollen'
#' pol.curve <- data.frame(age = GOLDKBG$sample.meta$Age,
#'                         Abies = GOLDKBG$counts[,'Abies'] / rowSums(GOLDKBG$counts[,pollen]))
#' plot(Abies * 100 ~ age, data = pol.curve, type='b',
#'      ylab = '% Abies', xlab='Calibrated Years BP', pch=19)
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get_download <- function(datasetid){

  #This needs work.
  base.uri <- 'http://api.neotomadb.org/v1/data/downloads'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())

  #  Parameter check on siteid:
  if('datasetid' %in% names(cl)){
    if(!is.numeric(datasetid)) stop('datasetid must be numeric.')
  }

  aa <- try(fromJSON(paste(base.uri, '/', cl, sep=''), nullValue = NA))

  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')

    #  So here the goal is to reduce this list of lists to as simple a set of
    #  matrices as possible.
    if('Samples' %in% names(aa[[1]])){
      meta.data <- with(aa[[1]], data.frame(dataset.id = DatasetID,
                                            dataset.name = DatasetName,
                                            collection.type = CollUnitType,
                                            collection.handle = CollUnitHandle,
                                            dataset.type = DatasetType))
      site.data <- as.data.frame(aa[[1]]$Site)
      site.data <- site.data[,c('SiteID', 'SiteName', 'Altitude',
                                'LatitudeNorth', 'LongitudeWest',
                                'LatitudeSouth', 'LongitudeEast',
                                'SiteDescription', 'SiteNotes')]

      pi.data <- aa[[1]]$DatasetPIs

      #  This is the part I'm less sure about doing nicely:
      samples <- aa[[1]]$Samples
      sample.meta <- data.frame(depths = sapply(samples, function(x) x$AnalysisUnitDepth),
                                thickness = sapply(samples, function(x) x$AnalysisUnitName),
                                ldply(samples, function(x) data.frame(x$SampleAges)),
                                IDs = sapply(samples, function(x) x$SampleID),
                                sample.name = sapply(samples, function(x) {
                                  if(is.null(x$SampleUnitName)) NA
                                  else x$SampleUnitName}),
                                unit.name = sapply(samples, function(x) x$AnalysisUnitName))
<<<<<<< HEAD

=======

>>>>>>> 9a7a9118a815defe72427557322d5a304b85e28d
      sample.data <- suppressMessages(melt(llply(samples, function(x) ldply(x$SampleData, data.frame))))
      taxon.list <- sample.data[!duplicated(sample.data[,1]),1:5]

      count.table <- xtabs(value ~ L1 + TaxonName, sample.data)

      aa <- list(metadata = meta.data,
                 pi.data = pi.data,
                 site.data = site.data,
                 sample.meta = sample.meta,
                 taxon.list = taxon.list,
                 counts = count.table)
    }
  }

  if(class(aa) == 'try-error') aa <- aa

  aa

<<<<<<< HEAD
}
=======
}
>>>>>>> 9a7a9118a815defe72427557322d5a304b85e28d
