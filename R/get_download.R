
#' Function to return full dataset records.
#'
#' Using the dataset ID, return all records associated with the data.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.   This function will only download one dataset at a time.
#'
#' @import RJSONIO RCurl
#' @param datasetid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{get_datasets}.
#' @param verbose logical; should messages on API call be printed?
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either a 'try-error' definined by the error returned
#'    from the Neotoma API call, or a list comprising the following items:
#'
#' \itemize{
#'  \item{metadata}{A table describing the collection, including dataset information, PI data compatable with \code{get_contacts} and site data compatable with \code{get_sites}.}
#'  \item{sample.meta}{Dataset information for the core, primarily the age-depth model and chronology.  In cases where multiple age models exist for a single record the most recent chronology is provided here.}
#'  \item{taxon.list}{The list of taxa contained within the dataset, unordered, including information that can be used in \code{get_taxa}}
#'  \item{counts}{The assemblage data for the dataset, arranged with each successive depth in rows and the taxa as columns.  All taxa are described in \code{taxon.list}, the chronology is in \code{sample.data}}
#'  \item{lab.data}{A data frame of laboratory data, such as exotic pollen
#'  spike, amount of sample counted, charcoal counts, etc.}
#'  \item{chronologies}{A list of existing chronologies.  If only a single chronology exists for a record then this is the same as the age-model in \code{sample.meta}.}
#' }
#'
#'    A full data object containing all the relevant assemblage information and metadata neccessary to understand a site.
#'    The data object is a list of lists and data.frames that describe an assemblage, the constituent taxa, the chronology, site and PIs who contributed the data.
#'    NOTE: The function returns a warning in cases where single taxa are defined by multiple taphonomic characteristics, for example grains that are identified seperately as crumpled and torn in the same sample and sums these values within a sample.
#' @examples \dontrun{
#' #  Search for sites with "Pseudotsuga" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_datasets(taxonname='*Pseudotsuga*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get the dataset for all records:
#' pollen.records <- get_download(sapply(t8kyr.datasets, function(x) x$DatasetID))
#'
#' #  Standardize the taxonomies for the different records using the WS64 taxonomy.
#' compiled.sites <- lapply(pollen.records, function(x) compile_list(x, list.name='WS64'))
#'
#' #  Extract the Cupressaceae curves for the sites:
#' get.curve <- function(x, taxa) {
#'                data.frame(site = x$metadata$site.data$SiteName,
#'                age = x$sample.meta$Age,
#'                count = x$counts[,taxa]/rowSums(x$counts, na.rm=TRUE))
#'              }
#'              
#' curves <- do.call(rbind.data.frame,
#'                   lapply(compiled.sites, get.curve, taxa = 'Larix/Pseudotsuga'))
#' 
#' #  For illustration, remove the sites with no Pseudotsuga occurance:
#' curves <- curves[curves$count > 0, ]
#' 
#' smooth.curve <- predict(loess(sqrt(count)~age, data=curves), data.frame(age=seq(20000, 0, by = -100)))
#' plot(sqrt(count) ~ age, data = curves,
#'      ylab = '% Pseudotsuga/Larix', xlab='Calibrated Years BP', pch=19,
#'      col=rgb(0.1, 0.1, 0.1, 0.1), xlim=c(0, 20000))
#' lines(seq(20000, 0, by = -100), smooth.curve, lwd=2, lty=2, col=2)
#'
#' #  This map shows us an apparent peak in Larix/Pseudotsuga pollen in the early-Holocene that
#' #  lends support to a warmer, drier early-Holocene in western North America.
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get_download <- function(datasetid, verbose = TRUE){

    ## Updated the processing here. There is no need to be fiddling with
    ## call. Use missing() to check for presence of argument
    ## and then process as per usual
    base.uri <- 'http://api.neotomadb.org/v1/data/downloads'

    if(missing(datasetid)) {
        stop(paste(sQuote("datasetid"), "must be provided."))
    } else {
        if(!is.numeric(datasetid))
            stop('datasetid must be numeric.')
    }

    get.sample <- function(x){
      ## query Neotoma for data set
      aa <- try(fromJSON(paste0(base.uri, '/', x), nullValue = NA))
  
      ## Might as well check here for error and bail
      if(inherits(aa, "try-error"))
          return(aa)
  
      ## if no error continue processing
      if (isTRUE(all.equal(aa[[1]], 0))) {
          stop(paste('Server returned an error message:\n', aa[[2]]),
               call.=FALSE)
      }
  
      if (isTRUE(all.equal(aa[[1]], 1))) {
          aa <- aa[[2]]
        
          if(verbose) {
              message(strwrap(paste("API call was successful. Returned record for ",
                                       aa[[1]]$Site$SiteName)))
          }
  
          ##  Here the goal is to reduce this list of lists to as
          ##  simple a set of matrices as possible.
          nams <- names(aa[[1]])
          aa1 <- aa[[1]]
          
          #  If there are actual stratigraphic samples with data in the dataset returned.
          if ('Samples' %in% nams) {
              
            ## Build the metadata for the dataset.
              meta.data <- list(
                  dataset = data.frame(dataset.id = aa1$DatasetID,
                                       dataset.name = aa1$DatasetName,
                                       collection.type = aa1$CollUnitType,
                                       collection.handle = aa1$CollUnitHandle,
                                       dataset.type =  aa1$DatasetType, stringsAsFactors = FALSE),
                  site.data = as.data.frame(aa1$Site[c('SiteID', 'SiteName',
                                           'Altitude','LatitudeNorth',
                                           'LongitudeWest','LatitudeSouth',
                                           'LongitudeEast','SiteDescription',
                                           'SiteNotes')], stringsAsFactors = FALSE),
                  pi.data = aa1$DatasetPIs)
  
              ## copy to make indexing below easier?
              samples <- aa1$Samples
  
              ## Build the metadata for each sample in the dataset.
              sample.meta <- do.call(rbind.data.frame,
                                     lapply(samples, `[`,
                                            c("AnalysisUnitDepth",
                                              "AnalysisUnitThickness",
                                              "SampleID", "AnalysisUnitName"
                                              )))
  
              ## sample age data
              ## not all depths have the same number of chronologies, which is a bit annoying,
              ## and actually more complicated than I originally thought.
              
              ## There may be multiple chronologies associated with each record, and not all chronologies
              ## will cover the entire core, which makes things frustrating and difficult.
              
              #  first, get all unique chronology names.  Some cores don't have age models, so we use a try function.
              chrons <- try(unique(unlist((sapply(samples, function(x)sapply(x$SampleAges, function(x)x$ChronologyName))))), silent=TRUE)
              
              base.frame <- as.data.frame(matrix(ncol = 6, nrow = nrow(sample.meta)))
              colnames(base.frame) <- c('AgeOlder', 'Age', 'AgeYounger', 'ChronologyName', 'AgeType', 'ChronologyID')
              
              if(!class(chrons) == 'try-error'){
                #  Create the list:
                chron.list <- list()
                for(i in 1:length(chrons)) chron.list[[i]] <- base.frame
                names(chron.list) <- chrons
                
                for(i in 1:length(samples)){
                  for(j in 1:length(samples[[i]]$SampleAges)){
                    chron.list[[ samples[[i]]$SampleAges[[j]]$ChronologyName ]][i,] <- data.frame(samples[[i]]$SampleAges[[j]], stringsAsFactors = FALSE)
                  }
                }
              }
              if(class(chrons)=='try-error'){
                chron.list <- list(base.frame)
              }
                
              ## sample names - can be NULL hence replace with NA if so
              tmp <- sapply(sample.names <-
                            lapply(samples, `[[`, "SampleUnitName"), is.null)
              sample.names[tmp] <- NA
  
              ## stick all that together, setting names, & reordering cols
              ## the most recent age model is provided as the default.
              sample.meta <- cbind.data.frame(sample.meta, chron.list[[length(chron.list)]],
                                              unlist(sample.names))
              names(sample.meta) <- c("depths", "thickness", "IDs", "unit.name",
                                      colnames(chron.list[[length(chron.list)]]), "sample.name")
              
              sample.meta <- sample.meta[, c(1:2, 5:10, 3, 11, 4)]
  
              ## sample data/counts
              ##  1) extract each SampleData component & then rbind. Gives a
              ##     list of data frames
              sample.data <- lapply(lapply(samples, `[[`, "SampleData"),
                                    function(x) do.call(rbind.data.frame, x))
              ##  2) How many counts/species in each data frame?
              nsamp <- sapply(sample.data, nrow)
              ##  3) bind each data frame - result is a data frame in long format
              sample.data <- do.call(rbind, sample.data)
              ##  4) add a Sample column that is the ID from smaple.meta
              sample.data$Sample <- rep(sample.meta$IDs, times = nsamp)
  
              #  We're going to isolate the count data and clean it up by excluding
              #  lab data and charcoal.  The charcoal exclusion needs some further consideration.
              take <- !(sample.data$TaxaGroup == "Laboratory analyses" | sample.data$TaxaGroup == "Charcoal")
              
              count.data <- sample.data[take, ]
              
              ## Ensure duplicate taxa are renamed (if variable context is different)
              count.data$TaxonName <- as.character(count.data$TaxonName)
              var.context <- !is.na(count.data$VariableContext)
              count.data$TaxonName[var.context] <- paste(count.data$TaxonName, count.data$VariableContext, sep='.')[var.context]
              
              ## data frame of unique taxon info.  This gets included in the final dataset output by the function.
              taxon.list <- sample.data[!duplicated(sample.data$TaxonName), 1:5]
  
              #  Some taxa/objects get duplicated because different identifiers for taphonomic modification
              #  get excluded in the API table.  Because the data is excluded we can't be sure that the
              #  modifications map exactly from sample to sample, so here we just sum all duplicated taxa and
              #  throw a warning to the user:
              mod.dups <- duplicated(count.data[,c(1,7)])
              
              if(sum(mod.dups) > 0){
                tax.dups <- unique(count.data$TaxonName[duplicated(count.data[,c(1,7)])])
                if(length(tax.dups) == 1){
                  message <- paste('\nModifiers seem absent from the taxon ', tax.dups, '. \nget_download will sum at depths with multiple entries to resolve the problem.', sep = '')
                }
                if(length(tax.dups) > 1){
                  tax.dups.col <- paste(tax.dups, collapse = ', ')
                  message <- paste('\nModifiers seem absent from the taxons ', tax.dups.col, '. \nget_download will sum at depths with multiple entries to resolve the problem.', sep = '')
                }
                warning(immediate. = TRUE, message, call. = FALSE)
              }
              
              ## reshape long sample.data into a sample by taxon data frame
              ## take here *only* counts - but needs work FIXME
              counts <- dcast(count.data,
                              formula = Sample ~ TaxonName, value.var = "Value", fun.aggregate = sum, na.rm=TRUE)
              
              ## add Sample col as the rownames
              rownames(counts) <- counts$Sample
              ## remove the Sample col, but robustly
              counts <- counts[, -which(names(counts) == "Sample")]
              
              ## It is possible that some depths have no count data, but that they were sampled.
              ## This will be reflected as a row with '0' counts for all taxa.
              if(any(!sample.meta$IDs %in% rownames(counts))){
                no.missing <- sum(!sample.meta$IDs %in% rownames(counts))
                
                for(i in 1:no.missing){
                  counts <- rbind(counts, rep(NA, ncol(counts)))
                }
                rownames(counts)[(nrow(counts)+1 - no.missing):nrow(counts)] <- sample.meta$IDs[!sample.meta$IDs %in% rownames(counts)]
                
                counts <- counts[as.character(sample.meta$IDs),]
              }
              
              ## Pull out the lab data and treat it in the same way as the previous:
              take <- sample.data$TaxaGroup == "Laboratory analyses" | sample.data$TaxaGroup == "Charcoal"
              lab.data <- sample.data[take, ]
              
              if(nrow(lab.data) > 0) {
                  lab.data$LabNameUnits <- paste0(lab.data$TaxonName, " (",
                                                  lab.data$VariableElement, ": ",
                                                  lab.data$VariableUnits, ")")
                  
                  mod.dups <- duplicated(lab.data[,c(1,7)])
                  
                  if(sum(mod.dups) > 0){
                    lab.dups <- unique(lab.data$TaxonName[duplicated(lab.data[,c(1,7)])])
                    if(length(lab.dups) == 1){
                      message <- paste('\nModifiers are absent from the lab object ', lab.dups, '. \nget_download will use unique identifiers to resolve the problem.', sep = '')
                    }
                    if(length(lab.dups) > 1){
                      lab.dups.col <- paste(lab.dups, collapse = ', ')
                      message <- paste('\nModifiers are absent from the lab objects ', lab.dups.col, '. \nget_download will use unique identifiers to resolve the problem.', sep = '')
                    }
                    warning(immediate. = TRUE, message, call. = FALSE)
                  }
                  
                  lab.data <- dcast(lab.data, formula = Sample ~ LabNameUnits,
                                    value.var = "Value")
              } else {
                  lab.data <- NA
              }
  
              ## stick all this together
              aa <- list(metadata = meta.data,
                         sample.meta = sample.meta,
                         taxon.list = taxon.list,
                         counts = counts,
                         lab.data = lab.data,
                         chronologies = chron.list)
          }
      }
      aa
    }
    

    if(length(datasetid) == 1) aa <- get.sample(datasetid)
    else                       aa <- lapply(datasetid, get.sample)
    
    aa
    
}
