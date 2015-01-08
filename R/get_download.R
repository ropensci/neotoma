#' @title Function to return full download records using \code{site}s, \code{dataset}s, or dataset IDs.
#' @description Using the dataset ID, site object or dataset object, return all records associated with the data as a \code{download_list}.
#'
#' @importFrom RJSONIO fromJSON
#' @param x Optional parameter for a \code{site}, \code{dataset}, or \code{dataset_list}.
#' @param datasetid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{get_datasets}.
#' @param verbose logical; should messages on API call be printed?
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either object of class \code{"try-error"}' (see \code{\link{try}}) definined by the error returned from the Neotoma API call, or an object of class \code{download_list}, containing a set of \code{download} objects, each with relevant assemblage information and metadata:
#' The \code{download} object is a list of lists and data frames that describe an assemblage, the constituent taxa, the chronology, site and PIs who contributed the data. The following are important components:
#'
#'  \item{ \code{dataset} }{A table describing the collection, including dataset information, PI data compatable with \code{\link{get_contact}} and site data compatable with \code{\link{get_site}}.}
#'  \item{ \code{sample.meta} }{Dataset information for the core, primarily the age-depth model and chronology.  In cases where multiple age models exist for a single record the most recent chronology is provided here.}
#'  \item{ \code{taxon.list} }{The list of taxa contained within the dataset, unordered, including information that can be used in \code{\link{get_taxa}}}
#'  \item{ \code{counts} }{The assemblage data for the dataset, arranged with each successive depth in rows and the taxa as columns.  All taxa are described in \code{taxon.list}, the chronology is in \code{sample.data}}
#'  \item{ \code{lab.data} }{A data frame of laboratory data, such as exotic pollen spike, amount of sample counted, charcoal counts, etc.}
#'  \item{ \code{chronologies} }{A list of existing chronologies.  If only a single chronology exists for a record then this is the same as the age-model in \code{sample.meta}.}
#'
#' @section Note:
#' The function returns a warning in cases where single taxa are defined by multiple taphonomic characteristics, for example grains that are identified seperately as crumpled and torn in the same sample and sums these values within a sample.
#' In the case that a geochronology dataset is passed to \code{get_download} the function returns a message and a NULL object (that is later excized).  Use \code{get_geochron} for these objects.
#'
#' @examples \dontrun{
#' #  Search for sites with "Pseudotsuga" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_dataset(taxonname='*Pseudotsuga*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get the dataset for all records:
#' pollen.records <- get_download(t8kyr.datasets)
#'
#' #  Standardize the taxonomies for the different records using the WS64 taxonomy.
#' compiled.sites <- compile_taxa(pollen.records, list.name='WS64')
#'
#' #  Extract the Pseudotsuga curves for the sites:
#' get.curve <- function(x, taxa) {
#'                data.frame(site = x$dataset$site.data$site.name,
#'                age = x$sample.meta$age,
#'                count = x$counts[,taxa]/rowSums(x$counts, na.rm=TRUE))
#'              }
#'
#' curves <- do.call(rbind.data.frame,
#'                   lapply(compiled.sites, get.curve, taxa = 'Larix/Pseudotsuga'))
#'
#' #  For illustration, remove the sites with no Pseudotsuga occurance:
#' curves <- curves[curves$count > 0, ]
#'
#' smooth.curve <- predict(loess(sqrt(count)~age, data=curves),
#'                         data.frame(age=seq(20000, 0, by = -100)))
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
#' @keywords IO connection
#' @export
get_download <- function(x, ...){
  UseMethod('get_download')
}

#' @title Function to return full download records using \code{site}s, \code{dataset}s, or dataset IDs.
#' @description Using the dataset ID, site object or dataset object, return all records associated with the data as a \code{download_list}.
#'
#' @importFrom RJSONIO fromJSON
#' @param datasetid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{get_datasets}.
#' @param verbose logical; should messages on API call be printed?
#' @export
get_download.default <- function(datasetid, verbose = TRUE){

  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual

  if (missing(datasetid)) {
      stop(paste0("Either a ",sQuote("datasetid"), " or a dataset object must be provided."))
  }
  if (!missing(datasetid) & !is.numeric(datasetid)) {
          stop('datasetid must be numeric.')
  }

  get.sample <- function(x){
    # query Neotoma for data set
    base.uri <- 'http://api.neotomadb.org/v1/data/downloads'
    
    aa <- try(fromJSON(paste0(base.uri, '/', x), nullValue = NA))

    # Might as well check here for error and bail
    if (inherits(aa, "try-error"))
        return(aa)

    # if no error continue processing
    if (isTRUE(all.equal(aa[[1]], 0))) {
        stop(paste('Server returned an error message:\n', aa[[2]]),
             call. = FALSE)
    }

    if (isTRUE(all.equal(aa[[1]], 1))) {
        aa <- aa[[2]]

        if (verbose) {
            message(strwrap(paste0("API call was successful. ",
                                   "Returned record for ",
                                   aa[[1]]$Site$SiteName)))
        }

        # Here the goal is to reduce this list of lists to as
        # simple a set of matrices as possible.
        nams <- names(aa[[1]])
        aa1 <- aa[[1]]

        # If there are actual stratigraphic samples with data
        # in the dataset returned.

        if ('Samples' %in% nams  & length(aa1$Samples) > 0) {

          # Build the metadata for the dataset.
            dataset <- list(
              site.data = data.frame(site.id = aa1$Site$SiteID,
                                     site.name = aa1$Site$SiteName,
                                     long = mean(unlist(aa1$Site[c('LongitudeWest', 'LongitudeEast')]),
                                                 na.rm = TRUE),
                                     lat = mean(unlist(aa1$Site[c('LatitudeNorth', 'LatitudeSouth')]),
                                                na.rm = TRUE),
                                     elev = aa1$Site$Altitude,
                                     description = aa1$Site$SiteDescription,
                                     long.acc = abs(aa1$Site$LongitudeWest - aa1$Site$LongitudeEast),
                                     lat.acc = abs(aa1$Site$LatitudeNorth - aa1$Site$LatitudeSouth),
                                     row.names = aa1$Site$SiteName,
                                     stringsAsFactors = FALSE),
              dataset.meta = data.frame(dataset.id = aa1$DatasetID,
                                        dataset.name = aa1$DatasetName,
                                        collection.type = aa1$CollUnitType,
                                        collection.handle = aa1$CollUnitHandle,
                                        dataset.type =  aa1$DatasetType,
                                        stringsAsFactors = FALSE),
              pi.data = do.call(rbind.data.frame,
                                  aa1$DatasetPIs),
              submission = data.frame(submission.date = strptime(aa1$NeotomaLastSub,
                                                                 '%m/%d/%Y'),
                                      submission.type = 'Last submission to Neotoma',
                                      stringsAsFactors=FALSE),
              access.date = Sys.time())

            class(dataset) <- c('dataset', 'list')
            class(dataset$site) <- c('site', 'data.frame')
            
            #  Geochronological datasets behave differently than any other dataset.
            #  It's frustrating.  This is the only way we can figure it out in a
            #  general way.
            if(dataset$dataset.meta$dataset.type == 'geochronologic'){
              
              message(paste0('The dataset ID ', dataset$dataset.meta$dataset.id,
                             ' is associated with a geochronology object, not count data.'))
              return(NULL)
              #geochron <- get_geochron(dataset$dataset.meta$dataset.id)
              
              #aa <- list(dataset, geochron = geochron)
              #class(aa) <- 'geochron'
              #return(aa)
            }
              
            if(!dataset$dataset.meta$dataset.type == 'geochronologic'){
              # copy to make indexing below easier?
              samples <- aa1$Samples
  
              # Build the metadata for each sample in the dataset.
              sample.meta <- do.call(rbind.data.frame,
                                     lapply(samples, `[`,
                                            c("AnalysisUnitDepth",
                                              "AnalysisUnitThickness",
                                              "SampleID", "AnalysisUnitName"
                                              )))
  
              # Sample age data
              # Not all depths have the same number of chronologies,
              # which is a bit annoying, and actually more
              # complicated than I originally thought.
  
              # There may be multiple chronologies associated with each record,
              # and not all chronologies will cover the entire core,
              # which makes things frustrating and difficult.
  
              # First, get all unique chronology names.
              # Some cores don't have age models, so we use a try function.
              chrons <- try(unique(as.vector(unlist(sapply(samples,
                                                           function(x)sapply(x$SampleAges, function(x)x$ChronologyName))))), silent = TRUE)
  
              base.frame <- as.data.frame(matrix(ncol = 6,
                                                 nrow = nrow(sample.meta)))
              colnames(base.frame) <- c('age.older', 'age',
                                        'age.younger', 'chronology.name',
                                        'age.type', 'chronology.id')
  
              if (!class(chrons) == 'try-error'){
                # Create the list:
                chron.list <- list()
                for (i in 1:length(chrons)) chron.list[[i]] <- base.frame
                names(chron.list) <- chrons
  
                for (i in 1:length(samples)){
                  for (j in 1:length(samples[[i]]$SampleAges)){
                    # Some of the new datasets are passing data without any chronology information.
                    if(!is.na(samples[[i]]$SampleAges[[j]]['ChronologyName'])) {
                    
                      chron.list[[ samples[[i]]$SampleAges[[j]]$ChronologyName ]][i, ] <-
                        data.frame(samples[[i]]$SampleAges[[j]],
                                   stringsAsFactors = FALSE)
                    }
                  }
                }
              }
              if (class(chrons) == 'try-error'){
                chron.list <- list(base.frame)
              }
  
              # sample names - can be NULL hence replace with NA if so
              tmp <- sapply(sample.names <-
                            lapply(samples, `[[`, "SampleUnitName"), is.null)
              sample.names[tmp] <- NA
  
              # stick all that together, setting names, & reordering cols
              # the most recent age model is provided as the default.
              sample.meta <- cbind.data.frame(sample.meta,
                                              chron.list[[length(chron.list)]],
                                              unlist(sample.names))
              names(sample.meta) <- c("depth", "thickness",
                                      "sample.id", "unit.name",
                                      colnames(chron.list[[length(chron.list)]]),
                                      "sample.name")
  
              #  re-ordering the columns so they make sense.
              sample.meta <- sample.meta[, c(1:2, 5:10, 3, 11, 4)]
  
              # sample data/counts
              # 1) extract each SampleData component & then rbind. Gives a
              # list of data frames
              sample.data <- lapply(lapply(samples, `[[`, "SampleData"),
                                    function(x) do.call(rbind.data.frame, x))
              # 2) How many counts/species in each data frame?
              nsamp <- sapply(sample.data, nrow)
              # 3) bind each data frame - result is a data frame in long format
              sample.data <- do.call(rbind, sample.data)
              # 4) add a Sample column that is the ID from smaple.meta
              sample.data$sample.id <- rep(sample.meta$sample.id, times = nsamp)
  
              # We're going to isolate the count data and clean it up by
              # excluding lab data and charcoal.  The charcoal exclusion
              # needs some further consideration.
              
              colnames(sample.data) <- c('taxon.name', 'variable.units',
                                         'variable.element', 'variable.context',
                                         'taxon.group', 'value',
                                         'ecological.group', 'sample.id')
              
              take <- !(sample.data$taxon.group == "Laboratory analyses" |
                          sample.data$taxon.group == "Charcoal")
  
              count.data <- sample.data[take, ]
  
              # Ensure duplicate taxa are renamed
              # (if variable context is different)
              count.data$taxon.name <- as.character(count.data$taxon.name)
              var.context <- !is.na(count.data$variable.context)
              count.data$taxon.name[var.context] <- paste(count.data$taxon.name,
                                                         count.data$variable.context,
                                                         sep = '.')[var.context]
  
              # data frame of unique taxon info.  This gets included in the
              # final dataset output by the function.
              taxon.list <- sample.data[!duplicated(sample.data$taxon.name),
                                        1:5]
  
              # Some taxa/objects get duplicated because different identifiers
              # for taphonomic modification get excluded in the API table.
              # Because the data is excluded we can't be sure that the
              # modifications map exactly from sample to sample, so here we
              # just sum all duplicated taxa and throw a warning to the user:
              mod.dups <- duplicated(count.data[, c('taxon.name', 'sample.id')])
  
              if (sum(mod.dups) > 0){
                tax.dups <- unique(count.data$taxon.name[duplicated(count.data[, c('taxon.name', 'sample.id')])])
                if (length(tax.dups) == 1){
                  message <- paste0('\nModifiers seem absent from the taxon ',
                                    tax.dups,
                                    '. \nget_download will sum at depths ',
                                    'with multiple entries to resolve the problem.')
                }
                if (length(tax.dups) > 1){
                  tax.dups.col <- paste(tax.dups, collapse = ', ')
                  message <- paste0('\nModifiers seem absent from the taxons ',
                                   tax.dups.col,
                                   '. \nget_download will sum at depths with ',
                                   'multiple entries to resolve the problem.')
                }
                warning (immediate. = TRUE, message, call. = FALSE)
              }
  
              # reshape long sample.data into a sample by taxon data frame
              # take here *only* counts - but needs work FIXME
              
              if(nrow(count.data) > 0){
                counts <- dcast(count.data,
                                formula = sample.id ~ taxon.name,
                                value.var = "value",
                                fun.aggregate = sum, na.rm = TRUE)
    
                # add Sample col as the rownames
                rownames(counts) <- counts$sample.id
                ## remove the Sample col, but robustly
                counts <- counts[, -which(names(counts) == "sample.id"), drop = F]
    
                # It is possible that some depths have no count data,
                # but that they were sampled. This will be
                # reflected as a row with '0' counts for all taxa.
                if (any(!sample.meta$sample.id %in% rownames(counts))){
                  no.missing <- sum(!sample.meta$sample.id %in% rownames(counts))
    
                  for (i in 1:no.missing){
                    counts <- rbind(counts, rep(NA, ncol(counts)))
                  }
                  rownames(counts)[(nrow(counts) + 1 - no.missing):nrow(counts)] <- sample.meta$sample.id[!sample.meta$sample.id %in% rownames(counts)]
    
                  counts <- counts[as.character(sample.meta$sample.id), ]
                }
                
              } else {
                counts <- NULL
              }
    
              # Pull out the lab data and treat it in
              # the same way as the previous:
              take <- sample.data$taxon.group == "Laboratory analyses" |
                sample.data$taxon.group == "Charcoal"
  
              lab.data <- sample.data[take, ]
  
              if (nrow(lab.data) > 0) {
                  lab.data$lab.name.units <- paste0(lab.data$taxon.name, " (",
                                                  lab.data$variable.element, ": ",
                                                  lab.data$variable.units, ")")
  
                  mod.dups <- duplicated(lab.data[, c(1, 7)])
  
                  if (sum(mod.dups) > 0){
                    lab.dups <- unique(lab.data$taxon.name[duplicated(lab.data[, c(1, 7)])])
                    if (length(lab.dups) == 1){
                      message <- paste0('\nModifiers are absent from the lab object ',
                                        lab.dups,
                                        '. \nget_download will use unique',
                                        'identifiers to resolve the problem.')
                    }
                    if (length(lab.dups) > 1){
                      lab.dups.col <- paste(lab.dups, collapse = ', ')
                      message <- paste0('\nModifiers are absent from the lab objects ',
                                        lab.dups.col,
                                        '. \nget_download will use unique',
                                        'identifiers to resolve the problem.')
                    }
                    warning (immediate. = TRUE, message, call. = FALSE)
                  }
  
                  lab.data <- dcast(lab.data,
                                    formula = sample.id ~ lab.name.units,
                                    value.var = "value")
              } else {
                  lab.data <- NULL
              }
  
              # stick all this together
              aa <- list(dataset = dataset,
                         sample.meta = sample.meta,
                         taxon.list = taxon.list,
                         counts = counts,
                         lab.data = lab.data,
                         chronologies = chron.list)
            
              class(aa) <- c('download', 'list')
            }
        }
        if ((!'Samples' %in% nams) | length(aa1$Samples) == 0) {
          message('Dataset contains no sample data.')
          return(NULL)
        }
    
    }
    
    aa
    
  }

  aa <- lapply(datasetid, get.sample)
  
  aa <- aa[-(which(sapply(aa,is.null),arr.ind=TRUE))]
  
  names(aa) <- sapply(lapply(lapply(aa, '[[', 'dataset'), '[[', 'dataset.meta'), '[[', 'dataset.id')
  
  class(aa) <- c('download_list', 'list')

  aa
  
}

#' @export
get_download.dataset <- function(x, verbose = TRUE){

  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual
  
  datasetid <- x$dataset.meta$dataset.id
  
  if(!x$dataset.meta$dataset.type %in% 'geochronologic'){
    aa <- get_download(datasetid)
  } else {
    cat('Dataset is a geochronological data object.  Defaulting to get_geochron.\n')
    geochron <- get_geochron(datasetid)
    aa <- list(dataset = x,
               geochronology = geochron)
  }

  aa
}

#' @export
get_download.dataset_list <- function(x, verbose = TRUE){
  
  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual
  
  datasetid <- unlist(lapply(x, FUN=function(x)x$dataset$dataset.id))
  
  aa <- get_download(datasetid)
  
  aa
}

#' @export
get_download.site <- function(x, verbose = TRUE){
  
  dataset <- get_dataset(x)
  
  datasetid <- unlist(lapply(dataset, FUN=function(x)x$dataset$dataset.id))
  
  aa <- get_download(datasetid)
  
  aa
}
