
#' Function to convert assemblage taxa to standardized lists.
#'
#' From the assemblage data for the core return assemblage data with the assemblage taxa
#'
#' @import RJSONIO RCurl
#' @param object A pollen object returned by \code{get_download}.
#' @param list.name The taxon compilation list, one of a set of lists from the literature (e.g., P25, Whitmore).  More detail in the Description.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns a list object with the same structure as the parent pollen object returned by \code{get_download}, or a \code{matrix} (or \code{data.frame}) depending on whether \code{object} is one or the other.  Any pollen taxon not included in the major taxa defined in the pollen gets returned as 'Other'.
#'
#' \itemize{
#'  \item{object}{Either a pollen object as returned by \code{get_download} or a \code{matrix} or \code{data.frame} containing the pollen data with rows representing strata and columns representing individual pollen taxa.}
#'  \item{list.name}{One of a set of predefined lists, 'P25', 'WS64', 'WhitmoreFull', or 'WhitmoreSmall'.  See below for more details.}
#'  \item{cf}{Should taxa listed as *cf*s (*e.g.*, *cf*. *Gilia*) be considered highly resolved?}
#'  \item{type}{Should taxa listed as types (*e.g.*, *Iva annua*-type) be considered highly resolved?}
#' }
#'
#'    The data object uses the smaller pollen subset.  As this package develops we will add the
#'    capacity to summarize data output from the translation.
#'    Currently we can return only subsets that have been defined in the literature.  These lists include:
#'  \itemize{
#'   \item{P25}{ This list is derived from Gavin et al., (2003), and includes 25 pollen taxa.}
#'   \item{WS64}{  This list is derived from Williams and Shuman (2008).}
#'   \item{WhitmoreFull}{  This is the full list associated with the Whitmore et al., (2008) North American Modern Pollen Database.}
#'   \item{Whitmore}{  As above, but taxa for which both fully resolved and undifferentiated exist these taxa are summed.}
#'  }
#'
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
#' GOLDKBG <- get_download(t8kyr.datasets[[1]]$DatasetID)
#'
#' gold.p25 <- compile_list(GOLDKBG, 'P25')
#'
#'
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' Williams J, Shuman B. 2008. Obtaining accurate and precise environmental reconstructions from the modern analog technique and North American surface pollen dataset. Quaternary Science Reviews. 27:669-687. http://dx.doi.org/10.1016/j.quascirev.2008.01.004
#'
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

compile_list <- function(object, list.name, cf = TRUE, type = TRUE){

  if(!class(object) %in% c('list', 'matrix', 'data.frame')){
    stop('Data object must be a pollen object returned by function get_download or a matrix or data.frame')
  }
  
  data(pollen.equiv)
  avail.lists <- c('P25', 'WS64', 'WhitmoreFull', 'WhitmoreSmall')
  
  if(cf == FALSE)   list.name <- list.name[is.na(pollen.equiv$cf)]
  if(type == FALSE) list.name <- list.name[is.na(pollen.equiv$type)]
  
  use.list <- which(avail.lists %in% list.name)
  
  if(class(object) == 'list'){
    used.taxa <- pollen.equiv[match(colnames(object$counts), pollen.equiv$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 2])
    agg.list[is.na(agg.list)] <- 'Other'
    
    compressed.list <- aggregate(t(object$counts), by = list(agg.list), sum, na.rm=TRUE)
  
    compressed.cols <- compressed.list[,1]
    
    compressed.list <- t(compressed.list[,-1])
    colnames(compressed.list) <- compressed.cols
    
    #  We want to make a taxon list like the one returned in get_downloads:
    new.list <- object$taxon.list
    new.list$compressed <- NA
    
    new.list$compressed <- pollen.equiv[match(new.list$TaxonName, pollen.equiv$taxon),use.list + 2]
    
    new.list$compressed[is.na(new.list$compressed) & new.list$TaxonName %in% colnames(object$counts)] <- 'Other'
  
    #  Returns a data.frame with taxa in the columns and samples in the rows.
    output <- list(metadata = object$metadata,
                   sample.meta = object$sample.meta,
                   taxon.list = new.list, 
                   counts = compressed.list,
                   lab.data = object$lab.data)
  }
  if(class(object) %in% c('matrix', 'data.frame')){
    used.taxa <- pollen.equiv[match(colnames(object), pollen.equiv$taxon),]
    agg.list <- as.vector(used.taxa[,use.list + 2])
    agg.list[is.na(agg.list)] <- 'Other'

    compressed.list <- aggregate(t(object), by = list(agg.list), sum, na.rm=TRUE)
    
    compressed.cols <- compressed.list[,1]
    
    compressed.list <- t(compressed.list[,-1])
    colnames(compressed.list) <- compressed.cols
    
    output <- compressed.list
  }
  
  return(output)
  
}
