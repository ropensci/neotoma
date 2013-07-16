
#' Function to convert assemblage taxa to standardized lists.
#'
#' From the assemblage data for the core return assemblage data with the assemblage taxa
#'
#' @import RJSONIO RCurl plyr
#' @param data A pollen object returned by \code{get_download}.
#' @param list.name The taxon compilation list, one of a set of lists from the literature (e.g., P25, Whitmore).  More detail in the Description.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns a list object containing \code{count} and \code{taxon.list} objects, similar to those associated with the \code{get_download} object.  Any pollen taxon not included in the major taxa defined in the pollen gets returned as 'Other'.
#'
#' \itemize{
#'  \item{taxon.list}{  The list of taxa contained within the dataset, unordered, including information that can be used in \code{get_taxa}}
#'  \item{counts}{  The assemblage data for the dataset, arranged with each successive depth in rows and the taxa as columns.  All taxa are described in \code{taxon.list}, the chronology is in \code{sample.data}}
#' }
#'
#'    The data object uses the smaller pollen subset.  As this package develops we will add the
#'    capacity to summarize data output from the translation.
#'    Currently we can return only subsets that have been defined in the literature.  These lists include:
#'  \itemize{
#'   \item{P25}{ This list is derived from Gavin et al., (2003), and includes 25 pollen taxa.}
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
#'
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

compile_list <- function(sample, list.name){

  #  List.name must be an acceptible list, including:
  #  P25 (from Gavin et al)
  #  Whitmore (full)
  #  Whit_trunc (from Goring et al)

  data(taxon.list)
  data(translate.table)
  translate.table <- apply(translate.table, 2, as.character)

  #  These can be deprecated if we decide to include the taxon table as a fixed data object in the project.

  #  A lot of the numbers from Neotoma get returned as factors.  It's annoying, this function
  #  helps.
  as.num <- function(x) as.numeric(as.character(x))

  #  Returns the TaxonIDs of the assemblages, by finding the equivalents in the larger taxon table.
  sets <- as.num(taxon.list$TaxonID[match(sample$taxon.list[,1], taxon.list$TaxonName)])

  #  The transformation table is a tab delimited table with a list of TaxonIDs associated with the lower
  #  taxa nested within each set taxon.
  taxa.list <- llply(strsplit(translate.table[translate.table[,1] == list.name,3], split=', '), as.numeric)

  new.listname <- rep(NA, length(sets))

  for(i in seq_along(sets)){
    taxa.class <- laply(taxa.list, function(x)any(sets[i] %in% x))
    if(any(taxa.class))  new.listname[i] <- translate.table[taxa.class, 2]
    else  new.listname[i] <- 'Other'
  }

  #  This is the actual transformation,
  new.samp <- sample$count
  colnames(new.samp) <- new.listname
  new.samp <- dcast(melt(new.samp), L1 ~ TaxonName, sum)[,-1]

  #  We want to make a taxon list like the one returned in get_downloads:
  taxon.type <- get_table('TaxaGroupTypes')
  taxon.element <- get_table('VariableElements')
  taxon.variables <- get_table('Variables')
  taxon.varunits <- get_table('VariableUnits')

  taxon.ids <- as.num(with(taxon.list, TaxonID[which(TaxonName %in% colnames(new.samp))]))
  taxon.variables <- taxon.variables[taxon.variables$TaxonID %in% taxon.ids,]

  taxon.out <- sample$taxon.list

  taxon.out$NewCol <- new.listname
  colnames(taxon.out)[colnames(taxon.out) == 'NewCol'] <- paste(list.name, '_comp', sep='')

  #  Returns a data.frame with taxa in the columns and samples in the rows.
  return(list(taxon.list = taxon.out,
              counts = new.samp))
}
