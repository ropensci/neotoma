
#' Get taxon information from Neotoma.
#' 
#' @import RJSONIO RCurl plyr
#' @param taxonid Numeric taxon identifier used in Neotoma
#' @param taxonname A character string representing the full or partial name of taxa of interest.
#' @param status The current status of the taxon, one of 'extinct', 'extant', 'all'.
#' @param taxagroup The taxonomic grouping for the taxa. See \link{http://api.neotomadb.org/doc/resources/taxa} for the list of approved groupings.
#' @param ecolgroup The ecological group of the taxa. More detailed than \code{taxagroup}, can be obtained using \code{get_table("EcolGroupTypes")}.
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a table.
#' 
#' \itemize{
#'  \item{TaxonID}{Unique database record identifier for a taxon.}
#'  \item{TaxonCode}{Shorthand notation for a taxon identification.}
#'  \item{TaxonName}{Name of the taxon.}
#'  \item{Author}{Author(s) of the name. Used almost exclusively with beetle taxa.}
#'  \item{Extinct}{True if extinct; false if extant.}
#'  \item{TaxaGroup}{Code for taxa group to which taxon belongs.}
#'  \item{EcolGroups}{Array of ecological group codes to which the taxon belongs.}
#'  \item{HigherTaxonID}{TaxonID of the next higher taxonomic rank.}
#'  \item{PublicationID}{Publication identification number.}
#'  \item{Notes}{Free-form notes or comments about the taxon.}
#' }
#' 
#' @examples \dontrun{
#' taxon.table <- get_table('Taxa')
#' 
#' #  Get the frequency of the first ten taxa in Neotoma.
#' tester <- function(x){ length(get.datasets(taxonname=x)) }
#' taxon.counts <- ldply(as.character(taxon.table$TaxonName)[1:10], tester, .progress='text')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export 
get_taxa <- function(taxonid, taxonname, status, taxagroup, ecolgroup){
  
  base.uri <- 'http://api.neotomadb.org/v1/data/taxa'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on taxagroup:
  if('taxagroup' %in% names(cl)){
    taxon.codes <- c('AVE', 'BIM', 'BRY',
                     'BTL', 'FSH', 'HRP',
                     'LAB', 'MAM', 'MOL',
                     'PHY', 'TES', 'VPL')
    
    if(!cl$taxagroup %in% taxon.codes){
      stop('taxonGroup is not an accepted code.  Use get_table(\'TaxaGroupTypes\') to obtain acceptible classes')
    }
  }
  
  #  Parameter check on taxonname and taxonids, I'm allowing only one, but I think it can accept two.
  if(any(c('taxonids', 'taxonname') %in% names(cl))){
    
    if(all(c('taxonids', 'taxonname') %in% names(cl))){
      stop('Can only accept either taxonids OR taxonname, not both.')
    }
    if('taxonids' %in% names(cl) & !is.numeric(cl$taxonids)) {
      stop('The variable taxonids must be numeric.  To obtain a list of taxon IDs use the get_table command.')
    }
    if('taxonname' %in% names(cl) & !is.character(cl$taxonname)) {
      stop('The variable taxonname must be a character string.  To obtain a list of taxon names use the get_table command.')
    }
  }
  
  if('status' %in% names(cl)){
    if(!cl$status %in% c('extinct', 'extant', 'all')){
      stop('Status must be one of: \'extinct\', \'extant\', or \'all\'')
    }
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue=NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    output <- aa[[2]]
    cat('The API call was successful, you have returned ', length(output), 'records.\n')
  }
  
  if(class(aa) == 'try-error'){ output <- neotoma.form }
  else{
    
    names(output) <- sapply(output, function(x)x$TaxonName)
    #  There are some values in here that are empy lists:
    output <- lapply(output, function(x){
      if(any(sapply(x, length) == 0)){
        x[[which(sapply(x, length) == 0)]] <- NA
      }
      x
    })
    
    output <- ldply(output, function(x) data.frame(x))  
  }
  
  output
  
}