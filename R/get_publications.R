
#' A file to get publications for sites or datasets in the Neotoma Database using the API.
#' 
#' The function takes the parameters, defined by the user, and returns a table with 
#'    publication information from the Neotoma Paleoecological Database.
#' 
#' @import RJSONIO RCurl plyr
#' @param pubid Numeric Publication ID value, either from \code{get.datasets} or known.
#' @param contactid Numeric Contact ID value, either from \code{get.datasets} or \code{get.contacts}
#' @param datasetid Numeric Dataset ID, known or from \code{get.datasets}
#' @param author Character string for full or partial author's name.  Can include wildcards such as 'Smit*' for all names beginning with 'Smit'.
#' @param pubtype Character string, one of eleven allowable types, see \code{get.table('PublicationTypes')}
#' @param year Numeric publication year.
#' @param search A character string to search for within the article citation.
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A table is returned with several fixed columns, and a variable number 
#'    of author fields:
#'    
#' \itemize{
#'  \item{PublicationID}{Unique database record identifier for the publication.}
#'  \item{PubType}{Publication type}
#'  \item{Year}{Year of publication.}
#'  \item{Citation}{The complete citation in a standard style. For legacy citations inherited from other databases, this field holds the citation as ingested from the other databases.}
#'  \item{Authors}{Array of author objects, can be of variable length.  Includes \code{Authors.ContactName.n}, \code{Authors.ContactID.n}, \code{Authors.Order.n}, where n ranges from 1 to the maximum number of authors returned by the API call.  When the maximum number of authors is 1 the number is excluded.}
#' }
#' @examples \dontrun{
#' #  To find all publications from 1998:
#' year.cont <- get_publication(year = 1998)
#' 
#' # To find all data contributors who have the last name "Smith"
#' smith.cont <- get_publication(author = 'Smith')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export 
get_publication <- function(pubid, contactid, datasetid, author, pubtype, year, search){
  
  base.uri <- 'http://api.neotomadb.org/v1/data/publications'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on pubid:
  if('pubid' %in% names(cl)){
    if(!is.numeric(cl$pubid)){
      stop('The pubid must be numeric.')
    }
  }
  
  #  Parameter check on contactid:
  if('contactid' %in% names(cl)){
    if(!is.numeric(cl$contactid)){
      stop('The contactid must be numeric.')
    }
  }
  
  #  Parameter check on datasetid:
  if('datasetid' %in% names(cl)){
    if(!is.numeric(cl$datasetid)){
      stop('The datasetid must be numeric.')
    }
  }
  
  #  Parameter check on author:
  if('author' %in% names(cl)){
    if(!is.character(cl$author)){
      stop('The author must be a character string.')
    }
  }
  
  if('pubtype' %in% names(cl)){
    if(!is.character(cl$pubtype)){
      stop('The pubtype must be a character string.  Use get.table(\'PublicationTypes\') to find acceptable tables.')
    }
  }
  
  if('year' %in% names(cl)){
    if(!is.numeric(cl$year)){
      stop('The year used must be numeric.')
    }
  }
  
  #  Parameter check on author:
  if('search' %in% names(cl)){
    if(!is.character(cl$search)){
      stop('The search string must be a character string.')
    }
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    names(aa) <- sapply(aa, function(x)x$SiteName)
    output <- suppressMessages(cast(melt(lapply(aa, data.frame)))[,-2])
  }
  
  output
}
