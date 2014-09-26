#' @title Get contact information.
#' @description Deprecated - see \code{get_contact}.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getForm
#' @importFrom reshape2 dcast melt
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords IO connection
#' @export
get_contacts <- function(contactid, contactname, contactstatus, familyname){

  .Deprecated('get_contact')
  
  base.uri <- 'http://api.neotomadb.org/v1/data/contacts'

  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())

  #  Parameter check on contactid:
  if('contactid' %in% names(cl)){
    if(!is.numeric(cl$contactid)){
      stop('The contactid must be numeric.')
    }
  }

  #  Parameter check on contactname:
  if('contactname' %in% names(cl)){
    if(!is.character(cl$contactname)){
      stop('The contactname must be a character string.')
    }
  }

  #  Parameter check on contactstatus:
  if('contactstatus' %in% names(cl)){
    if(!is.character(cl$contactstatus)){
      stop('The contactstatus must be a character string.')
    }
    else{
      if(!cl$contactstatus %in% c('active', 'deceased', 'defunct',
                                  'extant', 'inactive', 'retired', 'unknown')){
        stop('status must be an accepted term.  Use get.table(\'ContactStatues\')')
      }
    }
  }

  #  Parameter check on familyname:
  if('familyname' %in% names(cl)){
    if(!is.character(cl$familyname)){
      stop('The familyname must be a character string.')
    }
  }

  neotoma.form <- getForm(base.uri, .params = cl)
  aa <- try(fromJSON(neotoma.form, nullValue = NA))

  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    message(paste0('The API call was successful, you have returned ', length(aa), ' records.\n'))
  }

  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    names(aa) <- sapply(aa, function(x)x$ContactID)
    output <- melt(lapply(aa, data.frame),
                   id.vars = c("Address", "URL", "GivenNames",
                   "LeadingInitials", "Fax", "Title", "Email",
                   "FamilyName", "Phone", "ContactName", "ContactStatus",
                   "Notes", "Suffix"))
    output <- dcast(output,
                    formula = ContactName + ContactStatus + FamilyName +
                    LeadingInitials + GivenNames + Suffix + Title + Phone +
                    Fax + Email + URL + Address + Notes ~ variable) ##[,-2]
  }

  output
}
