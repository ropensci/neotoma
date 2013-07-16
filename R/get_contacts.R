
#' A function to obtain contact information for data contributors from the Neotoma
#'    Paleoecological Database.
#'
#' @import RJSONIO RCurl plyr
#' @importFrom reshape2 dcast melt
#' @param contactid Contact ID is a numerical value associated with the Neotoma
#'    Contact table's numerical Contact ID.
#' @param contactname A character string indicating the data contributors' project,
#'    organization or personal name.  May be a partial string and can include wildcards.
#' @param contactstatus The current status of the contact.  Possible values include:
#'    active, deceased, defunct, extant, inactive, retired, unknown.
#' @param familyname A character string.  Full or partial string indicating the
#'    contact's last name.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return The function takes parameters defined by the user and returns a list
#'    of contact information supplied by the Neotoma Paleoecological Database.
#'    The user may define all or none of the possible fields.  The function contains
#'    data chacks for each defined parameter.
#'
#'    The function returns either a single item of class "try-error" describing
#'    the reason for failure (either mis-defined parameters or an error from the Neotoma API),
#'    or a table of contacts, with rows corresponding to the number of individual
#'    contacts returned by the Neotoma API.  Each row entry includes the following parameters:
#'
#' \itemize{
#'  \item{ContactID}{  Unique database record identifier for the contact.}
#'  \item{AliasID}{  The ContactID of a person's current name. If the AliasID is different from the ContactID, the ContactID refers to the person's former name.}
#'  \item{ContactName}{  Full name of the person, last name first (e.g. "Simpson, George Gaylord") or name of organization or project (e.g. "Great Plains Flora Association").}
#'  \item{ContactStatus}{  Current status of the person, organization, or project. Field links to the ContactStatuses lookup table.}
#'  \item{FamilyName}{  Family or surname name of a person.}
#'  \item{LeadingInitials}{  Leading initials for given or forenames without spaces (e.g. "G.G.").}
#'  \item{GivenNames}{  Given or forenames of a person (e.g. "George Gaylord"). Initials with spaces are used if full given names are not known (e.g. "G. G").}
#'  \item{Suffix}{  Suffix of a person's name (e.g. "Jr.", "III").}
#'  \item{Title}{  A person's title (e.g. "Dr.", "Prof.", "Prof. Dr").}
#'  \item{Phone}{  Telephone number.}
#'  \item{Fax}{  Fax number.}
#'  \item{Email}{  Email address.}
#'  \item{URL}{  Universal Resource Locator, an Internet World Wide Web address.}
#'  \item{Address}{  Full mailing address.}
#'  \item{Notes}{  Free form notes or comments about the person, organization, or project.}
#' }
#' @examples \dontrun{
#' #  To find all data contributors who are active:
#' active.cont <- get_contacts(contactstatus = 'active')
#'
#' # To find all data contributors who have the last name "Smith"
#' smith.cont <- get_contacts(familyname = 'Smith')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get_contacts <- function(contactid, contactname, contactstatus, familyname){

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
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
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
