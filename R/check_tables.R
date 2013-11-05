
#' Check to make sure that neotoma linked tables are up to date.
#'
#' A function to access the Neotoma API and return tables corresponding to the
#'    parameters defined by the user.
#'
#' @import RJSONIO RCurl
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A warning, if the tables stored in the data folder are not the same as the ones online.
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
#'

check.tables <- function(){
  aa <- get_table('GeoPoliticalUnits')
  data(gp.table)
  if(!all.equal(aa, gp.table)){
    warning('The geopolitical table is out of sync with the table available from neotoma.  This may cause problems using the 
            gpid variable in get_datasets and get_sites.')
  }

}