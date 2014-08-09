#' Print function for the `download` class.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a very brief discription of the `download` object including site name and access date.
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

print.download <- function(x){
  class(x) <- 'list'
  cat(paste0('A single download object for site ', 
             x$metadata$site.data$SiteName, '\n',
             'Accessed on ', format(x$metadata$access.date, '%a %b %d %X %y'), '\n'))
  
}
