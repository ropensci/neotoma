#' Print function for the \code{download} class.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a very brief discription of the \code{download} object including site name and access date.
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

print.download <- function(x){
  class(x) <- 'list'

  if(length(x) == 1) {
    cat(paste0('A single download object for site ',
             x[[1]]$metadata$site.data$SiteName, '\n',
             'Accessed ', format(x[[1]]$metadata$access.date, "%Y-%m-%d %H:%M"), 'h. \n'))
  }
  if(length(x)>1){
    date.ranges <- sapply(range(sapply(x, function(y)y$metadata$access.date)),
                          function(z)format(as.POSIXct(z, origin="1970-01-01"), "%Y-%m-%d %H:%M"))

    cat(paste0('Downloads from ', length(x), ' sites.\n',
               'Accessed from ',date.ranges[1],'h to ', date.ranges[2],'h.\n'))
  }

  return(NULL)

}
