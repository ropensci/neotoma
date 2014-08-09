#' Print function for the `download.list` class.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a very brief discription of the size of the `download.list` object and the range of access dates.
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

print.download.list <- function(x){
  class(x) <- 'list'
  
  date.ranges <- sapply(range(sapply(x, function(y)y$metadata$access.date)),
                        function(z)format(as.POSIXct(z, origin="1970-01-01"), '%a %b %d %X %y'))
  
  cat(paste0('A download.list representing downloads from ', length(x), ' sites.\n',
             'Accessed from ',date.ranges[1],' to ', date.ranges[2],'.\n'))
  
}
