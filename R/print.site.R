#' Print function for the \code{site} class.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a very brief discription of the \code{site} object as a \code{data.frame}
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export

print.site <- function(x){
  class(x) <- 'data.frame'

  print(x)
}
