#' @export

print.site <- function(x, ...){
  class(x) <- 'data.frame'

  print(x, ...)
}
