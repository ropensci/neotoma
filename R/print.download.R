#' @export
print.download <- function(x, ...){
  class(x) <- 'list'

  cat(paste0('A download object for ',
           x$dataset$site$site.name, '\n',
           'Accessed ', format(x$dataset$access.date, "%Y-%m-%d %H:%M"), 'h. \n'))

  NULL
}
