#' @export
print.download <- function(x, ...){
  class(x) <- 'list'

  cat(paste0('A download object for site ',
           x$metadata$site.data$SiteName, '\n',
           'Accessed ', format(x$metadata$access.date, "%Y-%m-%d %H:%M"), 'h. \n'))

  NULL
}
