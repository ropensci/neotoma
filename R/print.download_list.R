#' @export
print.download_list <- function(x, ...){
  
  dates <- range(sapply(lapply(x, '[[', 'metadata'), '[[', 'access.date'))
  sites <- sapply(lapply(lapply(x, '[[', 'metadata'), '[[', 'site.data'), '[[', 'site.name')
  
  cat(paste0('A download_list containing ', length(x), ' objects:',
           x$metadata$site.data$site.name, '\n',
           'Accessed from ', 
           format(as.POSIXct(dates[1], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h to ',
           format(as.POSIXct(dates[2], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h. \n',
           'Sites include:\n'))
    

  NULL
}
