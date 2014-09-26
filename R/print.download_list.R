#' @export
print.download_list <- function(x, ...){
  
  dates <- range(sapply(lapply(x, '[[', 'dataset'), '[[', 'access.date'))
  sites <- sapply(lapply(lapply(x, '[[', 'dataset'), '[[', 'site'), '[[', 'site.name')
  dataset.id <- sapply(lapply(lapply(x, '[[', 'dataset'), '[[', 'dataset.meta'), '[[', 'dataset.id')
  
  #  Get minimum and maximum ages for a site:
  age.set <- suppressWarnings(t(sapply(lapply(x, '[[', 'sample.meta'), 
                    FUN=function(x)range(as.numeric(x[,c('age.older', 'age', 'age.younger')]), 
                                                    na.rm=TRUE))))
  age.set[!is.finite(age.set)] <- NA
  colnames(age.set) <- c('age.younger', 'age.older')
  
  #  Get site locations:
  locs <- get_site(x)[,c('long', 'lat')]
  
  cat(paste0('A download_list containing ', length(x), ' objects:',
           x$dataset$site$site.name, '\n',
           'Accessed from ', 
           format(as.POSIXct(dates[1], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h to ',
           format(as.POSIXct(dates[2], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h. \n',
           'Datasets:\n'))
  print(format(data.frame(dataset.id, site.name = sites, locs, age.set), justify='left'), row.names=FALSE)
  
  NULL
}
