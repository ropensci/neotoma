#' @export
print.download_list <- function(x, ...){
  
  dates <- range(sapply(lapply(x, '[[', 'dataset'), '[[', 'access.date'))
  sites <- sapply(lapply(lapply(x, '[[', 'dataset'), '[[', 'site.data'), '[[', 'site.name')
  dataset.id <- sapply(lapply(lapply(x, '[[', 'dataset'), '[[', 'dataset.meta'), '[[', 'dataset.id')
  
  types <- sapply(lapply(get_dataset(x), '[[', 'dataset.meta'), '[[', 'dataset.type')
  
  #  Get minimum and maximum ages from the dataset object within a download:
  site_ages <- function(x){
    if(all(is.na(x$sample.meta[,c('age.older', 'age', 'age.younger')]))){
      age.set <- c(NA, NA)
    } else{
      age.set <- suppressWarnings(range(as.vector(x$sample.meta[,c('age.older', 'age', 'age.younger')]), na.rm=TRUE))
    }
    
    age.set
    
  }
  
  age.set <- suppressWarnings(t(sapply(x, site_ages)))
  
  age.set[!is.finite(age.set)] <- NA
  colnames(age.set) <- c('age.younger', 'age.older')
  
  #  Get site locations:
  locs <- get_site(x)[,c('long', 'lat')]
  
  cat(paste0('A download_list containing ', length(x), ' objects:\n',
           'Accessed from ', 
           format(as.POSIXct(dates[1], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h to ',
           format(as.POSIXct(dates[2], origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
           'h. \n',
           'Datasets:\n'))
  print(format(data.frame(dataset.id, 
                          site.name = sites, 
                          locs, 
                          age.set,
                          type = types), 
               justify='left'), row.names=FALSE)
  
  NULL
}
