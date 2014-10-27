
#' Function to bind objects together into a longer object.
#'
#' From multiple \code{download*}s, \code{dataset*}s or \code{site}s, join them together into a single object.
#'
#' To support further synthesis and analysis \code{compile_download} works to transform a list
#' returned by \code{\link{get_download}} into a large data frame with columns for site and sample attributes
#' and also with the associated assemblage data at each sample depth.  This function also does the same for
#' single sites.
#'
#' @param x A n object returned by one of the \code{get_*} commands for download, site or dataset.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns a larger list.
#'
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.poa <- get_dataset(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#' t8kyr.canis <- get_dataset(taxonname='Canis*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' t8kyr.co_site <- bind(t8kyr.poa, t8kyr.canis)
#' 
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#'
#' @keywords utilities
#' @export

bind <-function(...){
  
  inputs <- list(...)
  
  if(!length(inputs)>1){
    stop('You must pass more than one object into bind.')
  }
  
  classes <- sapply(inputs, function(x)unlist(class(x)))
  
  if(all(classes[1,] == 'download_list')){
    new.list <- do.call(c, inputs)
    class(new.list) <- c('download_list', 'list')
    return(new.list)
  }
  
  if(all(classes[1,] == 'dataset_list')){
    new.list <- do.call(c, inputs)
    class(new.list) <- c('dataset_list', 'list')
    return(new.list)
  }
  
  if(all(classes[1,] == 'site')){
    new.list <- do.call(rbind.data.frame, inputs)
    class(new.list) <- c('site', 'data.frame')
    return(new.list)
  }
  
  if(!all(classes[1,] == 'download_list') & all(classes[1,] %in% c('download_list', 'download'))){
    #  Turn them into download_lists first.
    inputs <- lapply(inputs, function(x){
      if(class(x)[1] == 'download'){
        x <- list(x)
        class(x) <- c('download_list', 'list')
      }
      x})
    
    new.list <- do.call(c, inputs)
    class(new.list) <- c('download_list', 'list')
    return(new.list)
    
  }
  
  if(!all(classes[1,] == 'download_list') & all(classes[1,] %in% c('download_list', 'download'))){
    #  Turn them into dataset_lists first.
    x <- lapply(inputs, function(x){
      if(class(x)[1] == 'dataset'){
        x <- list(x)
        class(x) <- c('dataset_list', 'list')
      }
      x})
    
    new.list <- do.call(c, inputs)
    class(new.list) <- c('dataset_list', 'list')
    return(new.list)
    
  }
}
