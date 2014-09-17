
#' Function to convert multiple downloads into a single large table.
#'
#' From the assemblage data for multiple cores, return a single data.frame with columns for site
#' metadata and assemblage data.
#'
#' To support further synthesis and analysis \code{compile_download} works to transform a list
#' returned by \code{\link{get_download}} into a large data frame with columns for site and sample attributes
#' and also with the associated assemblage data at each sample depth.  This function also does the same for
#' single sites.
#'
#' @param downloads A list of downloads as returned by \code{\link{get_download}}, or mutliple sites joined in a list.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns a data frame.
#'
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_dataset(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
#' thuja.sites <- get_download(sapply(t8kyr.datasets, function(x)x$DatasetID))
#'
#' gold.p25 <- lapply(thuja.sites, compile_taxa, 'P25')
#'
#' all.gold <- compile_downloads(gold.p25)
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#'
#' Gavin DG, Oswald WW, Wahl ER, Williams JW. 2003. A statistical approach to evaluating distance metrics and analog assignments for pollen records. Quaternary Research 60: 356-367.
#'
#' Whitmore J, Gajewski K, Sawada M, Williams JW, Shuman B, Bartlein PJ, Minckley T, Viau AE, Webb III T, Shafer S, Anderson P, Brubaker L. 2005. Modern pollen data from North America and Greenland for multi-scale paleoenvironmental applications. Quaternary Science Reviews 24: 1828-1848.
#'
#' Williams J, Shuman B. 2008. Obtaining accurate and precise environmental reconstructions from the modern analog technique and North American surface pollen dataset. Quaternary Science Reviews. 27:669-687.
#'
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords utilities
#' @importFrom plyr ldply
#' @export

compile_downloads <-function(downloads){

  #  We're going to ldply the list to make a data.frame from key metadata, but
  #  first we need a function to turn a single download into a data.frame:

  if(!'download' %in% class(downloads)){
    stop('compile_datasets can only operate on lists as returned from get_download')
  }

  down.to.df <- function(x){
    if('download' %in% class(x)){
      #  There can be NULL values in the download object.  We'll turn them to NA values:
      if(is.null(x$metadata$site.data$sitename)) x$metadata$site.data$sitename <- paste('NoName_ID')
      if(is.null(x$sample.meta$depths)) x$sample.meta$depths <- NA
      if(is.null(x$sample.meta$Age)) x$sample.meta$Age <- NA
      if(is.null(x$sample.meta$AgeOlder)) x$sample.meta$AgeOlder <- NA
      if(is.null(x$sample.meta$AgeYounger)) x$sample.meta$AgeYounger <- NA
      if(is.null(x$metadata$site.data$lat)) x$metadata$site.data$lat <- NA
      if(is.null(x$metadata$site.data$long)) x$metadata$site.data$long <- NA

      site.info <- data.frame(sitename = x$metadata$site.data$sitename,
                              depth = x$sample.meta$depths,
                              age = x$sample.meta$Age,
                              ageold = x$sample.meta$AgeOlder,
                              ageyoung = x$sample.meta$AgeYounger,
                              date.type = x$sample.meta$AgeType,
                              lat = x$metadata$site.data$lat,
                              long = x$metadata$site.data$long,
                              dataset = x$metadata$dataset$dataset.id,
                              x$counts)
    }
    else{
      #  Dummy data for empty sites.
      site.info <- data.frame(sitename = NA,
                              depth = NA,
                              age = NA,
                              ageold = NA,
                              ageyoung = NA,
                              date.type = NA,
                              lat = NA,
                              long = NA,
                              dataset = NA,
                              Unknown = NA)
    }
    site.info
  }


  if('download' %in% class(downloads) & !'metadata'%in%names(downloads)){
    site.info <- ldply(downloads, down.to.df)
  }
  if('download' %in% class(downloads) & 'metadata'%in%names(downloads)){
    site.info <- down.to.df(downloads)
  }

  return(site.info)
}
