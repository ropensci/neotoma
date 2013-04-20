
#' Get Neotoma value tables.
#' 
#' @import RJSONIO RCurl plyr
#' @param table.name Call one of the available tables in the Neotoma Database.  
#'    A full listing of tables can be found here: \url{http://api.neotomadb.org/doc/resources/dbtables}.  
#'    By default it returns all objects in the table.
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details A table of values corresponding to the parameter of interest.
#' @examples \dontrun{
#' taxon.table <- get_table('Taxa')
#' 
#' #  Get the frequency of the first ten taxa in Neotoma.
#' tester <- function(x){ length(get.datasets(taxonname=x)) }
#' taxon.counts <- ldply(as.character(taxon.table$TaxonName)[1:10], tester, .progress='text')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export 
get_table <- function(table.name = NULL){
  
  base.uri <- 'http://api.neotomadb.org/v1/dbtables/'
  
  name.list <- c('AgeTypes','AggregateChronologies',
                 'AggregateDatasets','AggregateOrderTypes',
                 'AggregateSampleAges','AggregateSamples',
                 'AnalysisUnits','ChronControls',
                 'ChronControlTypes','Chronologies',
                 'CollectionTypes','CollectionUnits',
                 'Collectors','Contacts','ContactStatuses',
                 'Data','DatasetPIs','DatasetPublications',
                 'Datasets','DatasetSubmissions',
                 'DatasetSubmissionTypes','DatasetTypes',
                 'DepAgents','DepAgentTypes','DepEnvtTypes',
                 'EcolGroups','EcolGroupTypes','EcolSetTypes',
                 'FaciesTypes','Geochronology','GeochronPublications',
                 'GeochronTypes','GeoPoliticalUnits','Keywords',
                 'Lithology','Projects','PublicationAuthors',
                 'PublicationEditors','Publications',
                 'PublicationTypes','RelativeAgePublications',
                 'RelativeAges','RadiocarbonCalibration',
                 'RelativeAgeScales','RelativeAgeUnits',
                 'RelativeChronology','RepositoryInstitutions',
                 'RepositorySpecimens','SampleAges','SampleAnalysts',
                 'SampleKeywords','Samples','SiteImages','Sites',
                 'SiteGeoPolitical','SpecimenDates','Synonyms',
                 'SynonymTypes','Taxa','TaxaGroupTypes',
                 'Tephrachronology','Tephras','Variables',
                 'VariableContexts','VariableElements',
                 'VariableModifications','VariableUnits')
  
  if(!is.null(table.name)){
    is.match <- pmatch(toupper(table.name), toupper(name.list))
    if(!is.na(is.match)){
      table.name <- name.list[is.match]
      
      aa <- suppressWarnings(try(fromJSON(getForm(paste(base.uri, table.name, '?limit=all',sep=''),
                                                  binary = FALSE), nullValue = NA)))
      
      if(aa[[1]] == 1){
        allnames <- unique(laply(aa[[2]], names))
        
        table <- as.data.frame(matrix(unlist(laply(aa[[2]], unlist)), ncol=length(allnames)))
        colnames(table) <- allnames
        
      }
      else{
        warning(paste(aa[[2]], ' Returning list of acceptable variable names.', sep = ''))
        aa <- list(); aa[[1]] <- 0
      }
      
    }
    else{
      warning(paste('Unable to find match for variable name: ', table.name, '. Returning list of acceptable variable names.', sep = ''))
      aa <- list(); aa[[1]] <- 0
    }
  }
  else{
    warning(paste('No variable name supplied. Returning list of acceptable variable names.', sep = ''))
    aa <- list(); aa[[1]] <- 0
  }
  
  if(aa[[1]] == 1){ out <- table }
  else{ out <- name.list }
  
  out
}
