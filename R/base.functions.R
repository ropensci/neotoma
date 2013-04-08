##################################################################
#  Patching functions into the Neotoma API
#
#  

require(RJSONIO)
require(RCurl)
require(reshape)
require(plyr)

get.sites <- function(siteid, sitename, altmin, altmax, loc, gpid){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)

  base.uri <- 'http://api.neotomadb.org/v1/data/sites'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on siteid:
  if('siteid' %in% names(cl)){
    if(!is.numeric(siteid)) stop('siteid must be numeric.')
  }
  
  #  Parameter check on altitudes.  This gets reused, we could turn it into a
  #  higher level function to save reading lame code:
  if(all(c('altmin', 'altmax') %in% names(cl))){
    #  If the user defines a minimum and maximum altitude, make sure that the
    #  minimum is smaller than the max.
    if(cl$altmin > cl$altmax){
      altmin <- min(c(cl$altmin, cl$altmax))
      altmax <- max(c(cl$altmin, cl$altmax))
      warning('altmin must be smaller than atmax, values were switched in call.')
    }
  }
  
  # Parameter check on 'loc', ought to be a comma separated list of 
  # lonW, latS, lonE, latN when it is passed out, but it's probably
  # better to pass in a vector.  Although it might be better to associate
  # it with a spatial object existing in R like an extent or bbox.
  if('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)
    
    if(class(cl$loc) == 'numeric' & length(cl$loc == 4)){
      
      #  The latitudes must be from -90 to 90
      #  The longitudes must be from -180 to 180
      if(all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
           all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
        cl$loc <- paste(cl$loc, collapse = ',')
      }
      else{
        stop('loc must be in the form c(lonW, latS, lonE, latN).\nLongitudes from -180 to 180, latitudes from -90 to 90.')
      }
    }
    else{
      stop('The loc must be a numeric vector: lonW, latS, lonE, latN.\n')
    }
  }
  
  #  Parameter check on 'gpid', the name needs to be in the big table in
  #  data object geopol:
  if('gpid' %in% names(cl)){
    #if(!cl$gpid %in% geopol[,5]){
    #  stop('Unrecognized geopolitical entity.  Check for acceptible names in data(geopol).')
    #}
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    if('siteid' %in% names(cl) & length(names(cl)) == 1){
      
    }
    else{
       names(aa) <- sapply(aa, function(x)x$SiteName)
       output <- suppressMessages( do.call('rbind', lapply(aa, data.frame, stringsAsFactors = FALSE)))
    }
  }
  output
}

get.download <- function(datasetid){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)
  
  #This needs work.
  base.uri <- 'http://api.neotomadb.org/v1/data/downloads'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on siteid:
  if('datasetid' %in% names(cl)){
    if(!is.numeric(datasetid)) stop('datasetid must be numeric.')
  }
  
  aa <- try(fromJSON(paste(base.uri, '/', datasetid, sep=''), nullValue = NA))
  #getForm(base.uri, .params = cl)))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') aa <- neotoma.form
  
  aa
  
}

get.contacts <- function(contactid, contactname, contactstatus, familyname){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)
  
  base.uri <- 'http://api.neotomadb.org/v1/data/contacts'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on contactid:
  if('contactid' %in% names(cl)){
    if(!is.numeric(cl$contactid)){
      stop('The contactid must be numeric.')
    }
  }
  
  #  Parameter check on contactname:
  if('contactname' %in% names(cl)){
    if(!is.character(cl$contactname)){
      stop('The contactname must be a character string.')
    }
  }
  
  #  Parameter check on contactstatus:
  if('contactstatus' %in% names(cl)){
    if(!is.character(cl$contactname)){
      stop('The contactstatus must be a character string.')
    }
    else{
      if(!cl$status %in% c('active', 'deceased', 'defunct',
                           'extant', 'inactive', 'retired', 'unknown')){
        stop('status must be an accepted term.  Use get.table(\'ContactStatues\')')
      }
    }
  }
  
  #  Parameter check on familyname:
  if('familyname' %in% names(cl)){
    if(!is.character(cl$familyname)){
      stop('The familyname must be a character string.')
    }
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    names(aa) <- sapply(aa, function(x)x$ContactID)
    output <- suppressMessages(cast(melt(lapply(aa, data.frame)))[,-2])
  }
  
  output  
}

get.datasets <- function(siteid, datasettype, piid, altmin, altmax, loc, gpid, taxonids, taxonname, ageold, ageyoung, ageof, subdate){
#  The issue here is that these objects have multiple tables of multiple lengths.
  
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)
  
  base.uri <- 'http://api.neotomadb.org/v1/data/datasets'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  if('piid' %in% names(cl)){
    # piid must be the numeric PI id number in the Neotoma database.
    if(!is.numeric(cl$piid)) stop('piid must be a numeric value.')
  }
  
  #  Parameter check for the datasettype, make sure it's one of the
  #  accepted types:
  if('datasettype' %in% names(cl)){
    settypes <- c('geochronologic', 'loss-on-ignition', 'pollen', 
                  'plant macrofossils', 'vertebrate fauna', 'mollusks',
                  'pollen surface sample')
    
    set <- pmatch(cl$datasettype, settypes, nomatch=NA)
    if(is.na(set)) stop('datasettype must be one of: geochronologic, loss-on-ignition, pollen,\nplant macrofossils, vertebrate fauna, mollusks, pollen surface sample')
  }
  
  if('ageof' %in% names(cl)){
    if(!cl$ageof %in% c('sample', 'taxon', 'dataset')){
      stop('ageof parameter must be one of: sample, taxon or dataset')
    }
    else{
      if(any(c('taxonid', 'taxonname') %in% names(cl)) & !cl$ageof == 'taxon'){
        stop('When taxonid or taxonname is invoked, ageof must be taxon')
      }
    }
  }
  
  #  Parameter check on altitudes:
  if(all(c('altmin', 'altmax') %in% names(cl))){
    #  If the user defines a minimum and maximum altitude, make sure that the
    #  minimum is smaller than the max.
    if(cl$altmin > cl$altmax){
      altmin <- min(c(cl$altmin, cl$altmax))
      altmax <- max(c(cl$altmin, cl$altmax))
      warning('altmin must be smaller than altmax, values were switched in call.')
    }
  }
  
  #  Parameter check on ages:
  if(all(c('ageold', 'ageyoung') %in% names(cl))){
    #  If the user defines a minimum and maximum age, make sure that the
    #  minimum is smaller than the max.
    if(cl$ageyoung > cl$ageold){
      cl$ageyoung <- min(c(eval(cl$ageold), eval(cl$ageyoung)))
      cl$ageold <- max(c(eval(cl$ageold), eval(cl$ageyoung)))
      cat(cl)
      warning('ageyoung must be smaller than ageold, values were switched in call.')
    }
    else{
      cl$ageold <- eval(cl$ageold)
      cl$ageyoung <- eval(cl$ageyoung)
    }
  }
  
  # Parameter check on 'loc', ought to be a comma separated list of 
  # lonW, latS, lonE, latN when it is passed out, but it's probably
  # better to pass in a vector.  Although it might be better to associate
  # it with a spatial object existing in R like an extent or bbox.
  if('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)
    
    if(class(cl$loc) == 'numeric' & length(cl$loc == 4)){
      
      #  The latitudes must be from -90 to 90
      #  The longitudes must be from -180 to 180
      if(all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
           all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
        cl$loc <- paste(cl$loc, collapse = ',')
      }
      else{
        stop('loc must be in the form c(lonW, latS, lonE, latN).\nLongitudes from -180 to 180, latitudes from -90 to 90.')
      }
    }
    else{
      stop('The loc must be a numeric vector: lonW, latS, lonE, latN.\n')
    }
  }
  
  #  Parameter check on 'gpid', the name needs to be in the big table in
  #  data object geopol:
  #if('gpid' %in% names(cl)){
  #  if(!cl$gpid %in% geopol[,5]){
  #    stop('Unrecognized geopolitical entity.  Check for acceptible names in data(geopol).')
  #  }
  #}
  
  neotoma.form <- getForm(base.uri, .params = cl, binary=FALSE,
                          .encoding='utf-8', )
  
  aa <- try(fromJSON(neotoma.form, nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    output <- aa[[2]]
    cat('The API call was successful, you have returned ', length(output), 'records.\n')
  }
  

  if(class(output) == 'try-error') output <- neotoma.form
  else{
    #  This is a bit frustrating, I can't quite figure it out.
    # The things that are multiple lengths are:
    # Dataset PIs &
    # SubDates
    #  I'd like to put this out in a nice table format
    
    output <- lapply(output, function(x) {x$Site <- data.frame(x$Site); x})
    output <- lapply(output, function(x) {x$SubDates <- ldply(x$SubDate, function(x) t(data.frame(x)))
                                  x})
    output <- lapply(output, function(x) {
  #      if(length(x$DatasetPIs) > 0){
          x$DatasetPIs <-ldply(x$DatasetPIs, data.frame)
          x
   #     }
      })
    
  }
  

  output
  
}

get.publication <- function(pubid, contactid, datasetid, author, pubtype, year, search){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)
  
  base.uri <- 'http://api.neotomadb.org/v1/data/publications'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on pubid:
  if('pubid' %in% names(cl)){
    if(!is.numeric(cl$pubid)){
      stop('The pubid must be numeric.')
    }
  }
  
  #  Parameter check on contactid:
  if('contactid' %in% names(cl)){
    if(!is.numeric(cl$contactid)){
      stop('The contactid must be numeric.')
    }
  }
  
  #  Parameter check on datasetid:
  if('datasetid' %in% names(cl)){
    if(!is.numeric(cl$datasetid)){
      stop('The datasetid must be numeric.')
    }
  }
  
  #  Parameter check on author:
  if('author' %in% names(cl)){
    if(!is.character(cl$author)){
      stop('The author must be a character string.')
    }
  }
  
  if('pubtype' %in% names(cl)){
    if(!is.character(cl$pubtype)){
      stop('The pubtype must be a character string.  Use get.table(\'PublicationTypes\') to find acceptable tables.')
    }
  }
  
  if('year' %in% names(cl)){
    if(!is.numeric(cl$year)){
      stop('The year used must be numeric.')
    }
  }
  
  #  Parameter check on author:
  if('search' %in% names(cl)){
    if(!is.character(cl$search)){
      stop('The search string must be a character string.')
    }
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue = NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    aa <- aa[[2]]
    cat('The API call was successful, you have returned ', length(aa), 'records.\n')
  }
  
  if(class(aa) == 'try-error') output <- neotoma.form
  else{
    names(aa) <- sapply(aa, function(x)x$SiteName)
    output <- suppressMessages(cast(melt(lapply(aa, data.frame)))[,-2])
  }
  
  output
}

get.table <- function(table.name = NULL){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)

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

get.taxa <- function(taxonid, taxonname, status, taxagroup, ecolgroup){
  require(RJSONIO)
  require(RCurl)
  require(reshape)
  require(plyr)

  base.uri <- 'http://api.neotomadb.org/v1/data/taxa'
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir=parent.frame())
  
  #  Parameter check on taxagroup:
  if('taxagroup' %in% names(cl)){
    taxon.codes <- c('AVE', 'BIM', 'BRY',
                     'BTL', 'FSH', 'HRP',
                     'LAB', 'MAM', 'MOL',
                     'PHY', 'TES', 'VPL')
    
    if(!cl$taxagroup %in% taxon.codes){
      stop('taxonGroup is not an accepted code.  Use get.table(\'TaxaGroupTypes\') to obtain acceptible classes')
    }
  }
  
  #  Parameter check on taxonname and taxonids, I'm allowing only one, but I think it can accept two.
  if(any(c('taxonids', 'taxonname') %in% names(cl))){
    
    if(all(c('taxonids', 'taxonname') %in% names(cl))){
      stop('Can only accept either taxonids OR taxonname, not both.')
    }
    if('taxonids' %in% names(cl) & !is.numeric(cl$taxonids)) {
      stop('The variable taxonids must be numeric.  To obtain a list of taxon IDs use the get.table command.')
    }
    if('taxonname' %in% names(cl) & !is.character(cl$taxonname)) {
      stop('The variable taxonname must be a character string.  To obtain a list of taxon names use the get.table command.')
    }
  }
  
  if('status' %in% names(cl)){
    if(!cl$status %in% c('extinct', 'extant', 'all')){
      stop('Status must be one of: \'extinct\', \'extant\', or \'all\'')
    }
  }
  
  aa <- try(fromJSON(getForm(base.uri, .params = cl), nullValue=NA))
  
  if(aa[[1]] == 0){
    stop(paste('Server returned an error message:\n', aa[[2]]), call.=FALSE)
  }
  if(aa[[1]] == 1){
    output <- aa[[2]]
    cat('The API call was successful, you have returned ', length(output), 'records.\n')
  }
  
  if(class(aa) == 'try-error'){ output <- neotoma.form }
  else{
    
    names(output) <- sapply(output, function(x)x$TaxonName)
    #  There are some values in here that are empy lists:
    output <- lapply(output, function(x){
                if(any(sapply(x, length) == 0)){
                  x[[which(sapply(x, length) == 0)]] <- NA
                }
                x
                })
    
    output <- ldply(output, function(x) data.frame(x))  
  }
    
  output
  
}

