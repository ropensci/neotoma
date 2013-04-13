#' Return Site Information.
#'
#' \code{get.sites} returns site information from the Neotoma Paleoecological Database
#'    based on parameters defined by the user.
#'
#' @import RJSONIO RCurl plyr
#' @param siteid The numerical site ID.
#' @param sitename A character string representing the full or partial site name.
#' @param altmin Minimum site altitude  (in m).
#' @param altmax Maximum site altitude (in m).
#' @param loc A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator.
#' @param gpid A character string, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: http://api.neotomadb.org/apdx/geopol.htm
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A table:
#'
#' \itemize{
#'  \item{SiteID}{Unique database record identifier for the site.}
#'  \item{SiteName}{Name of the site.}
#'  \item{Altitude}{Altitude in meters.}
#'  \item{LatitudeNorth}{North bounding latitude, in decimal degrees, for a site.}
#'  \item{LatitudeSouth}{South bounding latitude, in decimal degrees, for a site.}
#'  \item{LongitudeEast}{East bounding longitude, in decimal degrees, for a site.}
#'  \item{LongitudeWest}{West bounding longitude, in decimal degrees, for a site.}
#'  \item{SiteDescription}{Free form description of a site, including such information as physiography and vegetation around the site.}
#' }
#'
#' Extended response variables when only a single site is returned:
#' \itemize{
#'  \item{CollectionUnitID}{Unique database record identifier for the collection unit.}
#'  \item{Handle}{Code name for the collection unit. This code may be up to 10 characters, but an effort is made to keep these to 8 characters or less. Data are frequently distributed by collection unit, and the handle is used for file names.}
#'  \item{CollType}{The collection type. Types include cores, sections, excavations, and animal middens. Collection Units may be modern collections, surface float, or isolated specimens. Composite Collections Units include different kinds of Analysis Units, for example a modern surface sample for ostracodes and an associated water sample.}
#'  \item{Datasets}{An array of objects that describe datasets associated with a site.}
#' }
#' @examples \dontrun{
#' #  What is the distribution of site elevations in Neotoma?
#' all.sites <- get.sites()  #takes a bit of time.
#'
#' plot(density(all.sites$Altitude, from = 0, na.rm=TRUE),
#' main = 'Altitudinal Distribution of Neotoma Sites', xlab = 'Altitude (m)', log='x')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get.sites <- function(siteid, sitename, altmin, altmax, loc, gpid){

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

#' Function to return full dataset records.
#'
#' Using the dataset ID, return all records associated with the data.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.
#'
#' @import RJSONIO RCurl plyr
#' @param datasetid Dataset ID, as returned by \code{get.datasets}.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either a 'try-error' definined by the error returned
#'    from the Neotoma API call, or a list comprising the following values:
#'
#' \itemize{
#'  \item{DatasetID}{Unique database record identifier for the dataset.}
#'  \item{DatasetName}{Name of the dataset; not commonly used.}
#'  \item{CollUnitHandle}{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
#'  \item{CollUnitType}{The collection type. Types include cores, sections, excavations, and animal middens.}
#'  \item{DatasetType}{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
#'  \item{NeotomaLastSub}{The date of the most recent submission.}
#'  \item{DefChronologyID}{The unique database identifier for the default chronology.}
#'  \item{DatasetPIs}{An array of objects that describe Principal Investigators associated with a dataset}
#'  \item{Site}{  An object describing the site where the dataset samples were taken}
#'  \item{Samples}{	An array of objects describing the individual dataset samples}
#' }
#'
#'    Here the \code{Samples} is the real prize, but at present the format of
#'    the data object is simply a list of lists.  This needs work.
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get.datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
#' GOLDKBG <- get.download(t8kyr.datasets[[1]]$DatasetID)
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get.download <- function(datasetid){

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

#' A function to obtain contact information for data contributors from the Neotoma
#'    Paleoecological Database.
#'
#' @import RJSONIO RCurl plyr
#' @param contactid Contact ID is a numerical value associated with the Neotoma
#'    Contact table's numerical Contact ID.
#' @param contactname A character string indicating the data contributors' project,
#'    organization or personal name.  May be a partial string and can include wildcards.
#' @param contactstatus The current status of the contact.  Possible values include:
#'    active, deceased, defunct, extant, inactive, retired, unknown.
#' @param familyname A character string.  Full or partial string indicating the
#'    contact's last name.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return The function takes parameters defined by the user and returns a list
#'    of contact information supplied by the Neotoma Paleoecological Database.
#'    The user may define all or none of the possible fields.  The function contains
#'    data chacks for each defined parameter.
#'
#'    The function returns either a single item of class "try-error" describing
#'    the reason for failure (either mis-defined parameters or an error from the Neotoma API),
#'    or a table of contacts, with rows corresponding to the number of individual
#'    contacts returned by the Neotoma API.  Each row entry includes the following parameters:
#'
#' \itemize{
#'  \item{ContactID}{  Unique database record identifier for the contact.}
#'  \item{AliasID}{  The ContactID of a person's current name. If the AliasID is different from the ContactID, the ContactID refers to the person's former name.}
#'  \item{ContactName}{  Full name of the person, last name first (e.g. "Simpson, George Gaylord") or name of organization or project (e.g. "Great Plains Flora Association").}
#'  \item{ContactStatus}{  Current status of the person, organization, or project. Field links to the ContactStatuses lookup table.}
#'  \item{FamilyName}{  Family or surname name of a person.}
#'  \item{LeadingInitials}{  Leading initials for given or forenames without spaces (e.g. "G.G.").}
#'  \item{GivenNames}{  Given or forenames of a person (e.g. "George Gaylord"). Initials with spaces are used if full given names are not known (e.g. "G. G").}
#'  \item{Suffix}{  Suffix of a person's name (e.g. \"Jr.\", "III").}
#'  \item{Title}{  A person’s title (e.g. "Dr.", "Prof.", "Prof. Dr").}
#'  \item{Phone}{  Telephone number.}
#'  \item{Fax}{  Fax number.}
#'  \item{Email}{  Email address.}
#'  \item{URL}{  Universal Resource Locator, an Internet World Wide Web address.}
#'  \item{Address}{  Full mailing address.}
#'  \item{Notes}{  Free form notes or comments about the person, organization, or project.}
#' }
#' @examples \dontrun{
#' #  To find all data contributors who are active:
#' active.cont <- get.contacts(contactstatus = 'active')
#'
#' # To find all data contributors who have the last name "Smith"
#' smith.cont <- get.contacts(familyname = 'Smith')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get.contacts <- function(contactid, contactname, contactstatus, familyname){

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
    if(!is.character(cl$contactstatus)){
      stop('The contactstatus must be a character string.')
    }
    else{
      if(!cl$contactstatus %in% c('active', 'deceased', 'defunct',
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

#' Obtain full datasets from the Neotoma Paleoecological Database.
#'
#' A function to access the Neotoma API and return datasets corresponding to the
#'    parameters defined by the user.
#'
#' @import RJSONIO RCurl plyr
#' @param siteid A numeric value corresponding to the site ID.
#' @param datasettype A character string corresponding to one of the allowed dataset types in the Neotoma Database.  Allowed types include: 'geochronologic', 'loss-on-ignition', 'pollen', 'plant macrofossils', 'vertebrate fauna', 'mollusks', and 'pollen surface sample'.
#' @param piid Numeric value for the Principle Investigator's ID number.
#' @param altmin Numeric value indicating the minimum altitude for the site (can be used alone or with altmax).
#' @param altmax Numeric value indicating the maximum altitude for the site (can be used alone or with altmin).
#' @param loc A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator
#' @param gpid A character string, must correspond to a valid geopolitical identity in the Neotoma Database.  Use \code{get.tables('GeoPoliticalUnits')} for a list of acceptable values, or link here: \url{http://api.neotomadb.org/apdx/geopol.htm}
#' @param taxonids A numeric identifier for the taxon.  Use \code{get.tables('Taxa')} for a list of acceptable values.
#' @param taxonname A character string corresponding to a valid taxon identity in the Neotoma Database.  Use \code{get.tables('Taxa')} for a list of acceptable values.
#' @param ageold The oldest date acceptable for the search (in years before present).
#' @param ageyoung The youngest date acceptable for the search.
#' @param ageof If a taxon ID or taxon name is defined this parameter must be set to "taxon", otherwise it may refer to "sample", in which case the age bounds are for any samples within datasets or "dataset" if you want only datasets that are within the bounds of ageold and ageyoung.
#' @param subdate Date of dataset submission, either YYYY-MM-DD or MM-DD-YYYY.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return More details on the use of these parameters can be obtained from
#'    \url{http://api.neotomadb.org/doc/resources/datasets.}
#'
#'    A list, with each item corresponding to an individual record.  Each list item
#'    (each dataset record) includes the following components:
#'
#' \itemize{
#'  \item{DatasetID}{Unique database record identifier for the dataset.}
#'  \item{DatasetName}{Name of the dataset; not commonly used.}
#'  \item{CollUnitHandle}{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
#'  \item{CollUnitID}{Unique database record identifier for the collection unit.}
#'  \item{CollType}{The collection type. Types include cores, sections, excavations, and animal middens.}
#'  \item{DatasetType}{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
#'  \item{AgeOldest}{The oldest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{AgeYoungest}{The youngest of all sample ages (in calendar years before present) in the dataset.}
#'  \item{SubDates}{An array of objects that describe dataset submission events.  If multiple submissions occured then this is a table.}
#'  \item{DatasetPIs}{An array of objects that describe Principal Investigators associated with a dataset.}
#'  \item{Site}{An object describing the site where the dataset samples were taken.}
#' }
#' @examples \dontrun{
#' # Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' # that are on the west coast of North America:
#' t8kyr.datasets <- get.datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' # Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.
#' gpids <- get.table(table.name='GeoPoliticalUnits')
#' canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]
#'
#' v2kyr.datasets <- get.datasets(datasettype='vertebrate fauna', gpid=canID, ageold = 2000)
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get.datasets <- function(siteid, datasettype, piid, altmin, altmax, loc, gpid, taxonids, taxonname, ageold, ageyoung, ageof, subdate){
  #  The issue here is that these objects have multiple tables of multiple lengths.

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

#' A file to get publications for sites or datasets in the Neotoma Database using the API.
#'
#' The function takes the parameters, defined by the user, and returns a table with
#'    publication information from the Neotoma Paleoecological Database.
#'
#' @import RJSONIO RCurl plyr
#' @param pubid Numeric Publication ID value, either from \code{get.datasets} or known.
#' @param contactid Numeric Contact ID value, either from \code{get.datasets} or \code{get.contacts}
#' @param datasetid Numeric Dataset ID, known or from \code{get.datasets}
#' @param author Character string for full or partial author's name.  Can include wildcards such as 'Smit*' for all names beginning with 'Smit'.
#' @param pubtype Character string, one of eleven allowable types, see \code{get.table('PublicationTypes')}
#' @param year Numeric publication year.
#' @param search A character string to search for within the article citation.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A table is returned with several fixed columns, and a variable number
#'    of author fields:
#'
#' \itemize{
#'  \item{PublicationID}{Unique database record identifier for the publication.}
#'  \item{PubType}{Publication type}
#'  \item{Year}{Year of publication.}
#'  \item{Citation}{The complete citation in a standard style. For legacy citations inherited from other databases, this field holds the citation as ingested from the other databases.}
#'  \item{Authors}{Array of author objects, can be of variable length.  Includes \code{Authors.ContactName.n}, \code{Authors.ContactID.n}, \code{Authors.Order.n}, where n ranges from 1 to the maximum number of authors returned by the API call.  When the maximum number of authors is 1 the number is excluded.}
#' }
#' @examples \dontrun{
#' #  To find all data contributors who are active:
#' active.cont <- get.contacts(contactstatus = 'active')
#'
#' # To find all data contributors who have the last name "Smith"
#' smith.cont <- get.contacts(familyname = 'Smith')
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get.publication <- function(pubid, contactid, datasetid, author, pubtype, year, search){

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
#' taxon.table <- get.table('Taxa')
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
get.table <- function(table.name = NULL){

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

#' Get taxon information from Neotoma.
#'
#' @import RJSONIO RCurl plyr
#' @param taxonid Numeric taxon identifier used in Neotoma
#' @param taxonname A character string representing the full or partial name of taxa of interest.
#' @param status The current status of the taxon, one of 'extinct', 'extant', 'all'.
#' @param taxagroup The taxonomic grouping for the taxa. See \link{http://api.neotomadb.org/doc/resources/taxa} for the list of approved groupings.
#' @param ecolgroup The ecological group of the taxa. More detailed than \code{taxagroup}, can be obtained using \code{get.table("EcolGroupTypes")}.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return Returns a table.
#'
#' \itemize{
#'  \item{TaxonID}{Unique database record identifier for a taxon.}
#'  \item{TaxonCode}{Shorthand notation for a taxon identification.}
#'  \item{TaxonName}{Name of the taxon.}
#'  \item{Author}{Author(s) of the name. Used almost exclusively with beetle taxa.}
#'  \item{Extinct}{True if extinct; false if extant.}
#'  \item{TaxaGroup}{Code for taxa group to which taxon belongs.}
#'  \item{EcolGroups}{Array of ecological group codes to which the taxon belongs.}
#'  \item{HigherTaxonID}{TaxonID of the next higher taxonomic rank.}
#'  \item{PublicationID}{Publication identification number.}
#'  \item{Notes}{Free-form notes or comments about the taxon.}
#' }
#'
#' @examples \dontrun{
#' taxon.table <- get.table('Taxa')
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
get.taxa <- function(taxonid, taxonname, status, taxagroup, ecolgroup){

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
