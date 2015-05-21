#' Function to return chronological control tables used to build age models.
#'
#' Using the dataset ID, return all records associated with the data.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.   This function will only download one dataset at a time.
#'
#' @importFrom RJSONIO fromJSON
#' @param chronologyid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{\link{get_dataset}}.
#' @param verbose logical, should messages on API call be printed?
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either an object of class  \code{"try-error"} containing the error returned
#'    from the Neotoma API call, or a full data object containing all the relevant information required to build either the default or prior chronology for a core. 
#'    
#'    This is a list comprising the following items:
#'
#'  \item{ \code{chron.control} }{A table describing the collection, including dataset information, PI data compatable with \code{\link{get_contact}} and site data compatable with \code{\link{get_site}}.}
#'  \item{ \code{meta} }{Dataset information for the core, primarily the age-depth model and chronology.  In cases where multiple age models exist for a single record the most recent chronology is provided here.}
#'  
#'  If Neotoma returns empty content, either the control table or the associated metadata (which happens in approximately 25% of cases) then the data.frames are returned with NA content.
#'  
#' @examples \dontrun{
#' #  The point of pulling chronology tables is to re-build or examine the chronological
#' #  information that was used to build the age-depth model for the core.
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords IO connection

#' @export
get_chroncontrol <- function(chronologyid, verbose = TRUE){

  # Updated the processing here. There is no need to be fiddling with
  # call. Use missing() to check for presence of argument
  # and then process as per usual
  base.uri <- 'http://api.neotomadb.org/v1/data/chronologies'

  if (missing(chronologyid)) {
      stop(paste(sQuote("chronologyid"), "must be provided."))
  } else {
      if (!is.numeric(chronologyid))
          stop('chronologyid must be numeric.')
  }

  # query Neotoma for data set
  aa <- try(fromJSON(paste0(base.uri, '/', chronologyid), nullValue = NA))

  # Might as well check here for error and bail
  if (inherits(aa, "try-error"))
      return(aa)

  # if no error continue processing
  if (isTRUE(all.equal(aa[[1]], 0))) {
      stop(paste('Server returned an error message:\n', aa[[2]]),
           call. = FALSE)
  }

  if (isTRUE(all.equal(aa[[1]], 1))) {
        aa <- aa[[2]]

        if (verbose) {
            writeLines(strwrap(paste0("API call was successful.",
                                      " Returned chronology.")))
        }

        # Here the goal is to reduce this list of lists to as
        # simple a set of matrices as possible.
        #  Some of the records do not contain a 'controls' table, so they only contain
        #  'meta':
        
        if(length(aa) == 0){
          # This removes the need to make a more deeply nested if statement. . . 
          aa <- list(empty = 0) 
        }
        
        if('controls' %in% names(aa[[1]])){
          control.table <- do.call(rbind.data.frame, lapply(aa, '[[', 'controls')[[1]])
          
          control.table <- control.table[, c('Depth', 'Thickness',
                                            'Age', 'AgeYoungest', 'AgeOldest',
                                            'ControlType', 'ChronControlID')]
          
          colnames(control.table) <- c('depth', 'thickness', 'age', 
                                       'age.young', 'age.old', 'control.type',
                                       'chron.control.id')
        } else {
          #  If there is no chron-control table, return an empty control.table element.
          control.table <- data.frame(depth = NA,
                                      thickness = NA,
                                      age = NA, 
                                      age.young = NA,
                                      age.old = NA, 
                                      control.type = NA,
                                      chron.control.id = NA)
        }
        
        if('Default' %in% names(aa[[1]])){
          meta.table <- data.frame(default     = aa[[1]]$Default,
                                   name        = aa[[1]]$ChronologyName,
                                   age.type    = aa[[1]]$AgeType,
                                   age.model   = aa[[1]]$AgeModel,
                                   age.older   = aa[[1]]$AgeOlder,
                                   age.younger = aa[[1]]$AgeYounger,
                                   chron.id    = aa[[1]]$ChronologyID,
                                   date        = aa[[1]]$DatePrepared)
        } else {
          # Return an empty data.frame.
          meta.table <- data.frame(default     = NA,
                                   name        = NA,
                                   age.type    = NA,
                                   age.model   = NA,
                                   age.older   = NA,
                                   age.younger = NA,
                                   chron.id    = NA,
                                   date        = NA)
        }
    }

  list(chron.control = control.table,
       meta = meta.table)

}
