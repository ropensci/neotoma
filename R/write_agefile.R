#' Write age control file to disk formatted for either Bacon or Clam
#'
#' Passing in a download object the function outputs a Bacon or Clam formatted file to a
#' user defined destination for age modelling with existing age-depth modeling software.
#'
#' @param download A single site returned by \code{get_download}.
#' @param chronology Default is \code{1}, the default chronology for the core.  If a core has more than one chronology the user can define a different set of chronological controls.
#' @param path The location of the 'Cores' folder & working directory for Bacon.  Do not include "Cores" in the path name.
#' @param corename The intended handle for the core, to be used in writing to file.
#' @param cal.prog The method intended to build the age model, either \code{'Bacon'} or \code{'Clam'}.
#' 
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' 
#' @return This command returns a file in location \code{path/Cores} containing all the relevant information required to build either the default or prior chronology for a core.
#' 
#' @examples \dontrun{
#' #  The point of pulling chronology tables is to re-build or examine the chronological 
#' #  information that was used to build the age-depth model for the core.
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
write_agefile <- function(download, chronology = 1, path, 
                          corename, cal.prog = 'Bacon'){

  if (!file.exists(paste0(path, '/Cores'))){
    stop(paste0('Core directory must exist.  ',
                'There is no directory at ', path, '/Cores'))
  }

  if (!class(download) == 'list' | !c('chronologies') %in% names(download)){
    stop(paste0('write_agefile can only operate on valid download ',
                'objects with valid chronologies'))
  }
  
  if (class(download) == 'list' & c('chronologies') %in% names(download)){
    
    
    
    chron.controls <- get_chroncontrol(chronologyid = download$chronologies[[chronology]]$ChronologyID[1],
                                       verbose = FALSE)
  
    if (nrow(chron.controls$chron.control) < 2){
      stop('Chronology must have more than a single date for proper analysis.')
    }
    
    uncal <- c('Radiocarbon', 'Radiocarbon, reservoir correction', 
               'Radiocarbon, average of two or more dates')
    
    if (!tolower(cal.prog) %in% c('bacon', 'clam')){
      stop('You must define either Bacon or Clam as your model output.')
    }
    if (cal.prog == 'Bacon'){
      chron <- data.frame(labid = paste0(chron.controls$chron.control$ControlType, 
                                         "_",
                                         chron.controls$chron.control$ChronControlID),
                          age = chron.controls$chron.control$Age,
                          error = abs(chron.controls$chron.control$Age - 
                                        chron.controls$chron.control$AgeYoungest),
                          depth = chron.controls$chron.control$Depth,
                          cc = ifelse(chron.controls$chron.control$ControlType %in% uncal,
                                      1, 0))
    }
    if (cal.prog == 'Clam'){
      chron <- data.frame(ID = paste0(chron.controls$chron.control$ControlType, 
                                      "_",
                                     chron.controls$chron.control$ChronControlID),
                          C14_age = chron.controls$chron.control$Age,
                          cal_BP = chron.controls$chron.control$Age,
                          error = abs(chron.controls$chron.control$Age - 
                                        chron.controls$chron.control$AgeYoungest),
                          reservoir = NA,
                          depth = chron.controls$chron.control$Depth)
      chron$cal_BP [ chron.controls$chron.control$ControlType %in% uncal] <- NA
      chron$C14_age[!chron.controls$chron.control$ControlType %in% uncal] <- NA
      
    }
      
    depths <- download$sample.meta$depths
    
    if (!corename %in% list.files(paste0(path, '/Cores'))){
      works <- dir.create(path = paste0(path, '/Cores/', corename))
      if (!works) {
        stop(paste0('Could not create the directory.  ',
                    'Check the path, corename and your permissions.'))
      }
    }
    
    write.csv(chron, paste0(path, '/Cores/', corename, '/', corename, '.csv'),
              row.names = FALSE)
    write.csv(chron, paste0(path, '/Cores/', corename, '/', corename, '_depths.txt'),
              row.names = FALSE)
  }
}
