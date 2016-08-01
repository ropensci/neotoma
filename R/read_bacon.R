#' Function to read in defined Bacon outputs.
#' 
#' Reads in Bacon output and formats it for inclusion in a download object.
#' @param x A folder path that contains a Bacon \code{age} file.
#' @param add Should the results be added to an existing \code{download}? Defaults to \code{FALSE}.
#' @param download The target \code{download} if \code{add} is \code{TRUE}.
#' @param chron_name The name for the chronology if the Bacon file is being added to a \code{download}.
#' @param as_default Should the chronology become the default?
#' @param section If there are multiple Bacon runs in a folder, identify the file by the number of sections in the run.
#' @param age_field Should the age be assigned to the \code{"median"} or the \code{"wmean"}?
#' 
#' @details The function expects that you are in a working directory containing a "Cores" which would then contain output files from Bacon runs.  The output can either be added to an existing record (for example, replacing the default age model returned by Neotoma), or it can be loaded on its own.
#' @examples
#' \dontrun{
#' # Download the record for Lake O' Pines:
#' lake_o_dl <- get_download(15925)
#' 
#' # This assumes that you have Bacon installed in a folder and have
#' # set it to your working directory.
#' 
#' write_agefile(lake_o_dl[[1]], path = ".", chronology = 1, 
#'               corename = "LAKEPINES", cal.prog = 'Bacon') 
#'
#' source("Bacon.R") 
#' 
#' # These defaults just help the core run quickly, they're not neccesarily good parameters.
#' Bacon("LAKEPINES", acc.mean = 10, thick = 50, depths.file = TRUE, suggest = FALSE, ask = FALSE)
#' 
#' lake_o_dl <- read_bacon("LAKEPINES", add = TRUE, download = download, sections = 17)
#' 
#' }
#' @export
read_bacon <- function(x, add = FALSE, chron_name = "Bacon", as_default = TRUE, download = NULL, sections = NULL, age_field = "median") {
  if (add == TRUE & (is.null(download) | !"download" %in% class(download))) {
    stop("You can't add if you don't include a download object to add to.")
  }
  
  if (is.null(sections)) {
    out_files <- list.files(paste0('Cores/', x, '/'), pattern = "ages.txt")
    if (length(out_files) > 1) {
      stop("There are multiple output files in the Core directory.  Please use the `sections` parameter in `read_bacon`.")
    } else {
      sect <- gsub("[^1-9]", "", out_files)
    }
  }
  
  ages <- read.table(paste0('Cores/', x, '/', x, '_', sect, '_ages.txt'),
                          header = TRUE)
  
  settings <- read.table(paste0('Cores/', x, '/', x, "_settings.txt"),
                         comment.char = "")
  
  age_scale <- ifelse(settings[which(settings[,2] == "#cc"), 1] == 1,
                      "Calibrated radiocarbon years BP", "BC/AD")
  
  chronology <- data.frame(age.older = ages$max,
                           age = ages[,age_field],
                           age.younger = ages$min,
                           chronology.name = chron_name,
                           age.type = age_scale,
                           chronology.id = paste0('Cores/', x, '/', x, '_', sect),
                           dataset.id = NA)
  
  if (add == TRUE) {
    if (chron_name %in% names(download$chronologies)) {
      stop("Chronology name supplied matches an existing chronology name.")
    }
    
    if (!nrow(chronology) == nrow(download$sample.meta))  {
      stop("The new chronology and the existing data have a different number of rows.")
    }
    
    chronology$dataset.id <- download$dataset$dataset.meta$dataset.id

    download$chronologies[[length(download$chronologies) + 1]] <- chronology
    
    names(download$chronologies)[length(download$chronologies)] <- chron_name
    
    if (as_default == TRUE) {
      sample_cols <- colnames(download$sample.meta)[colnames(download$sample.meta) %in% colnames(chronology)]
      download$sample.meta[,sample_cols] <- chronology[,sample_cols]
    }
    return(download)
  }
  
  return(chronology)
  
}