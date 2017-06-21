#' Function to read in defined Bacon outputs.
#' 
#' Reads in Bacon output and formats it for inclusion in a download object.
#' @param x A folder path that contains a Bacon \code{age} file.
#' @param path The location of the \code{Cores} folder.
#' @param add Should the results be added to an existing \code{download}? Defaults to \code{FALSE}.
#' @param download The target \code{download} if \code{add} is \code{TRUE}.
#' @param chron_name The name for the chronology if the Bacon file is being added to a \code{download}.
#' @param as_default Should the chronology become the default?
#' @param sections If there are multiple Bacon runs in a folder, identify the file by the number of sections in the run.
#' @param age_field Should the age be assigned to the \code{"median"} or the \code{"wmean"}?
#' @param interp If the depths don't match up, should we interpolate from the Bacon output? (default \code{TRUE})
#' 
#' @details The function expects that you are in a working directory containing a "Cores" which would then contain output files from Bacon runs.  The output can either be added to an existing record (for example, replacing the default age model returned by Neotoma), or it can be loaded on its own.
#' If the depths for the loaded file do not match with the depths in the `download`'s `sample.meta` then the user can use the `interp` parameter to interpolate between depths.  This method uses linear interpolation.
#' 
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
#' # These defaults just help the core run quickly, they're not 
#' # neccesarily good parameters.
#' 
#' Bacon("LAKEPINES", acc.mean = 10, 
#'       thick = 50, depths.file = TRUE, 
#'       suggest = FALSE, ask = FALSE)
#' 
#' lake_o_dl <- read_bacon("LAKEPINES", add = TRUE, 
#'                         download = download, sections = 17)
#' 
#' }
#' @export
read_bacon <- function(x, path = '.', add = FALSE, chron_name = "Bacon", as_default = TRUE, download = NULL, sections = NULL, age_field = "median", interp = TRUE) {
  if (add == TRUE & (is.null(download) | !"download" %in% class(download))) {
    stop("You can't add if you don't include a download object to add to.")
  }
  
  if (is.null(sections)) {
    out_files <- list.files(paste0(path, '/Cores/', x, '/'), pattern = "ages.txt")
    if (length(out_files) > 1) {
      stop("There are multiple output files in the Core directory.  Please use the `sections` parameter in `read_bacon`.")
    } else {
      sections <- gsub("[^1-9]", "", out_files)
    }
  }
  
  ages <- utils::read.table(paste0(path, '/Cores/', x, '/', x, '_', sections, '_ages.txt'),
                          header = TRUE)
  
  matched_depths <- match(download$sample.meta$depth, ages$depth)

  # With a hiatus we get variable row lengths, so we need to use `readLines`:
  file_in <- file(paste0(path, '/Cores/', x, '/', x, "_settings.txt"))
  settings <- readLines(file_in)
  close(file_in)
    
  if (all(is.na(matched_depths)) & !(interp == TRUE)) {
    stop("The Bacon ages file and the core output do not have matching depths.\nPerhaps try using `depths.file = TRUE` flag when running Bacon, or set `interp=TRUE` flag.")
  }
  if (any(is.na(matched_depths)) & (interp == TRUE)) {
    warning(paste0(sum(is.na(matched_depths)), " depths need to be interpolated.  Using linear interpolation."))
    ages_old <- ages
    
    ages <- data.frame(depth  = download$sample.meta$depth,
                       min    = stats::approx(ages$depth, ages$min, xout = download$sample.meta$depth)$y,
                       max    = stats::approx(ages$depth, ages$max, xout = download$sample.meta$depth)$y,
                       median = stats::approx(ages$depth, ages$median, xout = download$sample.meta$depth)$y,
                       wmean  = stats::approx(ages$depth, ages$wmean, xout = download$sample.meta$depth)$y)
  } else {
    
  }
  
  age_scale <- ifelse(settings[grep("#cc$", settings)] == "1 #cc",
                      "Calibrated radiocarbon years BP", "BC/AD")

  matched_depths <- match(download$sample.meta$depth, ages$depth)
  
  chronology <- data.frame(age.older = ages$max[matched_depths],
                           age = ages[matched_depths,age_field],
                           age.younger = ages$min[matched_depths],
                           chronology.name = chron_name,
                           age.type = age_scale,
                           chronology.id = paste0(path, '/Cores/', x, '/', x, '_', sections),
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
