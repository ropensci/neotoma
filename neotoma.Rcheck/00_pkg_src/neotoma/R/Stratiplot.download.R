#' @title Palaeoecological stratigraphic diagrams
#' @description Draws paleoecological diagrams from a \code{download} object.  Allows control of variable type (using the \code{tran} function from the \code{analogue} package), and taxonomic grouping.
#' @param x A \code{download} object.
#' @param yaxis One of the columns in \code{sample.meta}, including \code{depth}, \code{age}, \code{age.younger}, or \code{age.older}, default \code{age}.
#' @param method An option for axis transformation using \code{tran} from the \code{analogue} package.  \code{"none"} by default.
#' @param group An ecological group from the taxon table.  
#' @param ... variables to be passed to \code{Stratiplot}.
#' @details A wrapper for the \code{analogue} package's \code{Stratiplot} function.  Allowing the user to plot a stratigraphic diagram directly from a \code{download} object.
#' @return A \code{trellis} object.
#' @examples
#' \dontrun{
#' lake_o_dl <- get_download(15925)
#' Stratiplot(lake_o_dl[[1]])
#' }
#' @importFrom analogue Stratiplot tran
#'@export
#'
Stratiplot.download <- function(x, yaxis = "age", method = "none", group = NULL, ...) {
  counts <- counts(x)
  
  if (!yaxis %in% c("depth", "age.older", "age.younger", "age")) {
    stop("You must provide a suitable variable name for the y axis.")
  }
  
  if (!"sample.meta" %in% names(x) | all(is.na(x$sample.meta[,yaxis]))) {
    stop("This download has no suitable chronological or depth information.")
  }
  
  if (!is.null(group)) {
    taxa <- x$taxon.list$taxon.name[x$taxon.list$ecological.group %in% group]
  } else {
    taxa <- x$taxon.list$taxon.name[!x$taxon.list$ecological.group %in% "LABO" ]
    
  }
  
  y <- x$sample.meta[,yaxis]
  
  counts <- analogue::tran(counts[,taxa], method = method)
  
  analogue::Stratiplot(counts, y, ...)
  
}

#' @title Palaeoecological stratigraphic diagrams
#' @description Draws paleoecological diagrams from a \code{download_list} object.  Allows control of variable type (using the \code{tran} function from the \code{analogue} package), and taxonomic grouping.  
#' This function only works for \code{download_list} objects that contain a single object.
#' @param x A \code{download_list} object.
#' @param yaxis One of the columns in \code{sample.meta}, including \code{depth}, \code{age}, \code{age.younger}, or \code{age.older}, default \code{age}.
#' @param method An option for axis transformation using \code{tran} from the \code{analogue} package.  \code{"none"} by default.
#' @param group An ecological group from the taxon table.  
#' @param ... variables to be passed to \code{Stratiplot}.
#' @details A wrapper for the \code{analogue} package's \code{Stratiplot} function.  Allowing the user to plot a stratigraphic diagram directly from a \code{download} object.
#' @return A \code{trellis} object.
#' @examples
#' \dontrun{
#' lake_o_dl <- get_download(15925)
#' # This works:
#' Stratiplot(lake_o_dl)
#' 
#' lakes_o_nw <- get_download(get_site(sitename = "Lake B%"))
#' # This Fails:
#' # Stratiplot(lake_o_nw)
#' 
#' }
#' @importFrom analogue Stratiplot tran
#' @export
#'
Stratiplot.download_list <- function(x, yaxis = "age", method = "none", group = NULL, ...) {
  if (length(x) == 1) {
    Stratiplot(x[[1]], yaxis = "age", method = "none", group = NULL, ...)
  } else {
    stop("You must select a single `download` object to plot.")
  }
    
}