##' Extracts age information from objects and returns them in a useful format.
##'
##' Methods are available for "download" and "download_list" objects.
##'
##' @title Access proxy age data
##'
##' @param obj an R object from which counts are to be extracted.
##' @param ... arguments passed to other methods.
##' @return Either a data frame of ages or a list of such objects.
##'
##' @author Simon Goring
##'
##' @export
##' @rdname ages
##'
##' @examples
##' \dontrun{
##' ostracodes <- get_dataset(datasettype = 'ostracode')
##'
##' ostro.dl <- get_download(ostracodes)
##' ostro.ages <- ages(ostro.dl)
##' }
`ages` <- function(obj, ...) {
    UseMethod("ages")
}

##' @export
##' @rdname ages
`ages.download` <- function(obj, ...) {
    ret <- as.data.frame(obj$sample.meta)
    class(ret) <- c("neo_ages", "data.frame")
    ret
}

##' @export
##' @rdname ages
`ages.download_list` <- function(obj, ...) {
    ret <- lapply(obj, '[[', 'sample.meta')
    ret <- lapply(ret, as.data.frame)
    class(ret) <- c("neo_ages_list", "list")
    ret
}
