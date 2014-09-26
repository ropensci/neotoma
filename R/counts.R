##' Extract pollen or other proxy counts from data objects and returns them in a useful format.
##'
##' Methods are available for "download" and "download_list" objects.
##'
##' @title Access proxy count data
##'
##' @param obj an R object from which counts are to be extracted.
##' @param ... arugments passed to other methods.
##' @return Either a data frame of counts or a list of such objects.
##'
##' @author Gavin Simpson
##'
##' @export
##' @rdname counts
`counts` <- function(obj, ...) {
    UseMethod("counts")
}

##' @export
##' @rdname counts
`counts.download` <- function(obj, ...) {
    ret <- obj[["counts"]]
    class(ret) <- c("neo_counts", "data.frame")
    ret
}

##' @export
##' @rdname counts
`counts.download_list` <- function(obj, ...) {
    ret <- lapply(obj, counts)
    class(ret) <- c("neo_counts_list", "list")
    ret
}
