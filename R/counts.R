##' Extract pollen or other proxy counts from data objects and returns them in a useful format.
##'
##'
##'
##' @title
##' @param obj an R object from which counts are to be extracted.
##' @param ...
##' @return
##' @author Gavin Simpson
`counts` <- function(obj, ...) {
    UseMethod("counts")
}

`counts.download` <- function(obj, ...) {
    ret <- obj[["counts"]]
    class(ret) <- c("neo_counts", "data.frame")
    ret
}

