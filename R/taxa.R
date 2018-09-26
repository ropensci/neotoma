##' Extracts taxa from \code{download} objects and returns them in a useful format.
##'
##' Methods are available for "download" and "download_list" objects.
##'
##' @title Access proxy taxonomic data
##'
##' @param obj an R object from which counts are to be extracted.
##' @param collapse should the results be returned as a list, one for each site (\code{FALSE}), or a single dataframe of all taxa? Default is \code{TRUE}
##' @param hierarchy Should the taxonomic hierarchy be included?
##' @param ... arguments passed to other methods.
##' @return Either a data frame of taxa or a list of such objects.
##'
##' @author Simon Goring
##'
##' @export
##' @rdname taxa
##'
##' @examples
##' \dontrun{
##' ostracodes <- get_dataset(datasettype = 'ostracode')
##'
##' ostro.dl <- get_download(ostracodes)
##' ostro.taxa <- taxa(ostro.dl)
##' }
`taxa` <- function(obj, ...) {
    UseMethod("taxa")
}

##' @export
##' @rdname taxa
`taxa.download` <- function(obj, ...) {
    ret <- as.data.frame(obj$taxon.list,
                         stringsAsFactors = FALSE)
    class(ret) <- c("neo_taxa", "data.frame")
    ret
}

##' @export
##' @importFrom dplyr bind_rows
##' @rdname taxa
`taxa.download_list` <- function(obj, collapse = TRUE, hierarchy = FALSE, ...) {
    ret <- lapply(obj, '[[', 'taxon.list')
    ret <- lapply(ret,
                  as.data.frame,
                  stringsAsFactors = FALSE)

    # if (hierarchy == TRUE) {
    #  taxonomy <- get_table("Taxa")

      # match_taxa <- function(tax){
      #   row <- match(tax, taxonomy$TaxonName)
      #
      #   output <- taxonomy[row,]
      #   if (taxonomy$TaxonID[row] == taxonomy$HigherTaxonID[row]) {
      #     return(taxonomy[row,])
      #   } else {
      #     output <- rbind(output,
      #                     match_taxa(taxonomy$TaxonName[which(taxonomy$TaxonID == taxonomy$HigherTaxonID[row])]))
      #   }
      #   return(output)
      # }

    # }

    if (collapse == TRUE) {
      ret <- dplyr::bind_rows(ret)
      ret <- ret[!duplicated(ret[,c(1:3)]),]

      # if(hierarchy == TRUE) {
      #   taxonhier <- do.call(rbind.data.frame, lapply(ret$taxon.name, match_taxa))
      #   taxonhier <- taxonhier[!duplicated(taxonhier), c("TaxonID", "TaxonName", "TaxaGroupID", "HigherTaxonID")]
      # }
    }

    class(ret) <- c("neo_taxa_list", "list")
    ret
}
