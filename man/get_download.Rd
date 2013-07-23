\name{get_download}
\alias{get_download}
\title{Function to return full dataset records.}
\usage{
  get_download(datasetid, verbose = TRUE)
}
\arguments{
  \item{datasetid}{A single numeric dataset ID, as returned
  by \code{get_datasets}.}

  \item{verbose}{logical; should messages on API call be
  printed?}
}
\value{
  This command returns either a 'try-error' definined by
  the error returned from the Neotoma API call, or a list
  comprising the following items:

  \itemize{ \item{metadata}{A table describing the
  collection, including dataset information, PI data
  compatable with \code{get_contacts} and site data
  compatable with \code{get_sites}.}
  \item{sample.meta}{Dataset information for the core,
  primarily the age-depth model and chronology.}
  \item{taxon.list}{The list of taxa contained within the
  dataset, unordered, including information that can be
  used in \code{get_taxa}} \item{counts}{The assemblage
  data for the dataset, arranged with each successive depth
  in rows and the taxa as columns.  All taxa are described
  in \code{taxon.list}, the chronology is in
  \code{sample.data}} \item{lab.data}{A data frame of
  laboratory data, such as exotic pollen spike, amount of
  sample counted, etc.} }

  A full data object containing all the relevant assemblage
  information and metadata neccessary to understand a site.
  The data object is a list of lists and data.frames that
  describe an assemblage, the constituent taxa, the
  chronology, site and PIs who contributed the data.
}
\description{
  Using the dataset ID, return all records associated with
  the data.  At present, only returns the dataset in an
  unparsed format, not as a data table.  This function will
  only download one dataset at a time.
}
\examples{
\dontrun{
#  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#  that are on the west coast of North America:
t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
GOLDKBG <- get_download(t8kyr.datasets[[1]]$DatasetID)

taxa.no <- nrow(GOLDKBG$taxon.list)

#  Of the 60 taxa in the record, plot the pollen curve for Abies over time:

##pollen <- GOLDKBG$taxon.list$VariableElement == 'pollen'
pol.curve <- data.frame(age = GOLDKBG$sample.meta$Age,
                        Abies = GOLDKBG$counts[,'Abies'] / rowSums(GOLDKBG$counts, na.rm = TRUE))
plot(Abies * 100 ~ age, data = pol.curve, type='b',
     ylab = '\% Abies', xlab='Calibrated Years BP', pch=19)

}
}
\author{
  Simon J. Goring \email{simon.j.goring@gmail.com}
}
\references{
  Neotoma Project Website: http://www.neotomadb.org API
  Reference:
  http://api.neotomadb.org/doc/resources/contacts
}
\keyword{API}
\keyword{Neotoma}
\keyword{Palaeoecology}

