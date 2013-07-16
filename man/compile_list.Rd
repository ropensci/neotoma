\name{compile_list}
\alias{compile_list}
\title{Function to convert assemblage taxa to standardized lists.}
\usage{
  compile_list(object, list.name, verbose = TRUE)
}
\arguments{
  \item{object}{A pollen object returned by
  \code{get_download}.}

  \item{list.name}{The taxon compilation list, one of a set
  of lists from the literature (e.g., P25, Whitmore).  More
  detail in the Description.}

  \item{verbose}{logical; print messages about progress?}
}
\value{
  This command returns a list object containing
  \code{count} and \code{taxon.list} objects, similar to
  those associated with the \code{get_download} object.
  Any pollen taxon not included in the major taxa defined
  in the pollen gets returned as 'Other'.

  \itemize{ \item{taxon.list}{ The list of taxa contained
  within the dataset, unordered, including information that
  can be used in \code{get_taxa}} \item{counts}{ The
  assemblage data for the dataset, arranged with each
  successive depth in rows and the taxa as columns.  All
  taxa are described in \code{taxon.list}, the chronology
  is in \code{sample.data}} }

  The data object uses the smaller pollen subset.  As this
  package develops we will add the capacity to summarize
  data output from the translation.  Currently we can
  return only subsets that have been defined in the
  literature.  These lists include: \itemize{ \item{P25}{
  This list is derived from Gavin et al., (2003), and
  includes 25 pollen taxa.} \item{WhitmoreFull}{ This is
  the full list associated with the Whitmore et al., (2008)
  North American Modern Pollen Database.} \item{Whitmore}{
  As above, but taxa for which both fully resolved and
  undifferentiated exist these taxa are summed.} }
}
\description{
  From the assemblage data for the core return assemblage
  data with the assemblage taxa
}
\examples{
\dontrun{
#  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#  that are on the west coast of North America:
t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
GOLDKBG <- get_download(t8kyr.datasets[[1]]$DatasetID)

gold.p25 <- compile_list(GOLDKBG, 'P25')



}
}
\author{
  Simon J. Goring \email{simon.j.goring@gmail.com}
}
\references{
  Neotoma Project Website: http://www.neotomadb.org

  API Reference:
  http://api.neotomadb.org/doc/resources/contacts
}
\keyword{API}
\keyword{Neotoma}
\keyword{Palaeoecology}

