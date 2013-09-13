\name{compile_list}
\alias{compile_list}
\title{Function to convert assemblage taxa to standardized lists.}
\usage{
  compile_list(object, list.name, cf = TRUE, type = TRUE)
}
\arguments{
  \item{object}{A pollen object returned by
  \code{get_download}.}

  \item{list.name}{The taxon compilation list, one of a set
  of lists from the literature (e.g., P25, Whitmore).  More
  detail in the Description.}
}
\value{
  This command returns a list object with the same
  structure as the parent pollen object returned by
  \code{get_download}, or a \code{matrix} (or
  \code{data.frame}) depending on whether \code{object} is
  one or the other.  Any pollen taxon not included in the
  major taxa defined in the pollen gets returned as
  'Other'.

  \itemize{ \item{object}{Either a pollen object as
  returned by \code{get_download} or a \code{matrix} or
  \code{data.frame} containing the pollen data with rows
  representing strata and columns representing individual
  pollen taxa.} \item{list.name}{One of a set of predefined
  lists, 'P25', 'WS64', 'WhitmoreFull', or 'WhitmoreSmall'.
  See below for more details.} \item{cf}{Should taxa listed
  as *cf*s (*e.g.*, *cf*. *Gilia*) be considered highly
  resolved?} \item{type}{Should taxa listed as types
  (*e.g.*, *Iva annua*-type) be considered highly
  resolved?} }

  The data object uses the smaller pollen subset.  As this
  package develops we will add the capacity to summarize
  data output from the translation.  Currently we can
  return only subsets that have been defined in the
  literature.  These lists include: \itemize{ \item{P25}{
  This list is derived from Gavin et al., (2003), and
  includes 25 pollen taxa.} \item{WS64}{ This list is
  derived from Williams and Shuman (2008).}
  \item{WhitmoreFull}{ This is the full list associated
  with the Whitmore et al., (2008) North American Modern
  Pollen Database.} \item{WhitmoreSmall}{ As above, but
  taxa for which both fully resolved and undifferentiated
  exist these taxa are summed.} }
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
  Williams J, Shuman B. 2008. Obtaining accurate and
  precise environmental reconstructions from the modern
  analog technique and North American surface pollen
  dataset. Quaternary Science Reviews. 27:669-687.
  http://dx.doi.org/10.1016/j.quascirev.2008.01.004

  API Reference:
  http://api.neotomadb.org/doc/resources/contacts
}
\keyword{API}
\keyword{Neotoma}
\keyword{Palaeoecology}

