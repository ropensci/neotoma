\name{get_taxa}
\alias{get_taxa}
\title{Get taxon information from Neotoma.}
\usage{
get_taxa(taxonid, taxonname, status, taxagroup, ecolgroup)
}
\arguments{
  \item{taxonid}{Numeric taxon identifier used in Neotoma}

  \item{taxonname}{A character string representing the full
  or partial name of taxa of interest.}

  \item{status}{The current status of the taxon, one of
  'extinct', 'extant', 'all'.}

  \item{taxagroup}{The taxonomic grouping for the taxa. See
  \url{http://api.neotomadb.org/doc/resources/taxa} for the
  list of approved groupings.}

  \item{ecolgroup}{The ecological group of the taxa. More
  detailed than \code{taxagroup}, can be obtained using
  \code{get_table("EcolGroupTypes")}.}
}
\value{
Returns a table.

\itemize{ \item{TaxonID}{Unique database record identifier
for a taxon.} \item{TaxonCode}{Shorthand notation for a
taxon identification.} \item{TaxonName}{Name of the taxon.}
\item{Author}{Author(s) of the name. Used almost
exclusively with beetle taxa.} \item{Extinct}{True if
extinct; false if extant.} \item{TaxaGroup}{Code for taxa
group to which taxon belongs.} \item{EcolGroups}{Array of
ecological group codes to which the taxon belongs.}
\item{HigherTaxonID}{TaxonID of the next higher taxonomic
rank.} \item{PublicationID}{Publication identification
number.} \item{Notes}{Free-form notes or comments about the
taxon.} }
}
\description{
Get taxon information from Neotoma.
}
\examples{
\dontrun{
## Return all species taxa with "Abies" in name - note wildcard
taxa <- get_taxa(taxonname = "Abies*")
}
}
\author{
Simon J. Goring \email{simon.j.goring@gmail.com}
}
\references{
Neotoma Project Website: http://www.neotomadb.org API
Reference: http://api.neotomadb.org/doc/resources/contacts
}
\keyword{API}
\keyword{Neotoma}
\keyword{Palaeoecology}

