\name{get.taxa}
\alias{get.taxa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get taxon information from Neotoma
}
\description{
Calls the Neotoma Database using an API and returns information for all taxa matching the user defined parameters.
}
\usage{
get.taxa(taxonid, taxonname, status, taxagroup, ecolgroup)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxonid}{
Numeric taxon identifier used in Neotoma
}
  \item{taxonname}{
A character string representing the full or partial name of taxa of interest.
}
  \item{status}{
The current status of the taxon, one of 'extinct', 'extant', 'all'.
}
  \item{taxagroup}{
The taxonomic grouping for the taxa. See \link{http://api.neotomadb.org/doc/resources/taxa} for the list of approved groupings.
}
  \item{ecolgroup}{
The ecological group of the taxa. More detailed than \code{taxagroup}, can be obtained using \code{get.table("EcolGroupTypes")}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a table.
\item{TaxonID}{Unique database record identifier for a taxon.}
\item{TaxonCode}{Shorthand notation for a taxon identification.}
\item{TaxonName}{Name of the taxon.}
\item{Author}{Author(s) of the name. Used almost exclusively with beetle taxa.}
\item{Extinct}{True if extinct; false if extant.}
\item{TaxaGroup}{Code for taxa group to which taxon belongs.}
\item{EcolGroups}{Array of ecological group codes to which the taxon belongs.}
\item{HigherTaxonID}{TaxonID of the next higher taxonomic rank.}
\item{PublicationID}{Publication identification number.}
\item{Notes}{Free-form notes or comments about the taxon.}
%% ...
}
\references{
Neotoma Project Website: http://www.neotomadb.org

API Reference:  http://api.neotomadb.org/doc/resources/contacts
}
\author{
Simon J. Goring
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
taxon.table <- get.table('Taxa')

#  Get the frequency of the first ten taxa in Neotoma.
tester <- function(x){ length(get.datasets(taxonname=x)) }
taxon.counts <- ldply(as.character(taxon.table$TaxonName)[1:10], tester, .progress='text')

}
\keyword{ Neotoma }
\keyword{ paleoecology }
\keyword{ palaeoecology }
\keyword{ API }