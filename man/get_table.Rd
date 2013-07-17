\name{get_table}
\alias{get_table}
\title{Get Neotoma value tables.}
\usage{
  get_table(table.name = NULL)
}
\arguments{
  \item{table.name}{Call one of the available tables in the
  Neotoma Database.  A full listing of tables can be found
  here:
  \url{http://api.neotomadb.org/doc/resources/dbtables}.
  By default it returns all objects in the table.}
}
\description{
  Get Neotoma value tables.
}
\details{
  A table of values corresponding to the parameter of
  interest.
}
\examples{
\dontrun{
taxon.table <- get_table('Taxa')

#  Get the frequency of the first ten taxa in Neotoma.
tester <- function(x) length(get_datasets(taxonname = x))
taxon.counts2 <- sapply(taxon.table$TaxonName[1:10], tester)
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

