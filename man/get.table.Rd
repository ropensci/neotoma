\name{get.table}
\alias{get.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Neotoma value tables.
}
\description{
Use the Neotoma API to obtain data tables from the Neotoma Paleoecological Database.
}
\usage{
get.table(table.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table.name}{
Call one of the available tables in the Neotoma Database.  A full listing of tables can be found here: \link{http://api.neotomadb.org/doc/resources/dbtables}.  By default it returns all objects in the table.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A table of values corresponding to the parameter of interest.
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