\name{get_geochron}
\alias{get_geochron}
\title{Function to return geochronological data from records.}
\usage{
get_geochron(datasetid, verbose = TRUE)
}
\arguments{
  \item{datasetid}{A single numeric dataset ID or a vector
  of numeric dataset IDs as returned by
  \code{get_datasets}.}

  \item{verbose}{logical; should messages on API call be
  printed?}
}
\value{
This command returns either a 'try-error' definined by the
error returned from the Neotoma API call, or geochronology
table:

\itemize{ \item{sample.id}{A unique identifier for the
geochronological unit.} \item{age.type.id}{Numeric. One of
five possible age types.} \item{age.type}{String.  The age
type, one of calendar years, radiocarbon years, etc.}
\item{age}{Dated age of the material.} \item{e.older}{The
older error limit of the age value.  Commonly 1 standard
deviation.} \item{e.young}{The younger error limit of the
age value.} \item{delta13C}{The measured or assumed δ13C
value for radiocarbon dates, if provided.}
\item{material.dated}{A table describing the collection,
including dataset information, PI data compatable with
\code{get_contacts} and site data compatable with
\code{get_sites}.} \item{geo.chron.type.id}{Numeric
identification for the type of geochronological analysis.}
\item{geo.chron.type}{Text string, type of geochronological
analysis, i.e., Radiocarbon dating, luminesence.}
\item{notes}{Text string} \item{infinite}{Boolean, does the
dated material return an "infinte" date?} }

A full data object containing all the relevant
geochronological data available for a dataset.
}
\description{
Using the dataset ID, return all geochronological data
associated with the dataID.  At present, only returns the
dataset in an unparsed format, not as a data table.  This
function will only download one dataset at a time.
}
\examples{
\dontrun{
#  Search for sites with "Pseudotsuga" pollen that are older than 8kyr BP and
#  find the relevant geochronological data associated with the samples.  Are some time periods better dated than others?
t8kyr.datasets <- get_datasets(taxonname='*Pseudotsuga*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Returns 74 records (as of 01/08/2014), get the dataset IDs for all records:
dataset.ids <- sapply(t8kyr.datasets, function(x) x$DatasetID)
geochron.records <- sapply(dataset.ids, function(x)try(get_geochron(x)))

#  Standardize the taxonomies for the different records using the WS64 taxonomy.

get_ages <- function(x){
  any.ages <- try(x$age[x$age.type.id == 4])
  if(class(any.ages) == 'try-error') output <- NA
  if(!class(any.ages) == 'try-error') output <- unlist(any.ages)
  output
}

radio_chron <- unlist(sapply(geochron.records, get_ages))

hist(radio_chron, breaks=seq(0, 40000, by = 500),
     main = 'Distribution of radiocarbon dates for Pseudotsuga records',
     xlab = 'Radiocarbon date (14C years before 1950)')
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

