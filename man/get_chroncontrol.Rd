\name{get_chroncontrol}
\alias{get_chroncontrol}
\title{Function to return chronological control tables used to build age models.}
\usage{
get_chroncontrol(chronologyid, verbose = TRUE)
}
\arguments{
  \item{chronologyid}{A single numeric dataset ID or a
  vector of numeric dataset IDs as returned by
  \code{get_datasets}.}

  \item{verbose}{logical, should messages on API call be
  printed?}
}
\value{
This command returns either a 'try-error' definined by the
error returned from the Neotoma API call, or a list
comprising the following items:

\itemize{ \item{chron.control}{A table describing the
collection, including dataset information, PI data
compatable with \code{get_contacts} and site data
compatable with \code{get_sites}.} \item{meta}{Dataset
information for the core, primarily the age-depth model and
chronology.  In cases where multiple age models exist for a
single record the most recent chronology is provided here.}
}

A full data object containing all the relevant information
required to build either the default or prior chronology
for a core.
}
\description{
Using the dataset ID, return all records associated with
the data.  At present, only returns the dataset in an
unparsed format, not as a data table.  This function will
only download one dataset at a time.
}
\examples{
\dontrun{
#  The point of pulling chronology tables is to re-build or examine the chronological
#  information that was used to build the age-depth model for the core.
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

