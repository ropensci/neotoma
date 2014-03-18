\name{get_chroncontrol}
\alias{get_chroncontrol}
\title{Function to return chronologia control tables used to build age models.}
\usage{
get_chroncontrol(chronologyid, verbose = TRUE)
}
\arguments{
  \item{chronologyid}{A single numeric dataset ID or a
  vector of numeric dataset IDs as returned by
  \code{get_datasets}.}

  \item{verbose}{logical; should messages on API call be
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
core.of.interest <- get_datasets(taxonname='*Pseudotsuga*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Returns 3 records (as of 04/04/2013), get the dataset for all records:
pollen.records <- get_download(sapply(t8kyr.datasets, function(x) x$chronologyid))

#  Standardize the taxonomies for the different records using the WS64 taxonomy.
compiled.sites <- lapply(pollen.records, function(x) compile_list(x, list.name='WS64'))

#  Extract the Cupressaceae curves for the sites:
get.curve <- function(x, taxa) {
               data.frame(site = x$metadata$site.data$SiteName,
               age = x$sample.meta$Age,
               count = x$counts[,taxa]/rowSums(x$counts, na.rm=TRUE))
             }

curves <- do.call(rbind.data.frame,
                  lapply(compiled.sites, get.curve, taxa = 'Larix/Pseudotsuga'))

#  For illustration, remove the sites with no Pseudotsuga occurance:
curves <- curves[curves$count > 0, ]

smooth.curve <- predict(loess(sqrt(count)~age, data=curves), data.frame(age=seq(20000, 0, by = -100)))
plot(sqrt(count) ~ age, data = curves,
     ylab = '\% Pseudotsuga/Larix', xlab='Calibrated Years BP', pch=19,
     col=rgb(0.1, 0.1, 0.1, 0.1), xlim=c(0, 20000))
lines(seq(20000, 0, by = -100), smooth.curve, lwd=2, lty=2, col=2)

#  This map shows us an apparent peak in Larix/Pseudotsuga pollen in the early-Holocene that
#  lends support to a warmer, drier early-Holocene in western North America.
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

