\name{get.download}
\alias{get.download}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to return full dataset records.
}
\description{
Using the dataset ID, return all records associated with the data.  At present, only returns the dataset in an unparsed format, not as a data table.
}

\usage{
get.download(datasetid)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasetid}{Dataset ID, as returned by \code{get.datasets}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This command returns either a 'try-error' definined by the error returned from the Neotoma API call, or a list comprising the following values:

\item{DatasetID}{Unique database record identifier for the dataset.}
\item{DatasetName}{Name of the dataset; not commonly used.}
\item{CollUnitHandle}{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
\item{CollUnitType}{The collection type. Types include cores, sections, excavations, and animal middens.}
\item{DatasetType}{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
\item{NeotomaLastSub}{The date of the most recent submission.}
\item{DefChronologyID}{The unique database identifier for the default chronology.}
\item{DatasetPIs}{An array of objects that describe Principal Investigators associated with a dataset}
\item{Site}{	An object describing the site where the dataset samples were taken}
\item{Samples}{	An array of objects describing the individual dataset samples}

Here the \code{Samples} is the real prize, but at present the format of the data object is simply a list of lists.  This needs work.
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
\code{\link{get.datasets}}
}

\examples{
#  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#  that are on the west coast of North America:
t8kyr.datasets <- get.datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.

GOLDKBG <- get.download(t8kyr.datasets[[1]]$DatasetID)

}

\keyword{ Neotoma }
\keyword{ Paleoecology }
\keyword{ Palaeoecology }
\keyword{ API }