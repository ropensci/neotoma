\name{get.datasets}
\alias{get.datasets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain full datasets from the Neotoma Paleoecological Database
}
\description{
A function to access the Neotoma API and return datasets corresponding to the parameters defined by the user.
}
\usage{
get.datasets(siteid, datasettype, piid, altmin, altmax, loc, gpid, taxonids, taxonname, ageold, ageyoung, ageof, subdate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{siteid}{
A numeric value corresponding to the site ID.
}
  \item{datasettype}{
A character string corresponding to one of the allowed dataset types in the Neotoma Database.  Allowed types include: 'geochronologic', 'loss-on-ignition', 'pollen', 'plant macrofossils', 'vertebrate fauna', 'mollusks', and 'pollen surface sample'.
}
  \item{piid}{
Numeric value for the Principle Investigator's ID number.
}
  \item{altmin}{
Numeric value indicating the minimum altitude for the site (can be used alone or with altmax).
}
  \item{altmax}{
Numeric value indicating the maximum altitude for the site (can be used alone or with altmin).}
  \item{loc}{
A numeric vector c(lonW, latS, lonE, latN) representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Grewnwich or longitudes south of the equator.
}
  \item{gpid}{
A character string, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: http://api.neotomadb.org/apdx/geopol.htm
}
  \item{taxonids}{
A numeric identifier for the taxon.  Use get.tables('Taxa') for a list of acceptable values.
}
  \item{taxonname}{
A character string corresponding to a valid taxon identity in the Neotoma Database.  Use get.tables('Taxa') for a list of acceptable values.}
  \item{ageold}{
The oldest date acceptable for the search (in years before present).
}
  \item{ageyoung}{
The youngest date acceptable for the search.
}
  \item{ageof}{
If a taxon ID or taxon name is defined this parameter must be set to "taxon", otherwise it may refer to "sample", in which case the age bounds are for any samples within datasets or "dataset" if you want only datasets that are within the bounds of ageold and ageyoung.
}
  \item{subdate}{
Date of dataset submission, either YYYY-MM-DD or MM-DD-YYYY.
}
}
\details{
More details on the use of these parameters can be obtained from http://api.neotomadb.org/doc/resources/datasets.  
}
\value{
A list, with each item corresponding to an individual record.  Each list item (each dataset record) includes the following components:
\item{DatasetID}{Unique database record identifier for the dataset.}
\item{DatasetName}{Name of the dataset; not commonly used.}
\item{CollUnitHandle}{Code name of the Collection Unit with which the dataset is associated. This code may be up to 10 characters. Data are frequently distributed by Collection Unit, and the Handle is used for file names.}
\item{CollUnitID}{Unique database record identifier for the collection unit.}
\item{CollType}{The collection type. Types include cores, sections, excavations, and animal middens.}
\item{DatasetType}{The dataset type, such as: geochronologic, loss-on-ignition, pollen, plant macrofossils, vertebrate fauna, etc.}
\item{AgeOldest}{The oldest of all sample ages (in calendar years before present) in the dataset.}
\item{AgeYoungest}{The youngest of all sample ages (in calendar years before present) in the dataset.}
\item{SubDates}{An array of objects that describe dataset submission events.  If multiple submissions occured then this is a table.}
\item{DatasetPIs}{An array of objects that describe Principal Investigators associated with a dataset.}
\item{Site}{An object describing the site where the dataset samples were taken.}

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
#  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#  that are on the west coast of North America:
t8kyr.datasets <- get.datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)

#  Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.

gpids <- get.table(table.name='GeoPoliticalUnits')
canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]

v2kyr.datasets <- get.datasets(datasettype='vertebrate fauna', gpid=canID, ageold = 2000)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Neotoma }
\keyword{ paleoecology }
\keyword{ palaeoecology }
\keyword{ API }
