\name{get_sites}
\alias{get_sites}
\title{Return Site Information.}
\usage{
  get_sites(siteid, sitename, altmin, altmax, loc, gpid)
}
\arguments{
  \item{siteid}{The numerical site ID.}

  \item{sitename}{A character string representing the full
  or partial site name.}

  \item{altmin}{Minimum site altitude (in m).}

  \item{altmax}{Maximum site altitude (in m).}

  \item{loc}{A numeric vector c(lonW, latS, lonE, latN)
  representing the bounding box within which to search for
  sites.  The convention here is to use negative values for
  longitudes west of Grewnwich or longitudes south of the
  equator.}

  \item{gpid}{A character string or numeric value, must
  correspond to a valid geopolitical identity in the
  Neotoma Database.  Use get.tables('GeoPoliticalUnits')
  for a list of acceptable values, or link here:
  http://api.neotomadb.org/apdx/geopol.htm}
}
\value{
  A table:

  \describe{ \item{SiteID}{Unique database record
  identifier for the site.} \item{SiteName}{Name of the
  site.} \item{Altitude}{Altitude in meters.}
  \item{LatitudeNorth}{North bounding latitude, in decimal
  degrees, for a site.} \item{LatitudeSouth}{South bounding
  latitude, in decimal degrees, for a site.}
  \item{LongitudeEast}{East bounding longitude, in decimal
  degrees, for a site.} \item{LongitudeWest}{West bounding
  longitude, in decimal degrees, for a site.}
  \item{SiteDescription}{Free form description of a site,
  including such information as physiography and vegetation
  around the site.} }

  Extended response variables when only a single site is
  returned: \itemize{ \item{CollectionUnitID}{Unique
  database record identifier for the collection unit.}
  \item{Handle}{Code name for the collection unit. This
  code may be up to 10 characters, but an effort is made to
  keep these to 8 characters or less. Data are frequently
  distributed by collection unit, and the handle is used
  for file names.} \item{CollType}{The collection type.
  Types include cores, sections, excavations, and animal
  middens. Collection Units may be modern collections,
  surface float, or isolated specimens. Composite
  Collections Units include different kinds of Analysis
  Units, for example a modern surface sample for ostracodes
  and an associated water sample.} \item{Datasets}{An array
  of objects that describe datasets associated with a
  site.} }
}
\description{
  \code{get_sites} returns site information from the
  Neotoma Paleoecological Database based on parameters
  defined by the user.
}
\examples{
\dontrun{
#  What is the distribution of site elevations in Neotoma?
all.sites <- get_sites()  #takes a bit of time.

plot(density(all.sites$Altitude, from = 0, na.rm=TRUE),
main = 'Altitudinal Distribution of Neotoma Sites', xlab = 'Altitude (m)', log='x')
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

