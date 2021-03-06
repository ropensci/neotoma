% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dataset.R
\name{get_dataset.default}
\alias{get_dataset.default}
\title{Obtain dataset information from the Neotoma Paleoecological Database or an existing object.}
\usage{
\method{get_dataset}{default}(
  x,
  datasettype,
  piid,
  altmin,
  altmax,
  loc,
  gpid,
  taxonids,
  taxonname,
  ageold,
  ageyoung,
  ageof,
  subdate
)
}
\arguments{
\item{x}{A numeric value corresponding to the site ID.}

\item{datasettype}{A character string corresponding to one of the allowed dataset types in the Neotoma Database.  You can find the full list of allowed datasettypes using: \code{get_table("datasettypes")}.}

\item{piid}{Numeric value for the Principle Investigator's ID number.}

\item{altmin}{Numeric value indicating the minimum altitude for the site (can be used alone or with \code{altmax}).}

\item{altmax}{Numeric value indicating the maximum altitude for the site (can be used alone or with \code{altmin}).}

\item{loc}{A numeric vector \code{c(lonW, latS, lonE, latN)} representing the bounding box within which to search for sites.  The convention here is to use negative values for longitudes west of Greenwich or longitudes south of the equator}

\item{gpid}{A character string or numeric value, must correspond to a valid geopolitical identity in the Neotoma Database.  Use get.tables('GeoPoliticalUnits') for a list of acceptable values, or link here: \url{http://wnapi.neotomadb.org/apdx/geopol.htm}}

\item{taxonids}{A numeric identifier for the taxon.  See \code{\link{get_table}} and use \code{get_table('Taxa')} for a list of acceptable values.}

\item{taxonname}{A character string corresponding to a valid taxon identity in the Neotoma Database.  See \code{\link{get_table}} and use \code{get_table('Taxa')} for a list of acceptable values.}

\item{ageold}{The oldest date acceptable for the search (in years before present).}

\item{ageyoung}{The youngest date acceptable for the search.}

\item{ageof}{If a taxon ID or taxon name is defined this parameter must be set to \code{"taxon"}, otherwise it may refer to \code{"sample"}, in which case the age bounds are for any samples within datasets or \code{"dataset"} if you want only datasets that are within the bounds of ageold and ageyoung.}

\item{subdate}{Date of dataset submission, either YYYY-MM-DD or MM-DD-YYYY.}
}
\description{
A function to access the Neotoma API and return datasets corresponding to the parameters defined by the user.
}
