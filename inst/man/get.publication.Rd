\name{get.publication}
\alias{get.publication}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A file to get publications for sites or datasets in the Neotoma Database using the API.
}
\description{
The function takes the parameters, defined by the user, and returns a table with publication information from the Neotoma Paleoecological Database.
}
\usage{
get.publication(pubid, contactid, datasetid, author, pubtype, year, search)
}

\arguments{
  \item{pubid}{
Numeric Publication ID value, either from \code{get.datasets} or known.
}
  \item{contactid}{
Numeric Contact ID value, either from \code{get.datasets} or \code{get.contacts}
}
  \item{datasetid}{
Numeric Dataset ID, known or from \code{get.datasets}
}
  \item{author}{
Character string for full or partial author's name.  Can include wildcards such as 'Smit*' for all names beginning with 'Smit'.
}
  \item{pubtype}{
Character string, one of eleven allowable types, see \code{get.table('PublicationTypes')}
}
  \item{year}{
Numeric publication year.
}
  \item{search}{
A character string to search for within the article citation.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A table is returned with several fixed columns, and a variable number of author fields:
\item{PublicationID  Unique database record identifier for the publication.}
\item{PubType	Publication type}
\item{Year	Year of publication.}
\item{Citation	The complete citation in a standard style. For legacy citations inherited from other databases, this field holds the citation as ingested from the other databases.}
\item{Authors	Array of author objects, can be of variable length.  Includes \code{Authors.ContactName.n}, \code{Authors.ContactID.n}, \code{Authors.Order.n}, where n ranges from 1 to the maximum number of authors returned by the API call.  When the maximum number of authors is 1 the number is excluded.
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
#  To find all publications in the year 1999
pubs.in.1999 <- get.publication(year=1999)

#  To find all publications that refer to the late-Glacial:
lateg.pub <- get.publication(search='late-Glacial')

}

\keyword{ Neotoma }
\keyword{ Paleoecology }
\keyword{ Palaeoecology }