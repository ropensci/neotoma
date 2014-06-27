\docType{package}
\name{neotoma-package}
\alias{neotoma-package}
\alias{neotoma}
\alias{neotoma-package}
\title{Paleoecological data retrieval and manipulation}
\description{Programmatic interface to the Neotoma Paleoecological Database}
\details{
  The neotoma package is a programmatic R interface to the Neotoma Paleoecological Database (\href{http://neotomadb.org}{link}). The package is intended to allow users to search for sites and to download data for use in analyical workflows of paleoecological research.  The package also provides some basic summary and manipulation tools.

  \code{neotoma} is part of the \href{http://ropensci.org/}{rOpenSci} project and is also hosted on \href{http://http://figshare.com/articles/RNeotoma_an_R_package_for_the_Neotoma_Paleoecological_Database/677131}{Figshare}.

  Package functions resolve various Neotoma APIs and re-form the data returned by the Neotoma database into R data objects. The format of the Neotoma data, and the actual API functions can be accessed on the Neotoma API website.
Currently Implemented:
  \tabular{ll}{
    \code{\link{compile_list}} \tab using established pollen-related taxonomies from the literature, take the published taxon list and standardize it to allow cross site analysis.\cr
    \code{\link{get_contacts}} \tab find contact information for data contributors to Neotoma.\cr
    \code{\link{get_datasets}} \tab obtain dataset metadata from Neotoma.\cr
    \code{\link{get_download}} \tab obtain full datasets from Neotoma.\cr
    \code{\link{get_publication}} \tab obtain publication information from Neotoma.\cr
    \code{\link{get_sites}} \tab obtain information on sites in the Neotoma dataset (which may contain multiple datasets).\cr
    \code{\link{get_table}} \tab return matrices corresponding to one of the Neotoma database tables. tables\cr
    \code{\link{get_taxa}} \tab Get taxon information from Neotoma.\cr
    \code{\link{get_chroncontrol}} \tab Get chronological information used to build the age-depth model for the record.\cr
  }
}
\section{Recent Changes}{
  \itemize{
  \item Minor fixes to the way that get_download treats age models reflecting the fact that some age models cover only part of the record.
  \item Changed the output format of get_publication to improve data structure and readability.
}
}

\section{Coming Soon}{
  \itemize{
    \item A broader set of taxonomies for use with datasets, including geography based taxonomies.
    \item Classes for objects to improve summary functions.
  }
}
\section{Dependencies}{
\code{neotoma} requires functions from \pkg{RJSONIO}, \pkg{RCurl}, \pkg{plyr} and \pkg{reshape2}.
}

\author{
Simon Goring \email{simon.j.goring@gmail.com}
}
\section{Contributors}{
  \tabular{ll}{
Gavin Simpson \tab University of Regina, Department of Biology \cr
Jeremiah Marsicek \tab University of Wyoming, Department of Geology and Geophysics \cr
Karthik Ram \tab University of California - Berkely, College of Natural Resources\cr
}
}

\keyword{package}