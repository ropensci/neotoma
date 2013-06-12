\name{get_contacts}
\alias{get_contacts}
\title{A function to obtain contact information for data contributors from the Neotoma
   Paleoecological Database.}
\usage{
  get_contacts(contactid, contactname, contactstatus,
    familyname)
}
\arguments{
  \item{contactid}{Contact ID is a numerical value
  associated with the Neotoma Contact table's numerical
  Contact ID.}

  \item{contactname}{A character string indicating the data
  contributors' project, organization or personal name.
  May be a partial string and can include wildcards.}

  \item{contactstatus}{The current status of the contact.
  Possible values include: active, deceased, defunct,
  extant, inactive, retired, unknown.}

  \item{familyname}{A character string.  Full or partial
  string indicating the contact's last name.}
}
\value{
  The function takes parameters defined by the user and
  returns a list of contact information supplied by the
  Neotoma Paleoecological Database.  The user may define
  all or none of the possible fields.  The function
  contains data chacks for each defined parameter.

  The function returns either a single item of class
  "try-error" describing the reason for failure (either
  mis-defined parameters or an error from the Neotoma API),
  or a table of contacts, with rows corresponding to the
  number of individual contacts returned by the Neotoma
  API.  Each row entry includes the following parameters:

  \itemize{ \item{ContactID}{ Unique database record
  identifier for the contact.} \item{AliasID}{ The
  ContactID of a person's current name. If the AliasID is
  different from the ContactID, the ContactID refers to the
  person's former name.} \item{ContactName}{ Full name of
  the person, last name first (e.g. "Simpson, George
  Gaylord") or name of organization or project (e.g. "Great
  Plains Flora Association").} \item{ContactStatus}{
  Current status of the person, organization, or project.
  Field links to the ContactStatuses lookup table.}
  \item{FamilyName}{ Family or surname name of a person.}
  \item{LeadingInitials}{ Leading initials for given or
  forenames without spaces (e.g. "G.G.").}
  \item{GivenNames}{ Given or forenames of a person (e.g.
  "George Gaylord"). Initials with spaces are used if full
  given names are not known (e.g. "G. G").} \item{Suffix}{
<<<<<<< HEAD:man/get.contacts.Rd
  Suffix of a person's name (e.g. \"Jr.\", "III").}
=======
  Suffix of a person's name (e.g. "Jr.", "III").}
>>>>>>> 9a7a9118a815defe72427557322d5a304b85e28d:man/get_contacts.Rd
  \item{Title}{ A person’s title (e.g. "Dr.", "Prof.",
  "Prof. Dr").} \item{Phone}{ Telephone number.}
  \item{Fax}{ Fax number.} \item{Email}{ Email address.}
  \item{URL}{ Universal Resource Locator, an Internet World
  Wide Web address.} \item{Address}{ Full mailing address.}
  \item{Notes}{ Free form notes or comments about the
  person, organization, or project.} }
}
\description{
  A function to obtain contact information for data
  contributors from the Neotoma Paleoecological Database.
}
\examples{
\dontrun{
#  To find all data contributors who are active:
active.cont <- get_contacts(contactstatus = 'active')

# To find all data contributors who have the last name "Smith"
smith.cont <- get_contacts(familyname = 'Smith')
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

