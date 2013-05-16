\name{makenbnurl}
\alias{makenbnurl}
\title{Build the URL to call NBN web-services}
\usage{
  makenbnurl(service = NULL, tvks = NULL, datasets = NULL,
    feature = NULL, startYear = NULL, endYear = NULL)
}
\arguments{
  \item{service}{the service you want to call. One of
  \code{"obs"} for the taxonObservations service,
  \code{"feature"} for the features service or
  \code{"taxon"} for the taxa service. The first letter is
  sufficient}

  \item{tvks}{a list of TVKs which are strings of 16
  alphanumeric characters}

  \item{datasets}{a list of dataset keys which are strings
  of 8 alphanumeric characters}

  \item{feature}{a featureID an integer number}

  \item{startYear}{a 4 digit year}

  \item{endYear}{a 4 digit year}
}
\value{
  the URL to call - a character string
}
\description{
  The base URL for the RESTful services is:
  \url{http://http://staging.testnbn.net/api/} Parameters
  can be added, depending on the service called. \cr\cr The
  following services are available at the time of writing:
  \cr
  \code{taxonObservations?ptvk=<tvk>&datasetKey=<dataset>&startYear=<year>
  &endYear=<year>} - get observations for the given (list
  of) Taxon Version Keys (TVKs) for the given (list of)
  dataset keys optionally with dates between startYear and
  endYear. If no dataset keys are given then all publically
  available data for the taxa will be returned \cr\cr
  \code{features/<featureID>} - get details about a
  particular location given its <featureID> \cr\cr
  \code{taxa/<tvk>} - get details about a taxon given its
  <tvk>
}
\examples{
makenbnurl(service="obs", tvks="NBNSYS0000007073")
makenbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001")
makenbnurl(service="obs", tvks="NBNSYS0000007073", datasets="SGB00001", startYear="1990", endYear="2010")
makenbnurl(service="feature", feature="284443")
makenbnurl(service="taxon", tvks="NBNSYS0000007073")
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}

