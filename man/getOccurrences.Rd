\name{getOccurrences}
\alias{getOccurrences}
\title{Get occurrences for a given species}
\usage{
  getOccurrences(tvks = NULL, datasets = NULL,
    startYear = NULL, endYear = NULL, VC = NULL,
    group = NULL, gridRef = NULL, latLong = TRUE,
    acceptTandC = FALSE, silent = FALSE)
}
\arguments{
  \item{tvks}{a list of TVKs which are strings of 16
  alphanumeric characters. You can look these up using
  \code{getTVKQuery}}

  \item{datasets}{a list of dataset keys which are strings
  of 8 alphanumeric characters}

  \item{startYear}{a 4 digit integer year}

  \item{endYear}{a 4 digit integer year}

  \item{VC}{a string giving a vice-county name (see
  \code{\link{listVCs}})}

  \item{group}{a string giving the name of a group (see
  \code{\link{listGroups}})}

  \item{gridRef}{a string giving a gridreference in which
  to search for occurrences}

  \item{latLong}{logical, if TRUE latitude and longitude
  are returned as additional columns. The conversion to
  latitude and longitude is currently accurate to about
  about ~20 meters, greater than the vast majoring of
  records' precision.}

  \item{acceptTandC}{if set to \code{TRUE} you accept the
  NBN gateway terms and conditions and privacy policy.
  These can be found at
  \url{https://data.nbn.org.uk/Terms}. Accepting the terms
  and conditions supresses the corresponding warning
  message.}

  \item{silent}{If TRUE batch request information is
  supressed}
}
\value{
  a data.frame of occurence records
}
\description{
  Gets publicly available occurrences from the NBN for a
  species (or list of species) specified by Taxon Version
  Keys in the \code{tvks} parameter (see
  \code{\link{getTVKQuery}}). Alternativly a group can be
  specified to \code{group} as a string, such as 'reptile'
  (see \code{\link{listGroups}}). To get access to data you
  must first register at
  \url{https://data.nbn.org.uk/User/Register}. You will
  need your username and password when running this
  function for the first time.\cr You can also specify a
  (list of) dataset key(s) in the \code{datasets} parameter
  for the datasets from which you want the data to come
  (otherwise all publicly available data will be returned)
  and the earliest and/or latest years for which you want
  data in the \code{startYear} and \code{endYear}
  parameters (otherwise occurrences at any date will be
  returned). A vice-county or grid reference can be
  specified by passing a name to \code{VC} (see
  \code{\link{listVCs}}), or \code{gridRef}.
}
\examples{
\dontrun{
 dt1 <- getOccurrences(tvks="NBNSYS0000002987", datasets="GA000373",
                       startYear="1990", endYear="1991")

 dt2 <- getOccurrences(tvks=c("NBNSYS0000002987","NHMSYS0001688296","NHMSYS0000080210"),
                       startYear="1990", endYear="1991")

 dt3 <- getOccurrences(group="quillwort", startYear="1990", endYear="2010",
                       VC="Shetland (Zetland)")
}
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{getFeature}}, \code{\link{getTVKQuery}},
  \code{\link{listVCs}}, \code{\link{listDatasets}},
  \code{\link{listGroups}}
}

