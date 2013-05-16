\name{getTaxon}
\alias{getTaxon}
\title{Get details for a given taxon}
\usage{
  getTaxon(tvk = NULL)
}
\arguments{
  \item{tvk}{A Taxon Version Key (TVK) which is a
  16-character string ID}
}
\value{
  A list containing the JSON object returned by the NBN
  Gateway.
}
\description{
  Given the Taxon Version Key (a 16-character string), this
  function get details of the taxon from the NBN Gateway.
  These include the name, authority, taxonomic rank,
  whether or not it is a synonym, etc.
}
\examples{
\dontrun{
 t <- getTaxon("NBNSYS0000007094")
 t['name']  ## [1] "Volucella bombylans"
}
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{getFeature}}, \code{\link{getOccurrences}}
}

