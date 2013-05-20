\name{getTaxonomy}
\alias{getTaxonomy}
\title{Get the taxonomical heirarchy for a given taxon}
\usage{
  getTaxonomy(tvks = NULL)
}
\arguments{
  \item{tvks}{A Taxon Version Key (TVK) which is a
  16-character string ID}
}
\value{
  A dataframe containing the JSON object returned by the
  NBN Gateway.
}
\description{
  Given the Taxon Version Key (a 16-character string), this
  function get details of the taxonomical heirarchy above
  the given TVK. This gives the rank, name and preferred
  Taxon Version Key for each level of the taxonomy
}
\examples{
\dontrun{
 t <- getTaxonomy("NHMSYS0000528028") # Myotis daubentonii (Daubenton's Bat)
}
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{getFeature}}, \code{\link{getOccurrences}}
}

