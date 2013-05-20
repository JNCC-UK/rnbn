\name{listDatasets}
\alias{listDatasets}
\title{Get Dataset definitions}
\usage{
  listDatasets()
}
\value{
  A dataframe containing the names and keys of datasets on
  the NBN Gateway.
}
\description{
  Gives a dataframe of the datasets available from the NBN
  Gateway for reference.
}
\examples{
\dontrun{
 t <- listDatasets()
}
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{getFeature}}, \code{\link{getOccurrences}}
}

