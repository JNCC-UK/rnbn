\name{getGroupSpeciesTVKs}
\alias{getGroupSpeciesTVKs}
\title{Gets TVKs for a group}
\usage{
getGroupSpeciesTVKs(name)
}
\arguments{
  \item{name}{A string for a group on the NBN gateway (e.g.
  'reptile')}
}
\value{
A vector of TVKs as characters.
}
\description{
Given the name of a group (see \code{\link{listGroups}})
this function returns the pTVKs (preferred taxon version
keys) for all members of that group. This is currently
restricted to returning up to 5000 results.
}
\examples{
\dontrun{
 t <- getGroupSpeciesTVKs('reptile')
}
}
\author{
Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
\code{\link{getFeature}}, \code{\link{getOccurrences}},
\code{\link{getTVKQuery}}
}

