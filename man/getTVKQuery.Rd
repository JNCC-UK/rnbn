\name{getTVKQuery}
\alias{getTVKQuery}
\title{Gets TVKs for a query}
\usage{
  getTVKQuery(query = NULL, species_only = TRUE,
    rec_only = FALSE, top = FALSE)
}
\arguments{
  \item{query}{A query string. This can range from latin
  binomials to partial english names.}

  \item{species_only}{Logical, if \code{TRUE} pTVKs of
  species are returned (i.e. sub-species and aggregates are
  removed). Defaults to \code{TRUE}}

  \item{rec_only}{Logical, if \code{TRUE} pTVKs of
  recommended names are returned (i.e. synonyms are
  removed). Defaults to \code{FALSE}. Remember, the pTVK of
  a synonym is a taxa with 'recommended' name status.}

  \item{top}{Logical, if \code{TRUE} only the top answer is
  returned. This is what the gateway thinks you are most
  likely to be after but may not always be right, use with
  care!}
}
\value{
  A vector of TVKs as characters.
}
\description{
  Given a search term this function returns the pTVKs for
  the first 25 taxa that match that search on the NBN.
}
\examples{
\dontrun{
 t <- getTVKQuery('blue tit')
}
}
\author{
  Tom August, CEH \email{tomaug@ceh.ac.uk}
}
\seealso{
  \code{\link{getGroupSpeciesTVKs}},
  \code{\link{getOccurrences}}
}

