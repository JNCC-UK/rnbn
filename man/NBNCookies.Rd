\name{NBNCookies}
\alias{NBNCookies}
\title{A simple function for removing cookies}
\usage{
  NBNCookies(path = getwd(), remove = FALSE)
}
\arguments{
  \item{path}{The directory where the cookies are stored,
  this is your working directory.}

  \item{remove}{Logical, if TRUE the cookie file is deleted
  if FALSE the existance of the file is reported.}
}
\value{
  NULL
}
\description{
  This function looks for cookies used for logging into the
  NBN. Sometimes this login process can go wrong, using
  this function you can delete cookies resolving some
  issues.
}
\examples{
\dontrun{
 NBNCookies()
 }
}
\author{
  Tom August, CEH \email{tomaug@ceh.ac.uk}
}

