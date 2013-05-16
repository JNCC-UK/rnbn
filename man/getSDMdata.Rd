\name{getSDMdata}
\alias{getSDMdata}
\title{Get data required to fit a SDM for one or more species}
\usage{
  getSDMdata(species = NULL, datasets = NULL,
    startYear = NULL, endYear = NULL,
    gridSystem = c("OSGB", "OSNI"), units = c("km", "m"),
    precision = 1000, unique = TRUE, includeNames = NULL,
    file = NULL)
}
\arguments{
  \item{species}{a data.frame with two columns, \code{tvk}
  and \code{name}, containing TVKs and names for the
  species you want to model (see details).  These should be
  character columns, not factors!}

  \item{datasets}{a list of the dataset IDs (8-character
  strings) for datasets on the NBN Gateway from which you
  want data extracted. If this is not included, all
  publicly available data will be extracted.}

  \item{startYear}{the earliest year for which you wish to
  extract data. If this is not given, all publicly
  available data up to endYear will be extracted.}

  \item{endYear}{the latest year for which you wish to
  extract data. If this is not given, all publicly
  available data from startYear will be extracted.}

  \item{gridSystem}{this can be \code{OSGB} (default) or
  \code{OSNI}}

  \item{units}{the units in which you wish to output
  coordinates. Can be \code{km} (default) or \code{m}.}

  \item{precision}{the precision of the coordinates to
  output (in metres). Set to 0 to use the original
  precision at which observations were made (see details).
  Default: 1000 (i.e. output 1km square coordinates).}

  \item{unique}{Logical TRUE (default) means output only
  the unique coordinates at the given precision. FALSE
  outputs one row for each available observation.}

  \item{includeNames}{Logical TRUE means include the
  species names in the output, FALSE only outputs the
  coordinates. This defaults to TRUE if the species
  parameter includes the name of more than one species,
  FALSE otherwise.}

  \item{file}{If supplied, the output will be written in
  CSV format to the file specified.}
}
\value{
  a data.frame with two columns \code{x} and \code{y} or
  three columns \code{name}, \code{x} and \code{y} if
  includeNames is TRUE,
}
\description{
  Get the data required to fit a Species Distribution Model
  for one or more species from the NBN Gateway. One element
  required by many modelling methods consists of the
  coordinates of the locations at which a species has been
  observed. This is often supplied in the form of a CSV
  file with either the x,y coordinates for a particular
  species in two columns or, if the method can fit a
  sequence of species in one run, then species name,x,y in
  three columns. If modelling is being done via an R
  package such as dismo, then the coordinates are generally
  supplied as x,y columns in a matrix or data.frame.
  \bold{This function assumes that you are fitting a model
  using coordinates of the National Grid for either GB or
  Ireland}.
}
\details{
  \subsection{Specifying species}{The \code{species}
  parameter has to cope with the need to agreggate data
  held under different TVKs. For example, \emph{Volucella
  bombylans} (TVK = NBNSYS0000007094) occurs in several
  named forms and some records on the NBN Gateway are
  associated with TVK: NBNSYS0000172195 (\emph{V.
  bombylans} form \emph{plumata}). For the purposes of
  modelling the distribution of the species, this
  distinction is irrelevant. We want all observations under
  both TVKs and we want to use the name \emph{Volucella
  bombylans} for all of them. This can be acheived by
  supplying a data.frame with a row for each TVK, but with
  the same species name:\cr\cr \code{sp <-
  data.frame(tvk=c("NBNSYS0000007094","NBNSYS0000172195"),
  name="Volucella bombylans", stringsAsFactors =
  FALSE)}\cr\cr If passed to the function's species
  parameter, this will extract records under both TVKs, but
  aggregate the data under the same species name.}

  \subsection{Precision of output coordinates}{The
  \code{precision} parameter controls the precision at
  which the coordinates are output. Most observations on
  the NBN Gateway are associated with a grid reference for
  the location where the observation was made. Grid
  references describe a square whose size depends on the
  number of digits used. For example, \code{TL29} is a 10
  x10km square, whilst \code{TL292988} is a 100 x 100m
  square. If the \code{precision} parameter is zero, then
  coordinates at the original precision implied by the grid
  reference will be output. That is the coordinate at the
  bottom, left-hand corner of the given grid square will be
  used. If a non-zero value (in meteres) is given for
  \code{precision}, then coordinates of the bottom,
  left-hand corner of each grid ref will be rounded to the
  value given. For example, the coordinates of the bottom,
  left-hand corner of \code{TL292988} are 529200, 298800 in
  metres, so a \code{precision} of 1000 will result in them
  being rounded to 529000,298000 (or 529,298 in km). In
  this case, if the original grid reference was the 10km
  square, e.g. \code{TL29} then its precision is 10,000m
  and it cannot be rounded to 1,000m and so this
  observation will not be included in the output.}
}
\examples{
\dontrun{
sp <- data.frame(tvk=c("NBNSYS0000007094","NBNSYS0000172195"),
                 name="Volucella bombylans", stringsAsFactors = FALSE)
sdm <- getSDMdata(sp, datasets="SGB00001",
                  startYear="1990", endYear="2012",
                  includeNames=TRUE, file="c:/temp/vbomb.csv")
}
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{getFrescaloData}},
  \code{\link{getOccurrences}}
}

