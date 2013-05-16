\name{gridCoords}
\alias{gridCoords}
\title{Get x,y coordinates from a grid reference}
\usage{
  gridCoords(grid = NULL, units = c("km", "m"))
}
\arguments{
  \item{grid}{an OSGB or OSNI grid reference string}

  \item{units}{metres or kilometres (default)}
}
\value{
  a list of class "gridref" with the following contents:
  \item{grid}{the original grid reference}
  \item{system}{the grid reference system, either "OSGB" or
  "OSNI"} \item{x}{the x coordinate (easting) in requested
  units} \item{y}{the y coordinate (northing)in requested
  units} \item{units}{"m" or "km"} \item{precision}{the
  prcision of the original grid reference in metres}
}
\description{
  Given an OSGB or OSNI grid reference string, get the x,y
  coordinates of the OSGB or OSNI grid for the bottom,
  left-hand corner of the grid square. The \code{units}
  parameter controls the units (metres \code{m} or
  kilometres \code{km}) in which the coordinates should be
  returned.
}
\examples{
gridCoords("NZ1265") # returns x=412, y=565 (in km)
gridCoords("NZ123654", units="m") # returns x=412300, y=565400 (in m)
}
\author{
  Stuart Ball, JNCC \email{stuart.ball@jncc.gov.uk}
}
\seealso{
  \code{\link{gridRef}}
}

