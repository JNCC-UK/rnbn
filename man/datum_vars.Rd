\name{datum_vars}
\alias{datum_vars}
\docType{data}
\title{
A dataset containing the datum information for the 3 projections/datums used in the UK 
}
\description{
This dataset is used in the rnbn package to faciliate the conversions to latitude and longitude. 
}
\usage{data(datum_vars)}
\format{
  A data frame with 3 observations (one for each of datum used in the UK: OSGB, OSNI, UTM30) on the following 8 variables.
  \describe{
    \item{\code{Datum}}{a character vector giving the code used to represent that datum in the package (e.g. OSGB, OSNI, UTM30)}
    \item{\code{a}}{a numeric vector giving the semi-major axis}
    \item{\code{b}}{a numeric vector giving the semi-minor axis}
    \item{\code{F0}}{a numeric vector givign the scale factor on central meridian}
    \item{\code{lat0}}{a numeric vector giving true latitude of grid origin}
    \item{\code{lon0}}{a numeric vector giving true longitude of grid origin}
    \item{\code{N0}}{a numeric vector giving northing of true orgin in metres}
    \item{\code{E0}}{a numeric vector giving easting of true origin in metres}
  }
}
\details{
As mentioned above this dataset is used to facilitate the conversions required. Under most situations users need not ever directly interact with this dataset.
}

\keyword{
datasets
}