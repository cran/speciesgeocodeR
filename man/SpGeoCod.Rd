\name{SpGeoCod}
\alias{SpGeoCod}

\title{
Point to Polygon classification from Text Files
}
\description{
Wrapper around \code{\link{ReadPoints}} and \code{\link{SpGeoCodH}}, runs a complete speciesgeocoder analysis from text files as input.
}
\usage{
SpGeoCod(x, y, areanames, occ.thresh = 0, elevation, threshold)
}

\arguments{
  \item{x}{
a character string giving the path to a text file containing the coordinates of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR a data.frame with three columns named identifier, XCOOR, YCOOR.
}
  \item{y}{
a character string giving the path to a text file containing the polygons of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR an object of the class SpatialPolygons OR an object of the class SpatialPolygonsDataframe.
}
  \item{areanames}{
character string.  If the input polygon file is a shape and not a text file, this column indicates the name of the column used for area names.  See the SpeciesgeocodeR documentation for more information, if the polygon input file is a text file leave as default (Default = ""). 
}
  \item{occ.thresh}{
numerical. Threshold for the minimum number of occurrences necessary in a polygon to be counted as present (in percent of the total occurrences of the species).  Default = 0.
}
 \item{elevation}{
logical.  Should the results be split by elevation class? (default = FALSE)
}
  \item{threshold}{
numeric vector.  Indicating the elevation thresholds.
}
}

\value{
An object of the class spgeoOUT.
}
\note{
See the speciesgeocodeR documentation for further information and examples.
}
\seealso{
 \code{\link{ReadPoints}}, \code{\link{SpGeoCodH}}.
}
\examples{
data(lemurs)
data(mdg_poly)

outp <- SpGeoCod(lemurs, mdg_poly)
}

\keyword{ spatial }