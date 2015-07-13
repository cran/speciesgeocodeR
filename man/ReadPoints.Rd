\name{ReadPoints}
\alias{ReadPoints}

\title{
Loading Data into SpeciesgeocodeR
}
\description{
Create an object of the class spgeoIN from the input arguments.
}
\usage{
ReadPoints(x, y, areanames = NA, verbose = FALSE)
}

\arguments{
  \item{x}{
can be a data.frame with three columns named identifier, XCOOR, YCOOR or a character string.  If x is a character string it can be the path to a tab-delimited text file containing the coordinates of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR a vector of species names.   In the latter cases GBIF is searched for occurrence data using the rgbif function of the rgbif package.  NO data cleaning is performed.
}
  \item{y}{
string giving the path to a text file containing the polygons of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR an object of the class \code{\link{SpatialPolygons}} OR an object of the class
 \code{\link{SpatialPolygonsDataFrame}}.
}
  \item{areanames}{
character string.  If the polygons in the spgeoIN object were derived from a shape file or a SpatialPolygonsDataframe, this column indicates the name of the column used for area names.   See the Speciesgeocoder documentation for more information.  If the polygons in the spgeoIN object were derived from a text file leave as default (Default = NA).
}
  \item{verbose}{
logical. If TRUE the functions report the progress of the analyses. This is advisable for large datasets.
}
}
\value{
An object of class spgeoIN.
}
\note{
See the speciesgeocodeR documentation for further information and examples.
}
\seealso{
 \code{\link{lemurs_in}}
}
\examples{
data(lemurs)
data(mdg_poly)

inp <- ReadPoints(lemurs, mdg_poly)
}

\keyword{ IO }