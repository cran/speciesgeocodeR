\name{SpeciesGeoCoder}
\alias{SpeciesGeoCoder}

\title{
A Complete SpeciesgeocodeR Analysis
}
\description{
 Performs a complete analysis and produces a set of standard output files in the working directory.
}
\usage{
SpeciesGeoCoder(x, y, coex = FALSE, graphs = TRUE, areanames = "",
                occ.thresh = 0, elevation = FALSE, threshold,
		verbose = FALSE, ...)
}

\arguments{
  \item{x}{
can be a data.frame with three columns named identifier, XCOOR, YCOOR or a character string.  If x is a character string it can be the path to a tab-delimited text file containing the coordinates of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR a vector of species names.  In the latter cases GBIF is search for occurrence data using the \code{rgbif} function.  NO data cleaning is performed.
}
  \item{y}{
character string giving the path to a text file containing the polygons of points of interest (tab delimited, three columns with header: identifier, XCOOR, YCOOR) OR an object of the class SpatialPolygons OR an object of the class SpatialPolygonsDataframe.
}
  \item{coex}{
logical.  Indicating if a coexistence matrix should be calculated and be shown as a heat plot (default = FALSE). Only for small to medium-sized datasets.
}
  \item{graphs}{
logical.  Defines if the summary plots and maps are created as pdf files in the working directory (default = TRUE).
}
  \item{areanames}{
character string.  If the input polygon file is a shape and not a text file, this column indicates the name of the column used for area names.  See the SpeciesgeocodeR documentation for more information, if the polygon input file is a text file leave as default (default = ""). 
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
  \item{verbose}{
logical.  If TRUE the functions report the progress of the analyses. This is advisable for large datasets.
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters (see par).
}
}
\details{
This performs a complete speciesgeocodeR analysis and produces the standard set of output files: 1. classification of all samples to a polygon, 2. Summary of species (identifier)
occurrence per polygon, 3. a table of samples that could not be classified to any of the input polygons, 4. a nexus-file, including the species classification, 5. a coexistence matrix, showing to which percentage species to co-occur in the input polygons, 5. a table giving species numbers per polygon. Furthermore produces a set of .pdf files in the output directory: 1. a bar chart showing the number of species per polygon, 2. a bar chart summarizing numbers of each species for each polygon, 3. a bar chart summarizing the occurrences in each polygon per species, 4. a map of all polygons with the points classified to them, colored for species (identifier), 5. a map of the occurrences of all species, a map showing all points used and all unclassified points in the geographic context, 6. a heat plot showing the co-occurrence patterns of all species in the polygons.  If elevation = T, the input dataset is split at the elevation(s) provided by threshold, and separate output files are generated for each elevation class.
}
\value{
A set of output tables (.txt) and graphics (.pdf).
}
\note{
See the speciesgeocodeR documentation for further information and examples.
}

\examples{
\dontrun{
data(lemurs)
data(mdg_poly)


SpeciesGeoCoder(lemurs, mdg_poly}
}

\keyword{ spatial }
\keyword{ IO }