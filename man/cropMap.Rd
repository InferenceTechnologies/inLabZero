% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semiFrame.R
\name{cropMap}
\alias{cropMap}
\title{Crop Map}
\usage{
cropMap(obj, mapBorder = NULL, core = 1, printProgress = 10, verbose = TRUE)
}
\arguments{
\item{obj}{\code{semiFrame} object.}

\item{mapBorder}{Numeric of length 4. Specifies map cropping (negative values) or extending (positive values). Elements correspond to the up, right, down and left direction (default: \code{NULL}).}

\item{core}{Integer. Specifies number of cores for parallel computation.}

\item{printProgress}{Integer. Defines the number of progress steps to be printed out (default: 10).}

\item{verbose}{Logical. If \code{FALSE} turns off function messages (default: \code{TRUE}).}
}
\value{
Copy of the \code{semiFrame} object with cropped (extended) maps.
}
\description{
Crops or extends maps for alignment purposes.
}
\examples{

data(wbm)

# Crop in all directions 
wbmCrop <- cropMap(wbm[1], mapBorder=rep(-3, 4))
# Extend in all directions
wbmExt <- cropMap(wbm[1], mapBorder=rep(3, 4))

# Compare maps
mapPlot(wbm[1], wbmCrop, wbmExt)

}
