% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfTools.R
\name{transfer}
\alias{transfer}
\title{Transfer Variables}
\usage{
transfer(
  from,
  to,
  col,
  filter = NULL,
  method = "majority",
  fromData = "par",
  toData = "par",
  key = "default",
  toDataType = NULL,
  toColNameBase = "map",
  verbose = TRUE
)
}
\arguments{
\item{from}{\code{semiTable} or \code{semiFrame} object.}

\item{to}{\code{semiTable} or \code{semiFrame} object.}

\item{col}{Character. Specifies column name or names to be transferred. A data type can be used for whole group transfer.
See \code{\link{dataType}} for more information on data types.}

\item{filter}{Filter used for conditioning values of transffered variables (default: \code{NULL}).}

\item{method}{Character. Specifies aggregation function. For example \code{majority} (default), 
\code{mean}, \code{median}, \code{max}, \code{min}.}

\item{fromData}{Character. Defines source data ("map" or "par") (default: "par").}

\item{toData}{Character. Defines target data ("map" or "par") (default: "par").}

\item{key}{Character. Specifies merging key for the variable transfer between different 
level frames (default: "default" - key will be chosen automatically as the highest intersect of granularities).}

\item{toDataType}{Character. Used for specifying target \code{dataType} in \code{from=="map" && to=="par"} scenarios.
See \code{\link{dataType}} for more information on data types (default: \code{NULL}).}

\item{toColNameBase}{Character. Used for specifying new column names in \code{from=="map" && to=="par"} scenarios (default: "map").}

\item{verbose}{Logical. If \code{FALSE} turns off function messages (default: \code{TRUE}).}
}
\value{
Copy of the \code{semiTable} or \code{semiFrame} object.
}
\description{
Transfers variables between different level frames.
}
\examples{

data(pcy)
data(wbm)

## Transfer to higher level 
# 1. Create a new cluster in wbm that will be used for transfer
wbm2 <- cluster(wbm, from=edge, to=newCluster)
# 2. Transfer cluster newCluster from wbm2 to pcy
transfer(wbm2, pcy, "newCluster")
# 3. Transfer requesting PCMYield below 80\%
transfer(wbm2, pcy, "newCluster", filter=pcy[,PCMYield]<80)

## Transfer to lower level
# 1. Create a new cluster in pcy2 that will be used for transfer
pcy2 <- cluster(pcy, from=edge, to=newPcyCluster)
# 2. Transfer cluster newPcyCluster from pcy2 to wbm
transfer(pcy2, wbm, "newPcyCluster", method="max")
# 3. Transfer mean for the PARAM62 from pcy to wbm
transfer(pcy, wbm, "PARAM62", method="mean")

}
\seealso{
\code{\link{dataType}} for available data types.
}
