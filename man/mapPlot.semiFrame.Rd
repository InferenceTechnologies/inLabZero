% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfGraphics.R
\name{mapPlot.semiFrame}
\alias{mapPlot.semiFrame}
\alias{mapPlot}
\title{Wafer Map Plot}
\usage{
\method{mapPlot}{semiFrame}(
  obj,
  ...,
  main = NULL,
  numMain = TRUE,
  subMain = NULL,
  clusterMain = TRUE,
  clusterThr = 0.5,
  edit = FALSE,
  showLegend = TRUE,
  method = "default",
  addPoints = NULL,
  mapAlpha = NULL,
  add = FALSE
)
}
\arguments{
\item{obj}{\code{semiFrame} object. Browsing mode is activated if object has 
multiple rows.}

\item{...}{Two possible actions:
\itemize{
  \item{ Map comparison: }{ Input arbitrary number of \code{semiFrame} objects. Maps from these 
    objects matching \code{lot} and \code{wafer} of the actually plotted map from 
    the obj \code{semiFrame} are plotted in a row. }
  \item{ Cluster browsing: }{ Input \code{cluster=clusterName}, which initiates browsing of the \code{clusterName} cluster. }
}}

\item{main}{Character. Main title.}

\item{numMain}{Logical. If TRUE index of plotted row is included into the main title.}

\item{subMain}{Character. Specifies column (columns) to be plotted below map. In cluster borwsing mode
cluster count is shown by default (default: NULL).}

\item{clusterMain}{Logical. If TRUE includes cluster name in the main title (default: TRUE).}

\item{clusterThr}{Numeric. Threshold for cluster membership.}

\item{edit}{Logical. If TRUE editing tools in the browsing mode are enabled and edited object
is returned upon browsing mode exit. See help in the browsing mode for more information (default: FALSE).}

\item{showLegend}{Logical. If \code{TRUE} legend is displayed.}

\item{method}{Advanced: Character. Determines wafer rendering method. "default" uses \code{inferenceLab}
package, "raster" uses \code{raster} package (default: "default").}

\item{addPoints}{Advanced: Matrix. Adds custom points to plot (default: NULL).}

\item{mapAlpha}{Advanced: Numeric. Adds transparency. Values from 0 to 1 (default: NULL).}

\item{add}{Advanced: Logical. Overlays map over current plot (default: FALSE).}
}
\value{
If edit mode is enabled, edited object is returned. Plot otherwise.
}
\description{
Plots wafer bin map or heatmap represented by a \code{semiFrame} row. 
In the WBM case colors correspond to bins and color sequence is 
based on bin occurrence on the wafer. In the heatmap case colors correspond
to bad die frequency.
}
\examples{

data(wbm)

# Plot a wbm corresponding to the fifth row
mapPlot(wbm[5])
# Plot a wbm specified by lot and wafer
mapPlot(wbm[lot=="LOT001"&wafer==12]) 

\dontrun{# Plot multiple maps consecutively
# Use console for browsing
mapPlot(wbm[1:10])
}
# Plot maps next to each other for comparison by matching lot and wafer
mapPlot(wbm[5], wbm) 

# Show probe date below and turn off legend
mapPlot(wbm[lotWafer=="LOT001-W12"], subMain = "probeEndDate", showLegend = FALSE) 

# Advanced: Add a point to wbm
mapPlot(wbm[40], addPoints=matrix(c(10,11,99), 1,3, byrow=TRUE))

# Print all visual parameters
mapPlotPar()

# Get chart margins
mapPlotPar(mar)

# Set chart margins
mapPlotPar(mar=c(5.2, 3.2, 4.2, 3.2))

# Set number of heatmap color levels
mapPlotPar(colors.n=5)

# Reload default visual parameters
mapPlotPar(default)
}
