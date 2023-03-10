% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfGraphics.R
\name{corPlotPar}
\alias{corPlotPar}
\title{Correlation Plot Parameters}
\usage{
corPlotPar(...)
}
\arguments{
\item{...}{Numeric, character or function. 

Root parameters
\itemize{
 \item{ \code{mat} }{ Correlation matrix parameters. }
 \item{ \code{dend} }{ Correlation dendrogram parameters. }
}
\code{mat} parameters:
\itemize{
  \item{ mat.tl.cex }{ Text magnification (default: 0.55). }
  \item{ mat.tl.color }{ Text color. }
  \item{ mat.gridColor }{ Grid color. }
  \item{ mat.mar }{ Chart margin (default: 0, 0, 2, 0). }
}
\code{dend} parameters:
\itemize{
  \item{ dend.cex }{ Text magnification (default: 0.7). }
  \item{ dend.thrColor }{ Color of grouping rectangles (default: "red"). }
  \item{ dend.mar }{ Chart margin (default: 5.1, 4.1, 4.1, 2.1). }
}}
}
\description{
Gets, sets and reloads default visual parameters of corPlot.
}
\details{
Use parameter names to get values, use assignment to set parameter
values and use keyword \code{default} to load default values.
}
\examples{

data(pcy)

# Plotting correlation matrix
corPlot(pcy)

# Plot dendogram with custom grouping threshold
corPlot(pcy, chart="dend", dendThr=0.95)

# Plot upper triangle correlation matrix and show correlations above 0.5 only
corPlot(pcy, chart="mat", matLimit=0.5, matType="upper")

# Use single linkage for dendrogram
corPlot(pcy, chart="dend", hclustLinkage="single")

# Print all visual parameters
corPlotPar()
# Get grouping rectangle color
corPlotPar(dend.thrColor)
# Set grouping rectangle color
corPlotPar(dend.thrColor="green")
corPlot(pcy, chart="dend", main="Threshold color changed")

# Reload default visual parameters
corPlotPar(default)
}
