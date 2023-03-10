% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfGraphics.R
\name{defBayesPlotPar}
\alias{defBayesPlotPar}
\title{Bayes Defectivity Plot Parameters}
\usage{
defBayesPlotPar(...)
}
\arguments{
\item{...}{Numeric, character or function. 

Root parameters:
\itemize{
  \item{ \code{level} }{ Color scheme. }
  \item{ \code{lwd} }{ Line width (default: 2.5). }
  \item{ \code{text} }{ Text parameters. }
  \item{ \code{legend} }{ Legend parameters. }
  \item{ \code{color} }{ Color parameters. }
  \item{ \code{las} }{ Style of axis labels: parallel (=0), perpendicular (=2), horizontal (=1) (default: 1). }
}
\code{text} parameters:
\itemize{
  \item{ \code{text.signif} }{ Number of significant digits (default: 2).}
  \item{ \code{text.pos} }{ Text relative position. Defaults to 1 i.e. below coordinates. }
  \item{ \code{text.bg} }{ Text background color (default: "white").}
}
\code{legend} parameters:
\itemize{
  \item{ \code{legend.pos} }{ Legend position (default: "topleft").}
}
\code{color} parameters:
\itemize{
  \item{ \code{color.yield.alpha} }{ Transparency setting for the "yield" chart type (default: 0.8).}
}}
}
\description{
Gets, sets and reloads default visual parameters of defBayesPlot.
}
\details{
Use parameter names to get values, use assignment to set parameter
values and use keyword \code{default} to load default values.
}
\examples{

# Defects dataset
data(def)
# Defect maps dataset
data(dem)
# Defects per die dataset
ded <- defDie(wbm, dem, def=def)

defBayesPlot(ded, steps=c("STEP1", "STEP2"))

# Print all visual parameters
defBayesPlotPar()

# Set legend position to bottomleft
defBayesPlotPar(legend.pos="bottomleft")

# Reload default visual parameters
defBayesPlotPar(default)
}
