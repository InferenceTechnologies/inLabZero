% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfGraphics.R
\name{defBayesPlot}
\alias{defBayesPlot}
\title{Naïve Bayes Model Plot}
\usage{
defBayesPlot(
  obj,
  steps,
  bins = "all",
  defCount = 0:3,
  chart = "prob",
  main = NULL,
  showLegend = TRUE,
  diePerc = TRUE,
  diePercDefCount = defCount[-1],
  dieDenom = NULL,
  priorIndices = list(TRUE),
  varCol = "ALL",
  plot = TRUE,
  core = length(steps),
  legendText = NULL,
  priorCoef = 1,
  probData = NULL,
  laplace = 0,
  verbose = TRUE
)
}
\arguments{
\item{obj}{`semiTable` object representing defects per die dataset.}

\item{steps}{Character. Defines manufacturing steps with correct order.}

\item{bins}{Character, Numeric. Specifies response bin or bins. Use keyword "all" for all the bad dice (default: "all").}

\item{defCount}{Integer. Defines range of defects per die cases. Any higher defects per die cases are always included
in the highest specified case (default: 0:3).}

\item{chart}{Character. Specifies chart type: "prob" (default) for viewing conditional probability of a bad die 
or "yield" for depiction of yield loss percentage.}

\item{main}{Character. Main title.}

\item{showLegend}{Logical. If \code{TRUE} legend is displayed.}

\item{diePerc}{Logical. If \code{TRUE} prevalence percentages of corresponding defects per die cases are shown along the lines (default: \code{TRUE}).}

\item{diePercDefCount}{Integer. Specifies which prevalence percentages are shown along the lines (default: \code{defCount[-1]}).}

\item{dieDenom}{Integer. User defined dice count denominator (default: \code{NULL}).}

\item{priorIndices}{List. User defined indices for prior calculation (default: list(\code{TRUE})).}

\item{varCol}{Character. Column carrying input variable. Can be used to specify defect type (default: "ALL").}

\item{plot}{Logical. If \code{FALSE}, table of bad die probability for given defect count, prior probability and
percentage of yield loss is returned instead of the plot (default: \code{FALSE}).}

\item{core}{Integer. Specifies number of cores for parallel computation.}

\item{legendText}{Character. User defined legend (default: \code{NULL}).}

\item{priorCoef}{Numeric. User defined multiplicative coefficients for prior probabilities (default: 1).}

\item{probData}{Probability data from previous run of \code{defBayesPlot} with \code{plot=FALSE} (default: \code{NULL}).}

\item{laplace}{Numeric. Laplace regularization term for the naïve Bayes model (default: 0).}

\item{verbose}{Logical. If \code{FALSE} turns off function messages (default: \code{TRUE}).}
}
\value{
Plot or probability data.
}
\description{
Plots conditional probabilities from naïve Bayes model of die failure given defects per die count.
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
