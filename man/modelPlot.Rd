% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfGraphics.R
\name{modelPlot}
\alias{modelPlot}
\title{Model Plot}
\usage{
modelPlot(
  obj,
  chart = "default",
  main = NULL,
  color = "heat",
  data = "last",
  snip = FALSE,
  positiveIdx = 2,
  extra = 108,
  varlen = 0,
  faclen = 0,
  ylim = NULL,
  file = NULL,
  first = NULL,
  PComp = NULL,
  ...
)
}
\arguments{
\item{obj}{\code{semiTable} or \code{semiFrame} object.}

\item{chart}{Character. Type of visualization. 

CART (\code{cart}) model relevant types:
\itemize{
  \item{ "default" }{ Plots rpart decision tree. For more details see \code{?rpart.plot::prp} (default). }
  \item{ "plotmo" }{ Plots model's response when varying one or two predictors while
    holding the other predictors constant. For more details see \code{?plotmo::plotmo}. }
  \item{ "importance" }{ Plots predictors importance score calculated by mean gini decrease. }
}
Random forest (\code{forest}) model relevant types:
\itemize{
  \item{"default" }{ Plots predictors importance score calculated by mean gini decrease (default). }
  \item{ "plotmo" }{ Plots model's response when varying one or two predictors while
    holding the other predictors constant. For more details see \code{?plotmo::plotmo}. }
  \item{ "error" }{ Plots the error rates. }
}
XGBoost (\code{xboost}) model relevant types:
\itemize{
  \item{"default" }{ Plots predictors importance score (default). }
  \item{ "deepness" }{ Plots model's complexity. For more information see \code{?xgboost::xgb.plot.deepness}. }
  \item{ "trees" }{ Exports model's ensemble visualization diagram. For more information see \code{?xgboost::xgb.plot.multi.trees}. }
}
PCA (\code{pca}) model relevant types:
\itemize{
  \item{"default" }{ Plots percent of data variance explained by Principal Components (default). }
  \item{ "cor" }{ Plots rescaled loadings ie. correlations for a specified Principal Component. }
  \item{ "exp" }{ Plots squared eigenvector components ie. variables contribution to a specified Principal Component. }
}}

\item{main}{Character. Main title.}

\item{color}{Character. Color modes used in relevant chart types:
\itemize{
  \item{ "default" }{ Default coloring scheme. }
  \item{ "heat" }{ Heatmap coloring scheme (default). }
  \item{ "majority" }{ Binary response majority coloring scheme. }
}}

\item{data}{Character. A \code{semiFrame} object can store models on both parametric and map data. Choose
\itemize{
  \item{ "par" }{ for model on parameters. }
  \item{ "map" }{ for model on maps. }
  \item{ "last" }{ for last modified model. }
}}

\item{snip}{Logical. Allows to interactively trim the tree with mouse (default: \code{FALSE}).}

\item{positiveIdx}{Numeric. Relevant if \code{color="heat"}, determines whether maximum or minimum is assigned
to the green color. Possible values 1 or 2.}

\item{extra}{Numeric. Show extra information in the CART nodes. For more details see \code{?rpart.plot::prp} (default: 108).}

\item{varlen}{Integer. Length of variable names in text at the CART splits. For more details see \code{?rpart.plot::prp} (default: 0).}

\item{faclen}{Integer. Length of factor level names in the CART splits. For more details see \code{?rpart.plot::prp} (default: 0).}

\item{file}{Character. Passes file name for \code{xboost} \code{chart="trees"} export. If \code{NULL}
opens in browser directly (default: \code{NULL}).}

\item{first}{Integer. Specifies first n values for relevant charts to display (default: \code{NULL}).}

\item{PComp}{Integer. Specifies Principal Component, relevant for PCA models (default: \code{NULL}).}

\item{...}{Passes parameters to relevant rendering functions.}

\item{yLim}{Numeric. User defined y axis limits. Vector of length 2.}
}
\value{
If snip mode is enabled, object with snipped CART is returned. Plot otherwise.
}
\description{
Provides model visualizations.
}
\examples{
# modelPlot

data(pcy)

# Train Random forest classification
pcy <- model(pcy, type="class", model="forest", splitRatio=1, formula="ProbeYield~.")

# Default variable importance plot
modelPlot(pcy)
# Partial dependence with passed degree and y limit setting
modelPlot(pcy, chart="plotmo", degree1=c(1,3), degree2=FALSE, ylim=c(0,0.5))

# Train CART classification of 'ProbeYield' (median response split by default) on all parameters
pcy <- model(pcy, type="class", model="cart", splitRatio=1, formula="ProbeYield~.", cp=0.03)

# Default decision tree plot
modelPlot(pcy)
\dontrun{
# Hide extra information in nodes and activate snipping tool
modelPlot(pcy, snip=TRUE, extra=0)
}
# Partial dependence with passed degree and y limit setting
modelPlot(pcy, chart="plotmo", yLim=0.9, degree1=1, degree2=FALSE)

# Print all visual parameters
modelPlotPar()
# Get value of a visual parameter
modelPlotPar(colors)

# Default majority mode color scheme
modelPlot(pcy, color="majority")
# Set box color in majority mode
modelPlotPar(colors.cart.br2.box=c("red", "green"))
# Custom majority mode color scheme
modelPlot(pcy, color="majority")

# Reload default visual parameters
modelPlotPar(default)
}
