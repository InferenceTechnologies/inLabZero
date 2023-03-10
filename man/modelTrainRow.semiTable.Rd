% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semiFrame.R
\name{modelTrainRow.semiTable}
\alias{modelTrainRow.semiTable}
\alias{modelTrainRow}
\alias{modelTrainRow.semiFrame}
\title{Get Model Training Rows}
\usage{
\method{modelTrainRow}{semiTable}(obj)

\method{modelTrainRow}{semiFrame}(obj, data = "last")
}
\arguments{
\item{obj}{\code{semiTable} or \code{semiFrame} object.}

\item{data}{Character. A \code{semiFrame} object can store models on both parametric and map data. Choose
\itemize{
  \item{ "par" }{ for model on parameters. }
  \item{ "map" }{ for model on maps. }
  \item{ "last" }{ for last modified model. }
}}
}
\value{
Rows used for the training of a model.
}
\description{
Gets data rows employed in model training using the \code{\link{model}} function.
Useful for train-set reconstruction in train/test splits and response balancing cases.
}
\examples{

data(pcy)

# Train a CART model
pcy2 <- model(pcy, type="class", model="cart", splitRatio=0.1)

# 10\% of the data used for training
length(modelTrainRow(pcy2))

}
