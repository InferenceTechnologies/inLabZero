% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfTools.R
\name{outlierFilter}
\alias{outlierFilter}
\title{Outliers' Filtering}
\usage{
outlierFilter(obj, col, k = 3, output = "obj", colData = NULL)
}
\arguments{
\item{obj}{\code{semiTable} or \code{semiFrame} object.}

\item{col}{Character. Specifies single column for outlier filtering.}

\item{k}{Either a single value \code{kl=kr} or a vector of two values \code{c(kl, kr)}.
These \code{k} values are then used for the definition of outliers i.e. data points outside the interval:
\deqn{<(Q1 - kl * IQR), (Q3 + kr * IQR)>,} where \code{Q1} and \code{Q3} are quartiles,
\code{IQR} is the interquartile range (default: 3).}

\item{output}{Character. Possible values:
\itemize{ 
  \item{ "obj" }{ Returns copy of \code{obj} without outlier rows.} 
  \item{ "data" }{ Returns column data with outliers as \code{NAs}. } 
  \item{ "logical"}{ Returns logical vector with \code{FALSE} values indicating outliers. } 
  \item{ "tab" }{ Returns \code{tab} part of a \code{semiTable} or \code{semiFrame} object without outlier rows. } 
}}

\item{colData}{Internal. Direct data input (default: \code{NULL}).}
}
\value{
Copy of the \code{semiTable} or \code{semiFrame} object with filtered out outliers by default.
See parameter \code{output} for more options.
}
\description{
Filters outliers using IQR multiples.
}
\details{
For yield variables, all values below 0 are set to 0 and all values above 100 are set to 100. 
          Then, remaining outliers are filtered.
}
\examples{

data(pcy)

# Filter out PARAM43 outliers and return copy of pcy
outlierFilter(pcy, "PARAM43", k = 2)

# Get column 'PARAM01' data with outliers denoted as NAs
outlierFilter(pcy, "PARAM01", k = 3, output="data")

}
