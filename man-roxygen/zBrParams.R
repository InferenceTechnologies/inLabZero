#' @param zBr Vector. Breaks of a numeric \code{z} column, works in conjunction with \code{zBrMethod} 
#' setting. Single integer value determines number of breaks. Vector of \code{length>1} can be numerical,
#' character or POSIXct and represents interval breaking points. Special character values "min", "mean",
#' "median", "max" and "seq" can be used.
#' @param zBrMethod Character. Relevant for numeric \code{z} columns, works in conjunction with \code{zBr} setting. 
#' Split methods:
#' \itemize{
#'   \item{ "median" }{ Breaks \code{z} column with \code{median}. }
#'   \item{ "mean" }{ Breaks \code{z} column with \code{mean}. }
#'   \item{ "thr" }{ Breaks \code{z} column with threshold value specified by \code{zBr}. }
#'   \item{ "yieldHeat" }{ Corresponds to \code{zBr=c("seq(min, median, length.out=brValue)", "max")}, 
#'    where \code{brValue} is the value specified by \code{zBr}. }
#'   \item{ "linear" }{ Breaks \code{z} column into equidistant intervals, number of splits is given
#'    by \code{zBr}. }
#'   \item{ "hist" }{ Breaks \code{z} column using graphic's \code{hist} function. Values
#'    are passed to hist's \code{breaks} parameter using \code{zBr}. }
#' } 
