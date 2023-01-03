#' @param br Vector. Relevant for numeric \code{z} column, works in conjunction with \code{brMethod} 
#' setting. Single integer value determines number of splits. Vector of \code{length>1} can be numerical,
#' character or POSIXct and represents interval breaking points. Special character values \code{min}, \code{mean},
#' \code{median}, \code{max} and \code{seq} can be used.
#' @param brMethod Character. Relevant for numeric \code{z} columns, works in conjunction with \code{br} setting. 
#' Split methods:
#' \itemize{
#'   \item{ median }{ Splits \code{z} column with median. }
#'   \item{ mean }{ Splits \code{z} column with mean. }
#'   \item{ tresh }{ Splits \code{z} column with trehsold value specified by \code{br}. }
#'   \item{ yieldHeat }{ Corresponds to \code{br=c("seq(min, median, length.out=brValue)", "max")}, 
#'    where \code{brValue} is the value specified by \code{br} }
#'   \item{ linear }{ Splits \code{z} column into equidistant intervals, number of splits is given
#'    by \code{br}. }
#' } 
