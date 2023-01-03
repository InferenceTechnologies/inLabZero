#' @examples
#'
#' data(pcy)
#'
#' # Plot siteid per site with default heatmap color scheme.
#' sitePlot(pcy, siteid)
#' # Plot siteid per site with single color scheme.
#' sitePlot(pcy, siteid, color="single")
#'
#' \dontrun{# Various ways to input variables for plotting
#' # Use console for browsing
#' var <- 10
#' sitePlot(pcy, 1, c(1,5), c(1:5), c(var), c("siteid"), 1:5, 8:-1, siteid, PARAM01:last)
#' }
#' # Plot PCMYield mean
#' sitePlot(pcy, PCMYield, fun=mean)
#'
#' # Print all visual parameters
#' sitePlotPar()
#' 
#' # Get significant digits setting
#' sitePlotPar(text.signif)
#' # Set number of significant digits
#' sitePlotPar(text.signif=5)
#'
#' # Reload default visual parameters
#' sitePlotPar(default)
