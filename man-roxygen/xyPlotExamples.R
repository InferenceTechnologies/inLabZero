#' @examples
#'
#' data(pcy)
#'
#' # Plot 1st vs 5th variable from the group
#' xyPlot(pcy, 1, 5)
#' 
#' \dontrun{# Various ways to input variables for plotting, first variable is used for x axis 
#' # Use console for browsing
#' var <- 10
#' xyPlot(pcy, 1, c(1,5), c(1:5), c(var), c("PARAM03"), 1:5, 8:-1, PARAM01, PARAM08:last)
#' }
#' # Select numerical z variable with input by index
#' xyPlot(pcy, 5, 10, z=8)
#' # Select categorical z variable with input by name
#' xyPlot(pcy, 2, 10, z="siteid", zLevel=c(1,5))
#' # Special keyword "all" for all levels
#' xyPlot(pcy, 2, 10, z="siteid", zLevel="all")
#' # Select date type z variable
#' xyPlot(pcy, 1, 10, z="pcStartDate", zBr=as.POSIXct(c("2015/10/01", "2016/01/01", "2016/10/01")))
#' 
#' # Break numerical variables into intervals
#' # No break
#' xyPlot(pcy, 1, 10, zBr=1)
#' # br keywords seq, min, median, mean and max together with values
#' xyPlot(pcy, 1, 10, zBr=c("min","5.6", "seq(20, 80, length.out=3)", "max"))
#'
#' # Add linear fit line
#' xyPlot(pcy, 2, 10, linFit=TRUE)
#' 
#' # Change draw order of points
#' xyPlot(pcy, 1, 10, zBr=2, drawOrder=c(2,1))
#' 
#' # Hide legend
#' xyPlot(pcy, 1, 10, main="Custom main", showLegend = FALSE)
#' 
#' # Print all visual parameters
#' xyPlotPar()
#' # Get value of a visual parameter
#' xyPlotPar(colors.zBr)
#' # Set breaking colors to red and green and legend position to top left
#' xyPlotPar(colors.zBr=c("red", "green"), legend.pos="topleft")
#'
#' # Reload default visual parameters
#' xyPlotPar(default)
