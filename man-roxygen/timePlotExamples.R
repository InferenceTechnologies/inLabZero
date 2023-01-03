#' @examples
#' 
#' data(pcy)
#' 
#' # Plot first variable from the group
#' timePlot(pcy, 1)
#' 
#' \dontrun{# Various ways to input variables for plotting 
#' # Use console for browsing
#' var <- 10
#' timePlot(pcy, 1, c(1,5), c(1:5), c(var), c("PARAM03"), 1:5, 8:-1, PARAM01, PARAM08:last)
#' }
#' # Select numerical z variable with input by index
#' timePlot(pcy, UPYield, z=1)
#' # Select categorical z variable with input by name
#' timePlot(pcy, 1, z="siteid", zLevel=c(1,5), chart="r")
#' # Special keyword "all" for all levels
#' timePlot(pcy, 1, z="siteid", zLevel="all", chart="r")
#' # Select date type z variable
#' timePlot(pcy, 1, z="pcStartDate", zBr=as.POSIXct(c("2012/01/01", "2013/01/01", "2014/01/01")))
#'
#' # Filter outliers
#' timePlot(pcy[pcStartDate>"2014/01/01"], UPYield, outlier=10, z=1, zOutlier=3, chart="box")
#' 
#' # Plot points as a sequence
#' timePlot(pcy, UPYield, zBr=5, sequence=TRUE)
#' 
#' # Break numerical variables into intervals
#' # No break
#' timePlot(pcy, 1, zBr=1)
#' # Breaking by median
#' timePlot(pcy, 1, zBrMethod="median")
#' # br keywords seq, min, median, mean and max together with values
#' timePlot(pcy, 1, zBr=c("min","5.6", "seq(20, 80, length.out=3)", "max"))
#'
#' # Plot rolling average with rolling window length of 30
#' timePlot(pcy, 1, smooth="r", rWin=30)
#' # Plot rolling median with center alignment
#' timePlot(pcy, 1, chart="r", rWin=30, rFun="median", rAlign="center")
#' # Add polynomial fit 
#' timePlot(pcy, 1, smooth="pol", polDegree=3)
#' # Plot polynomial fit line only 
#' timePlot(pcy, 1, chart="pol", polDegree=3)
#' 
#' # Use different y axes for different variables and with custom y limits
#' timePlot(pcy, 1, 2, chart="r", yAxisEach=TRUE, yLim=c(20,24), yLimRight=c(550,650)) 
#'
#' # Logarithmic scale of the y axis
#' timePlot(pcy, 1, chart="p", yLog=TRUE, main="Log y axis")
#'
#' # Hide legend
#' timePlot(pcy, 1, showLegend=FALSE)
#' 
#' # Change draw order of points
#' timePlot(pcy, 2, drawOrder=c(2,1))
#' 
#' # Print all visual parameters
#' timePlotPar()
#' # Get value of a visual parameter
#' timePlotPar(colors.zBr)
#' # Set breaking colors to red and green and legend position to top left
#' timePlotPar(colors.zBr=c("red", "green"), legend.pos="topleft")
#'
#' # Reload default visual parameters
#' timePlotPar(default)
