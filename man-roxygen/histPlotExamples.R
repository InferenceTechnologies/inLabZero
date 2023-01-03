#' @examples
#'
#' data(pcy)
#'
#' # Plot first variable from the group
#' histPlot(pcy, 1)
#' 
#' \dontrun{# Various ways to input variables for plotting 
#' # Use console for browsing
#' var <- 10
#' histPlot(pcy, 1, c(1,5), c(1:5), c(var), c("PARAM03"), 1:5, 8:-1, PARAM01, PARAM08:last)
#' }
#' # Select numerical z variable with input by index
#' histPlot(pcy, UPYield, z=5)
#' # Select categorical z variable with input by name
#' histPlot(pcy, 1, z="part", zLevel=c("PART1", "PART2"))
#' # Special keyword "all" for all levels
#' histPlot(pcy, 1, z="part", zLevel="all")
#' # Select date type z variable
#' histPlot(pcy, 1, z="pcStartDate", zBr=as.POSIXct(c("2015/10/01", "2016/01/01", "2016/10/01")))
#'
#' # Filter outliers
#' histPlot(pcy, UPYield, outlier=10, z=5, zOutlier=3)
#' 
#' # Break numerical z variables into intervals
#' # No break - single color mode
#' histPlot(pcy, 1, z=NULL)
#' # No break
#' histPlot(pcy, 1, zBr=1)
#' # Breaking by median, mean and yieldHeat method
#' histPlot(pcy, 1, zBrMethod="median")
#' histPlot(pcy, 1, zBrMethod="mean")
#' histPlot(pcy, 1, zBr=5, zBrMethod="yieldHeat")
#' # br keywords seq, min, median, mean and max together with values
#' histPlot(pcy, 10, chart="hist", zBr=c("min", "seq(20, 80, length.out=3)", "max"))
#' # Equivalent for zBrMethod="yieldHeat" using zBr keywords
#' histPlot(pcy, 1, zBr=c("seq(min, median, length.out=5)", "max"))
#' 
#' # Categorical x variable
#' histPlot(pcy, wafer, xLevel="all", z=NULL)
#' 
#' # Hide legend
#' histPlot(pcy, 1, showLegend=FALSE)
#'
#' # Print all visual parameters
#' histPlotPar()
#' # Get value of a visual parameter
#' histPlotPar(colors.zBr)
#' # Set breaking colors to red and green, transparency to zero and legend position to 
#' # top left
#' histPlotPar(colors.zBr=c("red", "green"), colors.alpha.density=1, legend.pos="topleft")
#'
#' # Reload default visual parameters
#' histPlotPar(default)
