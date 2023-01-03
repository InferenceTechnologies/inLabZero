#' @examples
#'
#' data(pcy)
#' 
#' # Plot a numerical vs a categorical variable from the group
#' boxPlot(pcy, siteid, 1, xLevel="all")
#' 
#' # Plot a numerical vs a numerical variable from the group with default breaking
#' boxPlot(pcy, 2, 3)
#' 
#' \dontrun{# Various ways to input variables for plotting, first variable is used for x axis 
#' # Use console for browsing
#' var <- 10
#' boxPlot(pcy, 2, c(2,5), c(2:5), c(var), c("PARAM03"), 2:5, 8:-1, PARAM02, PARAM08:last)
#' }
#' # Break numerical x variable into arbitrary intervals
#' boxPlot(pcy, 2, 3, xBr=10)
#' 
#' # Use z variable for boxes coloring
#' boxPlot(pcy, 2, 3, z="ProbeYield", zBr=10)
#' 
#' # Filter outliers
#' boxPlot(pcy, wafer, UPYield, outlier=3)
#' 
#' # Print all visual parameters
#' boxPlotPar()
#' # Get chart margins
#' boxPlotPar(mar)
#' # Set relative width of the boxes
#' boxPlotPar(boxWex=0.2)
#' # Boxes width proportional to number of observations in the groups
#' boxPlotPar(varWidth=TRUE)
#' # Set legend position to top left
#' boxPlotPar(legend.pos="topleft")
#' # Reload default visual parameters
#' boxPlotPar(default)
