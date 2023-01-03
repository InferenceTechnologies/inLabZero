#' @examples
#' 
#' data(wbm)
#' 
#' # Plot a wbm corresponding to the fifth row
#' mapPlot(wbm[5])
#' # Plot a wbm specified by lot and wafer
#' mapPlot(wbm[lot=="LOT001"&wafer==12]) 
#'
#' \dontrun{# Plot multiple maps consecutively
#' # Use console for browsing
#' mapPlot(wbm[1:10])
#' }
#' # Plot maps next to each other for comparison by matching lot and wafer
#' mapPlot(wbm[5], wbm) 
#' 
#' # Show probe date below and turn off legend
#' mapPlot(wbm[lotWafer=="LOT001-W12"], subMain = "probeEndDate", showLegend = FALSE) 
#'
#' # Advanced: Add a point to wbm
#' mapPlot(wbm[40], addPoints=matrix(c(10,11,99), 1,3, byrow=TRUE))
#'
#' # Print all visual parameters
#' mapPlotPar()
#' 
#' # Get chart margins
#' mapPlotPar(mar)
#' 
#' # Set chart margins
#' mapPlotPar(mar=c(5.2, 3.2, 4.2, 3.2))
#' 
#' # Set number of heatmap color levels
#' mapPlotPar(colors.n=5)
#'
#' # Reload default visual parameters
#' mapPlotPar(default)

