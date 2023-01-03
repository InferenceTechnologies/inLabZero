#' @examples
#'
#' data(pcy)
#'
#' # Plotting correlation matrix
#' corPlot(pcy)
#' 
#' # Plot dendogram with custom grouping threshold
#' corPlot(pcy, chart="dend", dendThr=0.95)
#' 
#' # Plot upper triangle correlation matrix and show correlations above 0.5 only
#' corPlot(pcy, chart="mat", matLimit=0.5, matType="upper")
#' 
#' # Use single linkage for dendrogram
#' corPlot(pcy, chart="dend", hclustLinkage="single")
#'
#' # Print all visual parameters
#' corPlotPar()
#' # Get grouping rectangle color
#' corPlotPar(dend.thrColor)
#' # Set grouping rectangle color
#' corPlotPar(dend.thrColor="green")
#' corPlot(pcy, chart="dend", main="Threshold color changed")
#'
#' # Reload default visual parameters
#' corPlotPar(default)
