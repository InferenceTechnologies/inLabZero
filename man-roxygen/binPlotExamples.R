#' @examples
#' 
#' data(wbm)
#' 
#' # Plot bin pareto for all maps and all bins
#' binPlot(wbm)
#' 
#' # Plot bin pareto for all maps and bins 1, 2, 3
#' binPlot(wbm, bins=1:3)
#' 
#' # Browsing through the 'edge' cluster for all available bins
#' binPlot(wbm, bins="all", cluster="edge")
#' 
#' # Return yield loss contribution percentages table for all maps
#' binTab <- binPlot(wbm, plot=FALSE)$all
#' 
#' # Print all visual parameters
#' binPlotPar()
#' 
#' # Get bar border color
#' binPlotPar(colors.border)
#' 
#' # Set bar color to 'green'
#' binPlotPar(colors.box="green")
#'
#' # Reload default visual parameters
#' binPlotPar(default)
