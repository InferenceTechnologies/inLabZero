#' @examples
#'
#' data(wbm)
#' 
#' # Overlay maps corresponding to rows 1-10 with default transparency
#' overlayMapPlot(wbm[1:10])
#' 
#' # Map from second argument is shaded in red
#' overlayMapPlot(wbm[1], wbm[2])
#' 
#' # Print all visual parameters
#' overlayMapPlotPar()
#'
#' # Get shading colors for multiple map objects regime
#' overlayMapPlotPar(colors)
#' 
#' # Set shading colors for multiple map objects regime
#' overlayMapPlotPar(colors=c("yellow", "blue", "black", "green", "red"))
#' 
#' # Reload default visual parameters
#' overlayMapPlotPar(default)
