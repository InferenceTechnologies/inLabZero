#' @examples
#'
#' data(pcy)
#'
#' pcy <- model(pcy, type="class", model="forest", splitRatio=1, formula="ProbeYield~.")
#' 
#' # Distribution shifting and narrowing visualization
#' distrShiftPlot(pcy, PARAM01, xCenter=22, xNarrow=0.5)
#' \dontrun{
#' # Distribution shifting and narrowing, browse through centers
#' distrShiftPlot(pcy, PARAM01, xCenter=c(22,23), xNarrow=0.5)
#' }
#' 
#' \dontrun{
#' # Select numerical z variable with input by index
#' distrShiftPlot(pcy, PARAM01, xCenter=22, z=5)
#' # Select categorical z variable with input by name
#' distrShiftPlot(pcy, 1, xCenter=22, z="part", zLevel=c("PART1", "PART2"))
#' }
#' # Special keyword "all" for all levels
#' distrShiftPlot(pcy, 1, xCenter=22, z="part", zLevel="all")
#'
#' # Print all visual parameters
#' distrShiftPlotPar()
#' # Get distribution's colors
#' distrShiftPlotPar(colors.distr)
#' # Set legend position to bottomleft
#' distrShiftPlotPar(legend.pos="bottomleft")
#'
#' # Reload default visual parameters
#' distrShiftPlotPar(default)
