#' @examples
#'
#' data(pcy)
#'
#' # Train Random forest classification
#' pcy <- model(pcy, type="class", model="forest", splitRatio=1, modelFormula="ProbeYield~.")
#' 
#' # Partial dependence method="model", default settings
#' partialPlot(pcy, PARAM01)
#' 
#' # Select numerical z variable with input by index
#' partialPlot(pcy, 1, z=5)
#' # Select categorical z variable with input by name
#' partialPlot(pcy, 1, z="part", zLevel=c("PART1", "PART2"))
#' # Special keyword "all" for all levels
#' partialPlot(pcy, 1, z="part", zLevel="all")
#' # Select date type z variable
#' partialPlot(pcy, 1, 10, z="pcStartDate", zBr=as.POSIXct(c("2015/10/01", "2016/01/01", "2016/10/01")))
#'
#' # Break numerical z variable into intervals
#' partialPlot(pcy, 1, method="model", z="ProbeYield", zBr=3, zBrMethod="yieldHeat")
#'
#' # Partial dependence method="data"
#' # Yield estimates after PARAM01's distribution shifting and narrowing by 50%
#' partialPlot(pcy, PARAM01, method="data", xInput=20:24, xNarrow=0.5)
#' # Yield estimates depending on z variable's distribution adjustments
#' partialPlot(pcy, PARAM01, method="data", xInput=5 ,xNarrow=0.5, z=5, zCenter=c(70,71), zNarrow=0.8)
#' 
#' # Print all visual parameters
#' partialPlotPar()
#' # Get confidence interval's color
#' partialPlotPar(colors.confInterv)
#' # Set legend position to bottomleft
#' partialPlotPar(legend.pos="bottomleft")
#' 
#' # Reload default visual parameters
#' partialPlotPar(default)
