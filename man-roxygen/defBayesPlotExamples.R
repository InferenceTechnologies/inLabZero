#' @examples
#' 
#' # Defects dataset
#' data(def)
#' # Defect maps dataset
#' data(dem)
#' # Defects per die dataset
#' ded <- defDie(wbm, dem, def=def)
#' 
#' defBayesPlot(ded, steps=c("STEP1", "STEP2"))
#' 
#' # Print all visual parameters
#' defBayesPlotPar()
#' 
#' # Set legend position to bottomleft
#' defBayesPlotPar(legend.pos="bottomleft")
#' 
#' # Reload default visual parameters
#' defBayesPlotPar(default)
