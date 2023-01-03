#' @examples
#' # modelPlot
#' 
#' data(pcy)
#' 
#' # Train Random forest classification
#' pcy <- model(pcy, type="class", model="forest", splitRatio=1, formula="ProbeYield~.")
#' 
#' # Default variable importance plot
#' modelPlot(pcy)
#' # Partial dependence with passed degree and y limit setting
#' modelPlot(pcy, chart="plotmo", degree1=c(1,3), degree2=FALSE, ylim=c(0,0.5))
#' 
#' # Train CART classification of 'ProbeYield' (median response split by default) on all parameters
#' pcy <- model(pcy, type="class", model="cart", splitRatio=1, formula="ProbeYield~.", cp=0.03)
#' 
#' # Default decision tree plot
#' modelPlot(pcy)
#' \dontrun{
#' # Hide extra information in nodes and activate snipping tool
#' modelPlot(pcy, snip=TRUE, extra=0)
#' }
#' # Partial dependence with passed degree and y limit setting
#' modelPlot(pcy, chart="plotmo", yLim=0.9, degree1=1, degree2=FALSE)
#' 
#' # Print all visual parameters
#' modelPlotPar()
#' # Get value of a visual parameter
#' modelPlotPar(colors)
#' 
#' # Default majority mode color scheme
#' modelPlot(pcy, color="majority")
#' # Set box color in majority mode
#' modelPlotPar(colors.cart.br2.box=c("red", "green"))
#' # Custom majority mode color scheme
#' modelPlot(pcy, color="majority")
#'
#' # Reload default visual parameters
#' modelPlotPar(default)
