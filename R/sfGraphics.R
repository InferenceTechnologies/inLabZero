
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# Graphics
# 
# 0.155.0
# 

#
# Imports
#

#
# Constants
#

sfCross <- list(img=diag(5), color=c("white", "red", "red"))
sfCross$img <- sfCross$img+sfCross$img[,ncol(sfCross$img):1]
sfCross$img <- "[<-"(matrix(0, 50, 50), 24:(23+nrow(sfCross$img)), 24:(23+ncol(sfCross$img)), value=sfCross$img)

sfBrowsingHelp <- list(default="  [enter] next\n  [num] go next 'num' steps\n  [-] go back\n  [-num] go 'num' steps back\n  [auto] auto browsing\n  [auto delay] auto browsing with specified 'delay'\n  [x] exit",
						mapPlot="\n\n  [add cluster] adds wafer to the 'cluster', creates 'cluster' if it doesn't exist\n  [move cluster] moves wafer to the 'cluster', creates 'cluster' if it doesn't exist\n  [del {cluster}] deletes wafer from all clusters or from the 'cluster' if specified\n  [remove cluster] removes 'cluster' completely\n  [summary] prints cluster members counts")

#
# PlotPar DB
#

# color transparency
colorAlpha <- function(colors, alpha=1, coef=1) {

	colorsAlpha <- character()
	for (color in colors) {
		code <- coef*col2rgb(color)/255
		colorsAlpha <- c(colorsAlpha, rgb(code[1], code[2], code[3], alpha))
	}
	
	return(colorsAlpha)
}

sfPar <- new.env()

# General Plot Settings
sfPar$all <- list(default=list(black="#556167"))
sfPar$all$default$par <- list(col=sfPar$all$default$black, col.axis=sfPar$all$default$black, col.lab=sfPar$all$default$black, col.main=sfPar$all$default$black, col.sub=sfPar$all$default$black, fg=sfPar$all$default$black, bty="o")
sfPar$all$default$parSave <- c("las", "cex.axis", "mar", "mai", "xpd", names(sfPar$all$default$par))

sfPar$all$user <- sfPar$all$default

# mapPlot
sfPar$map <- list(default=list(breaks=list(), colors=list(), frame=list()))
sfPar$map$default$breaks[["border"]] <- c(-5,-4,-2.1,-2, -1)
sfPar$map$default$breaks[[sfMapType$rbin]] <- function(rowTab, ...) { return(c(-1, rowTab[num>=0, num])) }
sfPar$map$default$breaks[[sfMapType$rdef]] <- function(n, ...) { return(c(-2, -1 ,0:(n-2), 10^10)) }
sfPar$map$default$breaks[[sfMapType$bin]] <- function(n, ...) { c(sfPar$map$default$breaks[["border"]], do.call(sfPar$map$default$breaks[[sfMapType$rbin]], list(n))[-1]) }
sfPar$map$default$breaks[[sfMapType$rbbin]] <- c(-1,0,1)
sfPar$map$default$breaks[[sfMapType$bbin]] <- c(sfPar$map$default$breaks[["border"]], sfPar$map$default$breaks[[sfMapType$rbbin]][-1]) 
sfPar$map$default$breaks[[sfMapType$rheat]] <- function(n, tr, ...)	{ 
																		breaks <- c(-1, tr+(0:(n-1))*(1-tr)/(n-1))
																		breaks[length(breaks)] <- 2
																		return(breaks)
																	}
sfPar$map$default$breaks[[sfMapType$heat]] <- function(n, tr, ...)	{ 
																		c(sfPar$map$default$breaks[["border"]],
																		do.call(sfPar$map$default$breaks[[sfMapType$rheat]], list(n, tr))[-1]) 
																	}							
sfPar$map$default$breaks[[sfMapType$rsite]] <- function(n, tr, ...)	{ 	
																		n <- n-1
																		breaks <- c(-1,0.95+0:n*1.1/n)
																		# breaks[length(breaks)] <- 3
																		return(breaks)
																	}
sfPar$map$default$breaks[[sfMapType$site]] <- function(n, tr, ...)	{ 
																		c(sfPar$map$default$breaks[["border"]],
																		do.call(sfPar$map$default$breaks[[sfMapType$rsite]], list(n, tr))[-1]) 
																	}	
sfPar$map$default$breaks[[sfMapType$rbsite]] <- c(-1,0.5,2.05)
sfPar$map$default$breaks[[sfMapType$bsite]] <- c(sfPar$map$default$breaks[["border"]], sfPar$map$default$breaks[[sfMapType$rbsite]][-1])
sfPar$map$default$breaks$tr <- 0.1


sfPar$map$default$colors$n <- 10
sfPar$map$default$colors$bin0 <- colorAlpha(RColorBrewer::brewer.pal(9,"Greys")[3], 0.6)
sfPar$map$default$colors[["border"]] <- c("black","grey","#737373", "white") 
sfPar$map$default$colors[[sfMapType$rbin]] <- function(rowTab) {  	
																	zeroColor <- nrow(rowTab[num==0])
																	rowTab <- rowTab[num>0]
																	rowTab <- rowTab[,c("row"):=list(.I)]
																	colors <- c(RColorBrewer::brewer.pal(11,"Spectral"),
																			RColorBrewer::brewer.pal(12,"Set3"))[1:nrow(rowTab)]
																	rowTab <- rowTab[order(-n)]
																	idx <- integer(nrow(rowTab))
																	for (i in 1:length(idx))  { idx[rowTab[,row][i]] <- i }
																	colors <- colors[idx]
																	if (zeroColor) { colors <- c(sfPar$map$user$colors$bin0, colors) }
																	return(colors)
																} 
sfPar$map$default$colors[[sfMapType$bin]] <- function(rowTab) { 
																c(sfPar$map$default$colors[["border"]],
																do.call(sfPar$map$default$colors[[sfMapType$rbin]], list(rowTab)))  
																} 
sfPar$map$default$colors[[sfMapType$rbbin]] <- c(sfPar$map$default$colors$bin0, "#800026") 
sfPar$map$default$colors[[sfMapType$rdef]] <- function(n) { 	
																if (n>9) { nPal <- 9; repLast <- n-nPal-1  }
																pal <- rev(RColorBrewer::brewer.pal(nPal,"YlOrRd"))
																pal <- c(pal, rep(pal[length(pal)], repLast))
																c("white", sfPar$map$default$colors$bin0, pal) } 
sfPar$map$default$colors[[sfMapType$bbin]] <- c(sfPar$map$default$colors[["border"]], sfPar$map$default$colors[[sfMapType$rbbin]])						
sfPar$map$default$colors[[sfMapType$rheat]] <- function(n) { c(sfPar$map$user$colors$bin0, RColorBrewer::brewer.pal(n-1,"YlOrRd"))	}	 
sfPar$map$default$colors[[sfMapType$heat]] <- function(n)	{ 
																c(sfPar$map$default$colors[["border"]],
																do.call(sfPar$map$default$colors[[sfMapType$rheat]], list(n))) 
															}							
sfPar$map$default$colors[[sfMapType$rsite]] <- function(n) { c(sfPar$map$default$colors$bin0, RColorBrewer::brewer.pal(n-1,"Greens")) }	 
sfPar$map$default$colors[[sfMapType$site]] <- function(n)	{ 
																c(sfPar$map$default$colors[["border"]],
																do.call(sfPar$map$default$colors[[sfMapType$rsite]], list(n))) 
															}							
sfPar$map$default$colors[[sfMapType$rbsite]] <- c(sfPar$map$default$colors$bin0, "#80b1d3")
sfPar$map$default$colors[[sfMapType$bsite]] <- c(sfPar$map$default$colors[["border"]], sfPar$map$default$colors[[sfMapType$rbsite]])

sfPar$map$default$frame[[sfMapType$bin]] <- FALSE
sfPar$map$default$frame[[sfMapType$bbin]] <- FALSE
sfPar$map$default$frame[[sfMapType$heat]] <- FALSE
sfPar$map$default$frame[[sfMapType$site]] <- FALSE
sfPar$map$default$frame[[sfMapType$bsite]] <- FALSE
sfPar$map$default$frame[[sfMapType$rbin]] <- TRUE
sfPar$map$default$frame[[sfMapType$rbbin]] <- TRUE
sfPar$map$default$frame[[sfMapType$rheat]] <- TRUE
sfPar$map$default$frame[[sfMapType$rsite]] <- TRUE
sfPar$map$default$frame[[sfMapType$rbsite]] <- TRUE
sfPar$map$default$frame[[sfMapType$rdef]] <- FALSE

sfPar$map$default$legend <- list(x=1.025, y=1.025, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)
sfPar$map$default$legend[[sfMapType$bin]] <- TRUE
sfPar$map$default$legend[[sfMapType$bbin]] <- TRUE
sfPar$map$default$legend[[sfMapType$heat]] <- FALSE
sfPar$map$default$legend[[sfMapType$site]] <- FALSE
sfPar$map$default$legend[[sfMapType$bsite]] <- FALSE
sfPar$map$default$legend[[sfMapType$rbin]] <- TRUE
sfPar$map$default$legend[[sfMapType$rbbin]] <- TRUE
sfPar$map$default$legend[[sfMapType$rheat]] <- FALSE
sfPar$map$default$legend[[sfMapType$rsite]] <- FALSE
sfPar$map$default$legend[[sfMapType$rbsite]] <- FALSE
sfPar$map$default$legend[[sfMapType$rdef]] <- TRUE

sfPar$map$default$subMainLine[["default"]] <- 1.5
sfPar$map$default$subMainLine[["raster"]] <- 2.5

sfPar$map$default$mar <- c(3.6, 3.1, 4.1, 3.1)

sfPar$map$user <- sfPar$map$default


# binPlot
sfPar$bin <- list(default=list(colors=list(), line=list()))
sfPar$bin$default$colors$box <- "#80b1d3"
sfPar$bin$default$colors$border <- colorAlpha(sfPar$bin$default$colors$dr$box, coef=0.9)
sfPar$bin$default$colors$line <- "#fb8072"
sfPar$bin$default$line$lwd <- 2
sfPar$bin$default$space <- 0.2

sfPar$bin$user <- sfPar$bin$default


# histPlot
sfPar$hist <- list(default=list(colors=list()))
sfPar$hist$default$colors$zLevel <- function(n) {
														pal <- c(RColorBrewer::brewer.pal(12,"Set3")[-2], RColorBrewer::brewer.pal(8,"Set2"))
														pal[c(4,3,1,5,2,6:length(pal))][1:n]
													}
sfPar$hist$default$colors$zBr <- function(n) {
														if (n>11) { n <- 11 }
														if (n==1) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(4,4)]
														} else if (n==2) {
															pal <- c(RColorBrewer::brewer.pal(9,"Greys")[7], RColorBrewer::brewer.pal(4,"RdYlGn")[4])
															pal[1] <- colorAlpha(pal[1],0.5)
														} else if (n==3) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(1,3,4)]
														} else {
															pal <- RColorBrewer::brewer.pal(n,"RdYlGn")
														}
														pal
													}
sfPar$hist$default$colors$border <- list(hist=0.9, density=0.9, beside=NA)
sfPar$hist$default$colors$alpha <- list(hist=0.5, density=0.5, beside=0.8, single=0.8)													
sfPar$hist$default$pbr <- list(hist=60, beside=30)
sfPar$hist$default$axes <- list(x=list(ticks=5, signif=3), y=list(type="perc", lab=list(count="Count", perc="%")))
sfPar$hist$default$space <- list(hist=0, beside=NULL, xLevel=0.5)
sfPar$hist$default$legend <- list(pos="topright", title=list(hist="Overlay", density="Overlay", beside="Beside"), signif=3, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)

sfPar$hist$user <- sfPar$hist$default

# timePlot
sfPar$time <- list(default=list(colors=list()))
sfPar$time$default$colors$zLevel <- sfPar$hist$default$colors$zLevel
sfPar$time$default$colors$line <- sfPar$hist$default$colors$zLevel
sfPar$time$default$colors$zBr <- sfPar$hist$default$colors$zBr
sfPar$time$default$colors$box <- list(fill="#D9D9D9", border="#525252")
sfPar$time$default$colors$alpha <- list(p=0.8)
sfPar$time$default$pch <- function(n) { rep(20,n) }
sfPar$time$default$cexAxis <- 1
sfPar$time$default$mgp <- c(2.5,1,0)
sfPar$time$default$ticks <- 5
sfPar$time$default$grid <- list(xy=c(FALSE, FALSE), color="grey", lty=3)
sfPar$time$default$lwd <- 3
sfPar$time$default$legend <- list(pos="topright", signif=3, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)

sfPar$time$user <- sfPar$time$default

# xyPlot
sfPar$xy <- list(default=list(colors=list()))
sfPar$xy$default$colors$zLevel <- sfPar$hist$default$colors$zLevel
sfPar$xy$default$colors$zBr <- sfPar$hist$default$colors$zBr
sfPar$xy$default$colors$alpha <- list(zLevel=0.8, zBr=1)
sfPar$xy$default$pch <- function(n) { rep(20,n) }
sfPar$xy$default$linFit <- list(color=colorAlpha("#525252",0.7), lwd=2)
sfPar$xy$default$legend <- list(pos="topright", signif=3, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)

sfPar$xy$user <- sfPar$xy$default

# modelPlot
sfPar$model <- list(default=list(heat=list(), colors=list(cart=list(default=list()))))
sfPar$model$default$heat$n <- 11
sfPar$model$default$heat$breaks <- function(n) { return(-0.05+ 0:n*1.1/n) }
sfPar$model$default$heat$colors <- function(n) { return(RColorBrewer::brewer.pal(n,"RdYlGn")) }
sfPar$model$default$heat$textColors <- function(n) { 	
														if (n==1) { return(RColorBrewer::brewer.pal(ceiling(3),"Greys")[3]) }
														tc <- rep(RColorBrewer::brewer.pal(ceiling(3),"Greys")[3], n)
														tc[c(1,2,n-1,n)] <- RColorBrewer::brewer.pal(ceiling(3),"Greys")[1]
														return(tc)
													}
sfPar$model$default$colors$cart$default$box <- 0
sfPar$model$default$colors$cart$default$border <- "black"
sfPar$model$default$colors$cart$br2$box <- c("#fdae61", "#a6d96a")
sfPar$model$default$colors$cart$br2$border <- sfPar$model$default$colors$cart$br2$box
sfPar$model$default$colors$dr$box <- "#80b1d3"
sfPar$model$default$colors$dr$border <- colorAlpha(sfPar$model$default$colors$dr$box, coef=0.9)
sfPar$model$default$colors$dr$line <- "#fb8072"
# sfPar$model$default$colors$cart$br2$box <- c("#f46d43", "#66bd63")
sfPar$model$default$cexAxis$boost <- 0.8
sfPar$model$default$cexAxis$dr <- 0.8

sfPar$model$user <- sfPar$model$default

# partialPlot
sfPar$partial <- list(default=list(colors=list()))
sfPar$partial$default$colors$confInterv <- colorAlpha("#525252", 0.1)
sfPar$partial$default$colors$zLevel <- sfPar$hist$default$colors$zLevel
sfPar$partial$default$colors$zBr <-	function(n) 	{	
														if (n>11) { n <- 11 }
														if (n==1) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(4,4)]
														} else if (n==2) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(1,4)]
														} else if (n==3) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(1,3,4)]
														} else {
															pal <- RColorBrewer::brewer.pal(n,"RdYlGn")
														}
														pal
													}
sfPar$partial$default$lwd <- 1.5
sfPar$partial$default$legend <- list(pos="topright", signif=3, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)

sfPar$partial$user <- sfPar$partial$default

# boxPlot
sfPar$box <- list(default=list(colors=list()))
sfPar$box$default$colors$zLevel <- sfPar$hist$default$colors$zLevel
sfPar$box$default$colors$zBr <- sfPar$partial$default$colors$zBr <- function(n) {
														if (n>11) { n <- 11 }
														if (n==1) {
															pal <- RColorBrewer::brewer.pal(8,"Set3")[c(5,5)]
														} else if (n==2) {
															pal <- c("#fdae61", "#a6d96a")
														} else if (n==3) {
															pal <- RColorBrewer::brewer.pal(4,"RdYlGn")[c(1,3,4)]
														} else {
															pal <- RColorBrewer::brewer.pal(n,"RdYlGn")
														}
														pal
													}
sfPar$box$default$colors$border <- "#737373"
sfPar$box$default$boxWex <- 0.5
sfPar$box$default$varWidth <- FALSE
sfPar$box$default$xSignif <- 3
sfPar$box$default$mar <- c(5.1, 4.1, 4.1, 2.1)
sfPar$box$default$axis <- list(x=list(las=0, cex=1, line=NA))
sfPar$box$default$legend <- list(pos="topright", signif=3, lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)

sfPar$box$user <- sfPar$box$default

# sitePlot
sfPar$site <- list(default=list(text=list()))
sfPar$site$default$text$cex <- 1.5
sfPar$site$default$text$pos <- 1
sfPar$site$default$text$offset <- 0.4
sfPar$site$default$text$flatPos <- 3
sfPar$site$default$text$flatOffset <- 0.5
sfPar$site$default$text$signif <- 3
sfPar$site$default$text$colors <- list()
sfPar$site$default$text$colors$single <- "black"
sfPar$site$default$text$colors$n <- 4
sfPar$site$default$text$colors$breaks <- function(n) { 
												n <- n-1	
												return(-0.05+ 0:n*1.1/n) 
											}
sfPar$site$default$text$colors$br <- function(n) {
												return(rev(RColorBrewer::brewer.pal(n+1,"Greys")[-1]))
											}

sfPar$site$user <- sfPar$site$default

# corPlot
sfPar$cor <- list(default=list(mat=list(), dend=list()))
sfPar$cor$default$mat$tl <- list(cex=0.55, color="#556167")
sfPar$cor$default$mat$gridColor <- "gray85"
sfPar$cor$default$mat$mar <- c(0,0,2,0)
sfPar$cor$default$dend$cex <- 0.7
sfPar$cor$default$dend$thrColor <- "red"
sfPar$cor$default$dend$mar <- c(5.1, 4.1, 4.1, 2.1)

sfPar$cor$user <- sfPar$cor$default

# distrShiftPlot
sfPar$distrShift <- list(default=list(colors=list()))
sfPar$distrShift$default$colors$alpha <- 0.7
sfPar$distrShift$default$colors$distr <- colorAlpha(c("#bdbdbd", "#8dd3c7"), sfPar$distrShift$default$colors$alpha)
sfPar$distrShift$default$colors$border <- colorAlpha(sfPar$distrShift$default$colors$distr, coef=0.9)
sfPar$distrShift$default$signif <- 3
sfPar$distrShift$default$pbr <- 60
sfPar$distrShift$default$space <- 0
sfPar$distrShift$default$legend <- list(pos="topright", lty=0, lwd=1, cex=0.9, bty="n", segLen=0, pch=15, ptCex=1.8)
sfPar$distrShift$default$zLegend <- list(pos="topleft")

sfPar$distrShift$user <- sfPar$distrShift$default

# overlayMapPlot
sfPar$overlayMap <- list(default=list())
sfPar$overlayMap$default$colors <- RColorBrewer::brewer.pal(9,"Set1")

sfPar$overlayMap$user <- sfPar$overlayMap$default

# defBayesPlot
sfPar$defBayes <- list(default=list())
sfPar$defBayes$default$lwd <- 2.5
sfPar$defBayes$default$text <- list(signif=2, pos=1, bg="white")
sfPar$defBayes$default$legend <- list(pos="topleft")
sfPar$defBayes$default$color <- list(prob=list(level=sfPar$hist$default$colors$zLevel), 
									yield=list(level=sfPar$hist$default$colors$zLevel, alpha=0.8),
									yieldProb=list(level=c("#d9d9d9", "#80b1d3", "#fb8072")))
sfPar$defBayes$default$las <- 1

sfPar$defBayes$user <- sfPar$defBayes$default

# defBayesOptimPlot
sfPar$defBayesOptim <- list(default=list(color=list()))
sfPar$defBayesOptim$default$color$yield <- "#80B1D3"
sfPar$defBayesOptim$default$color$prob <- function(n) { 
														n1 <- n%/%2
														n2 <- n-n1
														return(c(rev(RColorBrewer::brewer.pal(n1, "YlOrRd")),
																	RColorBrewer::brewer.pal(n2, "YlGn")))
													}

sfPar$defBayesOptim$user <- sfPar$defBayesOptim$default

# defInheritedPlot
sfPar$defInherited <- list(default=list())
sfPar$defInherited$default$level <- sfPar$hist$default$colors$zLevel
sfPar$defInherited$default$lwd <- 2.5
sfPar$defInherited$default$legend <- list(pos="bottomright")

sfPar$defInherited$user <- sfPar$defInherited$default

#
# PlotPar Functions
#

managePlotPar <- function(..., plotParType, checkChar=NULL) {

	plotParRoot <- plotParType
	plotParType <- paste0(plotParType, "$user")
	argsList <- eval(substitute(alist(...)))
	getList <- list()

	if (!length(argsList)) {
		plotParExpr <- paste0("names(",plotParType,")")
		rootName <- eval(parse(text=plotParExpr), envir=sfPar)	
		ncatn()
		for (i in seq_along(rootName)) {
			ncatn(paste0("$", rootName[i], " ..."), k2=2)
		}
		return(invisible())
	}

	existence <- function(plotParType, plotParExpr, nest=0) {
		
		plotParExprSpl <- strsplit(plotParExpr, "[.]")[[1]]
		plotParExprList <- paste0(plotParType, paste(paste0("[[\"",plotParExprSpl[1:(length(plotParExprSpl)-2*nest)],"\"]]"), collapse=""))
		if (nest) {
			plotParExprList <- paste0(plotParExprList, "$", paste(plotParExprSpl[(length(plotParExprSpl)-2*nest+1):length(plotParExprSpl)],collapse="."))
		}
		e <- try(eval(parse(text=paste0("is.null(",plotParExprList,")")), envir=sfPar), TRUE)
		if (inherits(e, "try-error")&&nest<2) {
			plotParExprList <- existence(plotParType, plotParExpr, nest=nest+1)
		} else {
			if (e) stop("Parameter does not exist")
		}

		return(plotParExprList)
	}

	for (i in 1:length(argsList)) {
		if (is.null(names(argsList[i]))||names(argsList[i])==character(1)) {
			if (argsList[[i]]=="default"||argsList[[i]]=="reset") {
				plotParExpr <- paste0(plotParType,"<-", plotParRoot, "$default")
				eval(parse(text=plotParExpr), envir=sfPar)
				return(invisible())
			}
			plotParExpr <- existence(plotParType, as.character(argsList[[i]]))
			getList[[as.character(argsList[[i]])]] <- eval(parse(text=plotParExpr), envir=sfPar)

		} else {
			val <- argsList[[i]]
			if (names(argsList[i])=="scheme") {
				plotParExpr <- paste0(plotParType,"<-", plotParRoot, "$",val)
				eval(parse(text=plotParExpr), envir=sfPar)
				return(invisible())
			}

			plotParExpr <- existence(plotParType, names(argsList[i]))

			if (!is.null(checkChar)) {
				if (names(argsList)[[i]]%in%names(checkChar)) {
					checkCharInput(argsList[[i]], checkChar[[names(argsList)[[i]]]], "values", inputName=names(argsList)[[i]])
				}
			}

			plotParExpr <- paste0(plotParExpr, "<-", deparse(substitute(val)))
			eval(parse(text=plotParExpr), envir=sfPar)
		}
	}

	if (length(getList)) { 
		listRecur <- function(lst) {

			lstPar <- list()
			for (i in seq_along(lst)) {
				if (is.list(lst[[i]])) {
					lstPar2 <- listRecur(lst[[i]])
					names(lstPar2) <- paste(names(lst)[i], names(lstPar2), sep=".")
					lstPar[names(lstPar2)] <- lstPar2
				} else {
					lstPar[[names(lst)[i]]] <- lst[[i]]
				}	

			}
			return(lstPar)
		}

		ncatn()
		return(listRecur(getList)) 
	}
	return(invisible())
}

#' General Plot Parameters
#' 
#' Gets, sets and resets global visual aspects of plots.
#' @param ... Numeric, character or function. 
#' Root parameters:
#' \itemize{
#'   \item{ \code{black} }{ Black color shade (default: "#556167"). }
#'   \item{ \code{par} }{ Graphical parameters to be set before plotting. }
#'   \item{ \code{parSave} }{ Graphical parameters to be saved during plotting. }
#' }
#' \code{par} parameters to be set before plotting:
#' \itemize{
#'   \item{ \code{par.col} }{ Default plotting color (default: "#556167"). }
#'   \item{ \code{par.col.axis} }{ Color for axis annotation (default: "#556167"). }
#'   \item{ \code{par.col.lab} }{ Color for x and y labels (default: "#556167"). }
#'   \item{ \code{par.col.main} }{ Color for titles (default: "#556167"). }
#'   \item{ \code{par.col.sub} }{ Color for subtitles (default: "#556167"). }
#'   \item{ \code{par.col.fg} }{ Plot foreground color (default: "#556167"). }
#'   \item{ \code{par.col.bty} }{ Color for type of box which is drawn around plots (default: "o"). }
#' }
#' \code{parSave} graphical parameters to be saved during plotting, by default these are:
#' \itemize{
#'   \item{ "las" }{ Style of axis labels: parallel(=0), perpendicular(=2), horizontal(=1). }
#'   \item{ "cex.axis" }{ Annotation magnificiation of axis. }
#'   \item{ "mar" }{ Chart margins. }
#'   \item{ "mai"}{ Margin size specified in inches. }
#'   \item{ "xpd" }{ If \code{FALSE}, all plotting is clipped to the plot region, if \code{TRUE}, all plotting is clipped to the figure region, and if \code{NA}, all plotting is clipped to the device region. }
#'   \item{ "col" }{ Default plotting color. }
#'   \item{ "col.axis" }{ Color for axis annotation. }
#'   \item{ "col.lab" }{ Color for x and y labels. }
#'   \item{ "col.main"}{ Color for titles. }
#'   \item{ "col.sub" }{ Color for subtitles. }
#'   \item{ "fg" }{ Plot foreground color. }
#'   \item{ "bty" }{ Color for type of box which is drawn around plots. }
#' }
#' 
#' @template parFunctionDetails
#' 
#' @examples
#' 
#' # Print all visual parameters
#' plotPar()
#' 
#' # Get value of a visual parameter
#' plotPar(black)
#' 
#' # Get value of a graphical parameters to be set before plotting
#' plotPar(par)
#' 
#' # Set axis color to red
#' plotPar(par.col.axis="red")
#' 
#' # Reload default visual parameters
#' plotPar(default)
#' 
#' @export
plotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="all"))
}


#' mapPlot Parameters
#'
#' @description Gets, sets and resets visual aspects of mapPlot.
#' 
#' @param ... Numeric, character or function. 
#' 
#' Map types:
#' \itemize{
#'   \item{ RB }{ Representation Bin map. }
#'   \item{ B }{ Bin map. }
#'   \item{ RBB }{ Representation Binary Bin map. }
#'   \item{ BB }{ Binary Bin map. }
#'   \item{ RH }{ Representation Heatmap.}
#'   \item{ H }{  Heatmap. }
#'   \item{ RSITE }{ Representation site map.}
#'   \item{ SITE }{ Site map. }
#'   \item{ RBSITE }{ Representation Bin site map. }
#'   \item{ BSITE }{ Bin site map. }
#' }
#' Root parameters:
#' \itemize{
#'   \item{ \code{breaks} }{ Breaking intervals per map type. }
#'   \item{ \code{colors} }{ Colors per map type. }
#'   \item{ \code{legend} }{ Legend display per map type. }
#'   \item{ \code{frame} }{ Frame display per map type. }
#'   \item{ \code{mar} }{ Chart margins (default: 5.1, 3.1, 4.1, 3.1). }
#' }
#' \code{breaks} parameters:
#' \itemize{
#'   \item{ \code{breaks.border} }{ Map border break intervals (default: -5.0, -4.0, -2.1, -2.0, -1.0). }
#'   \item{ \code{breaks.mapType} }{ Break intervals per map type. }
#'   \item{ \code{breaks.tr} }{ Heatmap zero level threshold. }
#' }
#' \code{colors} parameters: 
#' \itemize{
#'   \item{ \code{colors.n} }{ Number of color levels (default: 10). }
#'   \item{ \code{colors.bin0} }{ Good die color. }
#'   \item{ \code{colors.border} }{ Border colors. }
#'   \item{ \code{colors.mapType} }{ Colors per map type. }
#' }
#'
#' @template parFunctionDetails
#' 
#' @template mapPlotExamples
#' 
#' @export
mapPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="map"))
}

#' binPlot Parameters
#'
#' Gets, sets and resets visual aspects of binPlot.
#' @param ... Numeric, character or function. 
#' Root parameters:
#' \itemize{
#'   \item{ \code{colors} }{ Color profile. }
#'   \item{ \code{line} }{ Line attributes. }
#'   \item{ \code{space} }{ Space between bars (default: 0.2). }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.box} }{ Bar color (default: "#80b1d3"). }
#'   \item{ \code{colors.border} }{ Bar border color. }
#'   \item{ \code{colors.line} }{ Cumulative line color (default: "#fb8072"). }
#' }
#' \code{line} parameters: 
#' \itemize{
#'   \item{ \code{line.lwd} }{ Cumulative line width (default: 2). }
#' }
#' @template parFunctionDetails
#' @template binPlotExamples
#' 
#' @export
binPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="bin"))
}

#' Histogram Plot Parameters 
#'
#' @description Gets, sets and reloads default visual parameters of histPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{ 
#'   \item{ \code{colors} }{ Color schemes. }
#'   \item{ \code{pbr} }{ Default number of histogram cells for "hist" splits per chart type (default: 60 (hist), 30 (beside)). } 
#'   \item{ \code{axes} }{ Axes parameters. }
#'   \item{ \code{space} }{ Space between bars (default: 0 (hist)). }
#'   \item{ \code{legend} }{ Legend parameters. }
#' }
#'
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.zLevel} }{ Color scheme for categorical variables. }
#'   \item{ \code{colors.zBr} }{ Color scheme for numerical variables. }
#'   \item{ \code{colors.border} }{ Bar border color per chart type (default: 0.9 (hist, dens), \code{NA} (beside)). }
#'   \item{ \code{colors.alpha} }{ Alpha setting per chart type (default: 0.5 (hist, dens), 0.8 (beside)). }
#' }
#' 
#' \code{axes} parameters:
#' \itemize{
#'   \item{ \code{axes.x.ticks} }{ Number of x axis ticks (default: 5). }
#'   \item{ \code{axes.x.signif} }{ Significant digits in x axis (default: 3). }
#'   \item{ \code{axes.y.type} }{ Type of y axis: "perc" for percent of total and "count" for counts (default: "perc"). }
#'   \item{ \code{axes.y.lab} }{ Label of y axis per chart type. }
#' }
#' 
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "topright"). }
#'   \item{ \code{legend.title} }{ Legend title per chart type (default: "Overlay" (hist, dens), "Beside" (beside)). }
#'   \item{ \code{legend.signif} }{ Number of significant digits (default: 3). }
#' }
#' 
#' @template parFunctionDetails
#'
#' @template histPlotExamples
#'
#' @export
histPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="hist", 
		checkChar=list("axes.y.type"=c("count", "perc"))))
}

#' Timeseries Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of timePlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{ 
#'   \item{ \code{colors} }{ Color schemes. }
#'   \item{ \code{pch} }{ Plot points symbols. } 
#'   \item{ \code{cexAxis} }{ Magnification of axis annotation. }
#'   \item{ \code{mgp} }{ Margin line (in ‘mex’ units) for the axis title, axis, labels and axis line. } 
#'   \item{ \code{ticks} }{ Number of shown tickmarks. } 
#'   \item{ \code{grid} }{ Chart grid. } 
#'   \item{ \code{lwd} }{ Smoothed lines width. }
#'   \item{ \code{legend} }{ Legend parameters. }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.zLevel} }{ Color scheme for categorical variables. }
#'   \item{ \code{colors.zBr} }{ Color scheme for numerical variables. }
#'   \item{ \code{colors.line} }{ Colors of smoothed lines. }
#'   \item{ \code{colors.box.border} }{ Color of box border for \code{chart} box type (default: "#525252"). }
#'   \item{ \code{colors.box.fill} }{ Color of box fill for \code{chart} box type (default: "#D9D9D9").  }
#'   \item{ \code{colors.alpha} }{ Color transparency setting per chart type (default: 0.8).  }
#' }
#' \code{grid} parameters:
#' \itemize{
#'   \item{ \code{grid.lty} }{ Grid lines type (default: 3). }
#'   \item{ \code{grid.color} }{ Grid lines color (default: "grey"). }
#'   \item{ \code{grid.xy} }{ Visibility of grid lines corresponding to x and y axis respectively (default: \code{FALSE} \code{FALSE}). }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "topright"). }
#'   \item{ \code{legend.signif} }{ Number of significant digits (default: 3). }
#' }
#'
#' @template parFunctionDetails
#'
#' @template timePlotExamples
#'
#' @export
timePlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="time"))
}

#' Scatter Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of xyPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{  
#'   \item{ \code{colors} }{ Color schemes. }
#'   \item{ \code{pch} }{ Plot points symbols. }
#'   \item{ \code{linFit} }{ Linear fit line parameters. }
#'   \item{ \code{legend} }{ Legend parameters. } 
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.zLevel} }{ Color scheme for categorical variables. }
#'   \item{ \code{colors.zBr} }{ Color scheme for numerical variables. }
#'   \item{ \code{colors.alpha.zLevel} }{ Color transparency for categorical variables (default: 0.8). }
#'   \item{ \code{colors.alpha.zBr} }{ Color transparency for numerical variables (default: 1). }
#' }
#' \code{linFit} parameters:
#' \itemize{
#'   \item{ \code{linFit.color} }{ Color of linear fit line (default: "#525252B3"). }
#'   \item{ \code{linFit.lwd} }{ Width of linear fit line (default: 2). }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "topright"). }
#'   \item{ \code{legend.signif} }{ Number of significant digits (default: 3). }
#'   \item{ \code{legend.lty} }{ Lines type of the legend (default: 0). }
#'   \item{ \code{legend.lwd} }{ Lines width of the legend (default: 1). }
#'   \item{ \code{legend.cex} }{ Annotation magnification of the legend (default: 0.9). }
#'   \item{ \code{legend.bty} }{ Type of box that is drawn around the legend (default: "n"). }
#'   \item{ \code{legend.segLen} }{ The length of lines drawn to illustrate ‘lty’ and/or ‘lwd’
#'           (in units of character widths) (default: "0"). }
#'   \item{ \code{legend.pch} }{ Points symbols of the legend (default: 15). }
#'   \item{ \code{legend.ptCex} }{ A numeric determining the size of the exemplary vertex in the legend box (default: 1.8). }
#' }
#'
#' @template parFunctionDetails
#'
#' @template xyPlotExamples
#'
#' @export
xyPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="xy"))
}

#' Box Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of boxPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{ 
#'   \item{ \code{colors } }{ Color schemes. } 
#'   \item{ \code{boxWex} }{ Relative width of boxes (default: 0.5). } 
#'   \item{ \code{varWidth} }{ Variable width of boxes proportional to 
#'    the square-roots of the number of observations in the groups (default: \code{FALSE}). } 
#'   \item{ \code{xSignif} }{ Number of significant digits for x axis values (default: 3). } 
#'   \item{ \code{mar} }{ Chart margins. } 
#'   \item{ \code{axis} }{ Axis parameters. }
#'   \item{ \code{legend} }{ Legend parameters. }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ colors.zLevel }{ Color scheme for categorical variables. }
#'   \item{ colors.zBr }{ Color scheme for numerical variables. }
#'   \item{ colors.border }{ Boxplot border color. }
#' }
#' \code{axis} parameters:
#' \itemize{
#'   \item{ axis.x.las }{ Parallel (=0) or perpendicular(=2) labels to x axis (default: 0). }
#'   \item{ axis.x.cex }{ Annotation magnification of x axis (default: 1). }
#'   \item{ axis.x.line }{ Distance from x axis (default: \code{NA}).  }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ legend.pos }{ Legend position (default: "topright"). }
#'   \item{ legend.signif }{ Number of significant digits (default: 3). }
#' }
#'
#' @template parFunctionDetails
#'
#' @template boxPlotExamples
#'
#' @export
boxPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="box"))
}

#' Model Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of modelPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters
#' \itemize{
#'   \item{ \code{heat} }{ Heatmap color scheme. }
#'   \item{ \code{colors} }{ Color schemes. }
#'   \item{ \code{cexAxis} }{ Axis cex for corresponding chart type. }
#' }
#' \code{heat} parameters:
#' \itemize{
#'   \item{ heat.n }{ Number of color shades (default: 11). }
#'   \item{ heat.breaks }{ Intervals breaks. }
#'   \item{ heat.colors }{ Shading colors. }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ colors.cart.default.box }{ CART default box color (default: 0). }
#'   \item{ colors.cart.default.border }{ CART default border color (default: "black"). }
#'   \item{ colors.cart.br2.box }{ CART box color in \code{color="majority"} mode (default: "#fdae61", "#a6d96a"). }
#'   \item{ colors.cart.br2.border }{ CART border color in \code{color="majority"} mode (default: "#fdae61", "#a6d96a"). }
#'   \item{ colors.dr.box }{ Dimensionality reduction box color (default: "#80b1d3"). }
#'   \item{ colors.dr.border }{ Dimensionality reduction border color (default: "#739FBEFF"). }
#'   \item{ colors.dr.line }{ Dimensionality reduction line color (default: "#fb8072"). }
#' }
#' \code{axis text magnification} parameters:
#' \itemize{
#'   \item{ cexAxis.boost }{ Cex for Generalized Boosted Regression model (default: 0.8). }
#'   \item{ cexAxis.dr }{ Cex for Dimensionality reduction (default: 0.8). }
#' }
#' 
#' @template parFunctionDetails
#'
#' @template modelPlotExamples
#'
#' @export
modelPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="model"))
}

#' Partial Plot Parameters
#'
#' Gets, sets and reloads default visual parameters of partialPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters
#' \itemize{
#'	 \item{ \code{colors} }{ Color schemes. }
#'	 \item{ \code{lwd} }{ Line width (default: 1.5). }
#'	 \item{ \code{legend} }{ Legend parameters. }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.confInterv} }{ Confidence interval area color (default: "#5252521A"). }
#'   \item{ \code{colors.zLevel} }{ Color scheme for categorical variables. }
#'   \item{ \code{colors.zBr} }{ Color scheme for numerical variables. }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "topright"). }
#'   \item{ \code{legend.signif} }{ Number of significant digits (default: 3). }
#'   \item{ \code{legend.lty} }{ Lines type of the legend (default: 0). }
#'   \item{ \code{legend.lwd} }{ Lines width of the legend (default: 1). }
#'   \item{ \code{legend.cex} }{ Annotation magnification of the legend (default: 0.9). }
#'   \item{ \code{legend.bty} }{ Type of box that is drawn around the legend (default: "n"). }
#'   \item{ \code{legend.segLen} }{ The length of lines drawn to illustrate ‘lty’ and/or ‘lwd’
#'           (in units of character widths) (default: "0"). }
#'   \item{ \code{legend.pch} }{ Points symbols of the legend (default: 15). }
#'   \item{ \code{legend.ptCex} }{ A numeric determining the size of the exemplary vertex in the legend box (default: 1.8). }
#' }
#'
#' @template parFunctionDetails
#'
#' @template partialPlotExamples
#'
#' @export
partialPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="partial"))
}

#' Site Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of sitePlot.
#' 
#' @param ... Numeric, character or function. 
#' 
#' Root parameter:
#' \itemize{
#'	 \item{ \code{text} }{ Site text parameters. }
#' }
#' \code{text} parameters:
#' \itemize{
#'   \item{ \code{text.cex} }{ Text magnification (default: 1.5). }
#'   \item{ \code{text.pos} }{ Text relative position. Defaults to 1 i.e. below coordinates. }
#'   \item{ \code{text.offset} }{ Text offset from relative position (default: 0.4). }
#'   \item{ \code{text.flatPos} }{ Text relative position for site near flat if too close to the edge. Defaults to 3 i.e. above coordinates. }
#'   \item{ \code{text.flatOffset} }{ Text offset from relative position for site near flat if too close to the edge (default: 0.5). }
#'   \item{ \code{text.signif} }{ Number of significant digits (default: 3). }
#'   \item{ \code{text.colors} }{ Text shading scheme. }
#' }
#' \code{text.colors} parameters:
#' \itemize{
#'   \item{ \code{text.colors.single} }{ Text color in single color mode (default: "black"). }
#'   \item{ \code{text.colors.n} }{ Number of text shades (default: 4). }
#'   \item{ \code{text.colors.breaks} }{ Shading intervals breaks. }
#'   \item{ \code{text.colors.br} }{ Shading colors. }
#' }
#'
#' @details Function \code{mapPlot} is used for wafer rendering. See \code{?mapPlotPar} for heatmap
#' color scheme settings.
#' 
#' @template parFunctionDetails
#'
#' @template sitePlotExamples
#'
#' @export
sitePlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="site"))
}

#' Correlation Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of corPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters
#' \itemize{
#'	 \item{ \code{mat} }{ Correlation matrix parameters. }
#'	 \item{ \code{dend} }{ Correlation dendrogram parameters. }
#' }
#' \code{mat} parameters:
#' \itemize{
#'   \item{ mat.tl.cex }{ Text magnification (default: 0.55). }
#'   \item{ mat.tl.color }{ Text color. }
#'   \item{ mat.gridColor }{ Grid color. }
#'   \item{ mat.mar }{ Chart margin (default: 0, 0, 2, 0). }
#' }
#' \code{dend} parameters:
#' \itemize{
#'   \item{ dend.cex }{ Text magnification (default: 0.7). }
#'   \item{ dend.thrColor }{ Color of grouping rectangles (default: "red"). }
#'   \item{ dend.mar }{ Chart margin (default: 5.1, 4.1, 4.1, 2.1). }
#' }
#'
#' @template parFunctionDetails
#'
#' @template corPlotExamples
#'
#' @export
corPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="cor"))
}

#' Distribution Shift Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of distrShiftPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameters
#' \itemize{
#'	 \item{ \code{colors} }{ Color schemes. }
#'	 \item{ \code{signif} }{ Number of significant digits (default: 3). }
#'	 \item{ \code{pbr} }{ Default number of histogram cells for "hist" splits (default: 60). }
#'	 \item{ \code{space} }{ Space between bars (default: 0). }
#'	 \item{ \code{legend} }{ Legend parameters. }
#'	 \item{ \code{zLegend} }{ Legend corresponding do \code{z} variable. }
#' }
#' \code{colors} parameters:
#' \itemize{
#'   \item{ \code{colors.alpha} }{ Transparency setting (default: 0.7). }
#'   \item{ \code{colors.distr} }{ Distributions colors (default: "#BDBDBDB3", "#8DD3C7B3"). }
#'   \item{ \code{colors.border} }{ Distributions borders colors (default: "#AAAAAAFF", "#7FBEB3FF"). }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ legend.pos }{ Legend position (default: "topright"). }
#'   \item{ \code{legend.lty} }{ Lines type of the legend (default: 0). }
#'   \item{ \code{legend.lwd} }{ Lines width of the legend (default: 1). }
#'   \item{ \code{legend.cex} }{ Annotation magnification of the legend (default: 0.9). }
#'   \item{ \code{legend.bty} }{ Type of box that is drawn around the legend (default: "n"). }
#'   \item{ \code{legend.segLen} }{ The length of lines drawn to illustrate ‘lty’ and/or ‘lwd’
#'           (in units of character widths) (default: "0"). }
#'   \item{ \code{legend.pch} }{ Points symbols of the legend (default: 15). }
#'   \item{ \code{legend.ptCex} }{ A numeric determining the size of the exemplary vertex in the legend box (default: 1.8). }
#' }
#' \code{zLegend} parameters:
#' \itemize{
#'   \item{ zLegend.pos }{ Legend position (default: "topleft"). }
#' }
#' @template parFunctionDetails
#'
#' @template distrShiftPlotExamples 
#'
#' @export
distrShiftPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="distrShift"))
}

#' Map Overlay Plot Parameters
#'
#' @description Gets, sets and reloads default visual parameters of overlayMapPlot.
#'
#' @param ... Numeric, character or function. 
#' 
#' Root parameter
#' \itemize{
#'	 \item{ \code{colors} }{ Shading colors for multiple map objects regime. }
#' }
#'
#' @template parFunctionDetails
#'
#' @template overlayMapPlotExamples
#'
#' @export
overlayMapPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="overlayMap"))
}

#' Bayes Defectivity Plot Parameters
#' 
#' @description Gets, sets and reloads default visual parameters of defBayesPlot.
#' 
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{
#'   \item{ \code{level} }{ Color scheme. }
#'   \item{ \code{lwd} }{ Line width (default: 2.5). }
#'   \item{ \code{text} }{ Text parameters. }
#'   \item{ \code{legend} }{ Legend parameters. }
#'   \item{ \code{color} }{ Color parameters. }
#'   \item{ \code{las} }{ Style of axis labels: parallel (=0), perpendicular (=2), horizontal (=1) (default: 1). }
#' }
#' \code{text} parameters:
#' \itemize{
#'   \item{ \code{text.signif} }{ Number of significant digits (default: 2).}
#'   \item{ \code{text.pos} }{ Text relative position. Defaults to 1 i.e. below coordinates. }
#'   \item{ \code{text.bg} }{ Text background color (default: "white").}
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "topleft").}
#' }
#' \code{color} parameters:
#' \itemize{
#'   \item{ \code{color.yield.alpha} }{ Transparency setting for the "yield" chart type (default: 0.8).}
#' }
#' 
#' @template parFunctionDetails
#' 
#' @template defBayesPlotExamples
#' 
#' @export
defBayesPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="defBayes"))
}


#' Bayes Defectivity Optimization Plot Parameters
#' 
#' @description Gets, sets and reloads default visual parameters of defBayesOptimPlot.
#' 
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{
#'   \item{ \code{color} }{ Color parameters. }
#' }
#' \code{color} parameters:
#' \itemize{
#'   \item{ \code{color.yield} }{ Color setting for the "yield" chart type (default: "#80B1D3").}
#'   \item{ \code{color.prob} }{ Color setting for the "prob" chart type.}
#' }
#' 
#' @template parFunctionDetails
#' 
#' @template defBayesOptimPlotExamples
#' 
#' @export
defBayesOptimPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="defBayesOptim"))
}

#' Inherited Defects Percentages Plot Parameters
#' 
#' @description Gets, sets and reloads default visual parameters of defInheritedPlot.
#' 
#' @param ... Numeric, character or function. 
#' 
#' Root parameters:
#' \itemize{
#'   \item{ \code{level} }{ Color scheme. }
#'   \item{ \code{lwd} }{ Line width (default: 2.5). }
#'   \item{ \code{legend} }{ Legend parameters. }
#' }
#' \code{legend} parameters:
#' \itemize{
#'   \item{ \code{legend.pos} }{ Legend position (default: "bottomright").}
#' }
#' 
#' @template parFunctionDetails
#' 
#' @template defInheritedPlotExamples
#' 
#' @export
defInheritedPlotPar <- function(...) {
	
	return(managePlotPar(..., plotParType="defInherited"))
}


#
# Generics
#

# S3 semiFrame generic to plot maps
#' @export
mapPlot <- function(...) { UseMethod("mapPlot") }

#
# Methods Functions
#

#
# semiFrame Generics Methods
#

#' Wafer Map Plot
#'
#' @description Plots wafer bin map or heatmap represented by a \code{semiFrame} row. 
#' In the WBM case colors correspond to bins and color sequence is 
#' based on bin occurrence on the wafer. In the heatmap case colors correspond
#' to bad die frequency. 
#'
#' @param obj \code{semiFrame} object. Browsing mode is activated if object has 
#' multiple rows.
#' @param ... Two possible actions:
#' \itemize{
#'   \item{ Map comparison: }{ Input arbitrary number of \code{semiFrame} objects. Maps from these 
#'     objects matching \code{lot} and \code{wafer} of the actually plotted map from 
#'     the obj \code{semiFrame} are plotted in a row. }
#'   \item{ Cluster browsing: }{ Input \code{cluster=clusterName}, which initiates browsing of the \code{clusterName} cluster. }
#' } 
#' @template mainParam 
#' @template numMainRowParam 
#' @param subMain Character. Specifies column (columns) to be plotted below map. In cluster borwsing mode
#'   cluster count is shown by default (default: NULL).
#' @param clusterMain Logical. If TRUE includes cluster name in the main title (default: TRUE).
#' @template clusterThrParam
#' @param edit Logical. If TRUE editing tools in the browsing mode are enabled and edited object
#' is returned upon browsing mode exit. See help in the browsing mode for more information (default: FALSE).
#' @template showLegendParam
#' @param method Advanced: Character. Determines wafer rendering method. "default" uses \code{inferenceLab}
#' package, "raster" uses \code{raster} package (default: "default").
#' @param addPoints Advanced: Matrix. Adds custom points to plot (default: NULL).
#' @param mapAlpha Advanced: Numeric. Adds transparency. Values from 0 to 1 (default: NULL).
#' @param add Advanced: Logical. Overlays map over current plot (default: FALSE).
#'
#' @aliases mapPlot
#' 
#' @return If edit mode is enabled, edited object is returned. Plot otherwise.
#'
#' @template mapPlotExamples
#'  
#' @export
mapPlot.semiFrame <- function(obj, ..., main=NULL, numMain=TRUE, subMain=NULL, clusterMain=TRUE, clusterThr=0.5, edit=FALSE, 
								showLegend=TRUE, method="default", addPoints=NULL, mapAlpha=NULL, add=FALSE) {


	checkCharInput(method, c("default", "raster"))	

	sfCompare <- eval(substitute(alist(...)))

	showCluster <- NULL
	if ("cluster"%in%names(sfCompare)) {
		showCluster <- gsub("\"*","",deparse(sfCompare[["cluster"]]))
		if (showCluster=="") { showCluster <- NULL }
		sfCompare[["cluster"]] <- NULL
	}

	sfTab <- tab(obj, addRow=TRUE)
	reloadTab <- FALSE
	
	if (!nrow(sfTab)) { 
		image(sfCross$img, col=sfCross$color, axes=FALSE)
		return(invisible()) 
	}

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP), add=TRUE, after=FALSE)
	par(mar=sfPar$map$user$mar)

	if (length(sfCompare)) {
		for (i in seq_along(sfCompare)) {
			sfCompare[[i]] <- eval(sfCompare[[i]])
		}
		beforePar <- par(mfcol=c(1,length(sfCompare)+1))
		on.exit(par(beforePar), add=TRUE, after=FALSE)
	}

	hasCluster <- sfDataType$clust%in%names(dataType(obj))&&!mapType(obj)%in%sfMapTypeGroup$heat
	if (!hasCluster&&edit) {
		warning("semiFrame object has no clusters.")
		edit <- hasCluster
	}

	mType <- mapType(obj)
	rowIdx <- 1
	autoBrowse <- 0
	if (is.null(showCluster)) {
		rows <- 1:nrow(sfTab)
	} else {
		rows <- sfTab[eval(as.name(showCluster))>clusterThr,eval(as.name(tmName$allRow))]
	}


	while (rowIdx %in% seq_along(rows)) {
		sfTabRow <- sfTab[rows[rowIdx]]

		# main
		if (is.null(main)) {
			plotMapMain  <- sprintf("%s%s, %s, %s", sfTabRow[[sfName$tech]], ifelse(is.na(sfTabRow[[sfName$maskset]]),"",paste0(", ",sfTabRow[[sfName$maskset]])), 
								sfTabRow[[sfName$lot]], sfTabRow[[sfName$wafer]])
			if (numMain) { plotMapMain <- sprintf("%d: %s", sfTabRow[[tmName$allRow]],plotMapMain) }
			if (clusterMain&&hasCluster) { 
				isCluster <- t(sfTabRow[,dataType(obj)[[sfDataType$clust]],with=FALSE])
				isCluster <- rownames(isCluster[isCluster==max(isCluster)&isCluster>clusterThr,,drop=FALSE])
				if (length(isCluster)) {
					plotMapMain <- paste(plotMapMain, paste(isCluster, collapse=", "), sep=", ")	
				}
			}
		} else {
			plotMapMain=main
		}
		
		# submain
		plotMapSubmain <- character(1)	
		if (!is.null(subMain)) {
			for (i in 1:length(subMain)) {
				if (subMain[i]%in%colnames(sfTabRow)) {
					if (!is.na(sfTabRow[[subMain[i]]])) {
						plotMapSubmain <- paste0(plotMapSubmain, ifelse(i>1,", ",""),
													sfTabRow[[subMain[i]]])
					}
				}
			}
		} else if (!is.null(showCluster)) {
			plotMapSubmain <- sprintf("%d / %d", rowIdx, length(rows))
		}
		
		if (method=="raster") {
			
			mapR <- getRowRas(obj, rows[rowIdx])
			sp::plot(mapR, main=plotMapMain)
		} else {

			mapDim <- as.integer(obj$matDim[.(sfTabRow[[tmName$matN]])][,c(sfName$matRows,sfName$matCols),with=FALSE])
			mapDB <- as.integer(sfTabRow[,c(tmName$matN,tmName$matRow),with=FALSE])
			mapData <- obj$mat[[mapDB[1]]][mapDB[2],]
			mapData <- matrix(mapData, mapDim[1], mapDim[2], byrow=TRUE)

			# image diplay: 90degrees rotation
			mapData <- t(mapData)
			mapData <- mapData[,ncol(mapData):1]

			if (!is.null(addPoints)) {
				for (i in 1:nrow(addPoints)) {
					mapData[addPoints[i,2], addPoints[i,1]] <- addPoints[i,3]
				}
			}

			userPlotPar <- sfPar$map$user

			if (mType%in%sfMapTypeGroup$bin) {
				arg1 <- tableNum(mapData)
				arg2 <- NULL
			} else {
				arg1 <- userPlotPar$colors$n		
				arg2 <- userPlotPar$breaks$tr	
			}

			breaks <- funVec(userPlotPar$breaks[[mType]], arg1, arg2)
			colors <- funVec(userPlotPar$colors[[mType]], arg1)

			if (!is.null(mapAlpha)) { colors <- colorAlpha(colors, mapAlpha) }

			image(mapData, col=colors, breaks=breaks, main=plotMapMain, axes=userPlotPar$frame[[mType]], 
					xaxt='n', yaxt='n', add=add)
		}
		title(sub=plotMapSubmain, line=sfPar$map$user$subMainLine[[method]])

		if (showLegend&&method!="raster"&&sfPar$map$user$legend[[mType]]) { 
			legendText <- as.character(breaks[breaks>=0])
			if (mType==sfMapType$rdef) {
				legendText[length(legendText)] <- paste(1+breaks[breaks>=0][length(legendText)-1], "=<")
			}
			par(xpd=TRUE)
			legend(x=sfPar$map$user$legend$x, y=sfPar$map$user$legend$y, legendText, 
				col=na.omit(colors[breaks>=-1]), lty=sfPar$map$user$legend$lty, 
				lwd=sfPar$map$user$legend$lwd, cex=sfPar$map$user$legend$cex, bty=sfPar$map$user$legend$bty,
				seg.len=sfPar$map$user$legend$segLen, pch=sfPar$map$user$legend$pch, 
				pt.cex=sfPar$map$user$legend$ptCex) 
		}
		
		if (length(sfCompare)) {
			for (sfCompareObj in sfCompare) {
				if (mapType(sfCompareObj)%in%sfMapTypeGroup$heat) {
					compareLot <- sfName$clustLot
					compareWafer <- which.max(as.integer(sfTabRow[,dataType(obj)[[sfDataType$clust]],with=FALSE]))
					compareWafer <- dataType(obj)[[sfDataType$clust]][compareWafer]
				} else {
					compareLot <- sfTabRow[[sfName$lot]]
					compareWafer <- sfTabRow[[sfName$wafer]]
				}
				mapPlot(sfCompareObj[eval(.sfName$lot)==compareLot&eval(.sfName$wafer)==compareWafer], main=main, 
					numMain=numMain, subMain=subMain, method=method, showLegend=TRUE) 
			}
		}

		if (nrow(sfTab)>1) { 
			browse <- plotBrowsing(rowIdx, idxMax=nrow(sfTab), autoBrowse=autoBrowse, caller=ifelse(edit, "mapPlot", "default"))
			autoBrowse <- browse$auto
			rowIdx <- browse$idx
			if (is.na(rowIdx)) { break }
			clust <- browse$cluster
			if (edit&&!is.null(clust)) {
				clustName <- clust$clust
				row <- rows[rowIdx]
				reloadTab <- TRUE
				if (clust$action=="add") {
					obj <- cluster(obj, row, to=clustName, add=TRUE, fromThr=clusterThr, verbose=FALSE)
				} else if (clust$action=="move") {
					obj <- cluster(obj, row, to=clustName, add=FALSE, fromThr=clusterThr, verbose=FALSE)
				} else if (clust$action=="del") {
					obj <- cluster(obj, row, from=clustName, to=del, fromThr=clusterThr, verbose=FALSE)
				} else if (clust$action=="remove") {
					obj <- cluster(obj, remove=clustName, verbose=FALSE)
				} else if (clust$action=="summary") {
					summary(obj, details=FALSE)
					reloadTab <- FALSE
				}
				rowIdx <- rowIdx - 1
			}
		}
		rowIdx <- rowIdx + 1
		
		if (reloadTab) { 
			sfTab <- tab(obj, addRow=TRUE) 
			if (!is.null(showCluster)) {
				rows <- sfTab[eval(as.name(showCluster))>clusterThr,eval(as.name(tmName$allRow))]
			}
			reloadTab <- FALSE
		}
	}

	if (edit) { return(obj) }
	return(invisible())
}

#' Bin Pareto Plot
#'
#' Plots contribution to yield loss by bins. If clusters are present, browsing is initiated.
#' 
#' @template sfobjParam
#' @param bins Integer. Specifies which bins to use. For all available bins use keyword "all" (default: "all").
#' @param ... Cluster browsing. Input \code{cluster=clusterName} to browse directly into \code{clusterName} cluster.
#' @template mainParam
#' @template clusterThrParam
#' @param plot Logical. If \code{FALSE}, list of bin pareto percentages is returned for all maps and for each cluster (default: \code{TRUE}).
#' 
#' @return Plot or list of tables of pareto percentages.
#' 
#' @template binPlotExamples
#' 
#' @export
binPlot <- function(obj, bins="all", ..., main=NULL, clusterThr=0.5, plot=TRUE) {

	if (!is.semiFrame(obj)) { stop("Provide a 'semiFrame' object.") }

	args <- eval(substitute(alist(...)))
	hasCluster <- sfDataType$clust %in% names(obj$aid$dataType)
	showCluster <- NULL
	if (hasCluster && "cluster" %in% names(args)) {
		showCluster <- gsub("\"*","",deparse(args[["cluster"]]))
		if (showCluster=="") { showCluster <- NULL }
		args[["cluster"]] <- NULL
	}

	if (hasCluster && is.null(showCluster)) {
		showCluster <- c("all", obj$aid$dataType[[sfDataType$clust]])
	}

	if (is.null(showCluster)) { showCluster <- "all" }

	binTotal <- lapply(seq_along(showCluster), function(...) integer(1))
	names(binTotal) <- showCluster
	binPerc <- list()
	binCum <- list()
	for (clust in showCluster) {

		for (matN in seq_along(obj$mat)) {
			
			if (clust=="all") {
				row <- TRUE
			} else {
				if (nrow(obj$matDim)>1) {
					row <- which(obj$tab[.(matN)][[clust]]>clusterThr)
				} else {
					row <- which(obj$tab[[clust]]>clusterThr)
				}
			}

			mapIdx <- obj$mat[[matN]][row,]>0
			binTotal[[clust]] <- binTotal[[clust]] + sum(mapIdx)
			count <- as.data.frame(table(as.vector(obj$mat[[matN]][row,][mapIdx])),  stringsAsFactors=FALSE)
			countN <- t(as.data.table(count[[2]]))
			colnames(countN) <- count[[1]]
			if (is.null(binPerc[[clust]])) {
				binPerc[[clust]] <- countN
			} else {
				binPerc[[clust]] <- rbind(binPerc[[clust]], countN, fill=TRUE)
			}
		}
		
		binName <- colnames(binPerc[[clust]])
		binPerc[[clust]] <- data.table(bin=as.integer(colnames(binPerc[[clust]])), 
										perc=100*sapply(binPerc[[clust]], function(col)  sum(col,na.rm=TRUE))/binTotal[[clust]])
		binPerc[[clust]] <- binPerc[[clust]][order(-perc)]
		binPerc[[clust]]$perc <- round(binPerc[[clust]]$perc, 3)

		if (!any(bins=="all")) {

			if (!any(binPerc[[clust]]$bin %in% bins)) {
				warning(sprintf("Bin(s) specified using 'bins' parameter not found in cluster %s.", clust))
			} else {
				binPerc[[clust]] <- binPerc[[clust]][bin %in% bins]
			}
		}


		binCum[[clust]] <- data.table(bin=binPerc[[clust]]$bin, perc=cumsum(binPerc[[clust]]$perc))
	}

	gc()

	if (!plot) { return(binPerc) }

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))
	par(mar=c(5.1, 4.1, 5.1, 4.1))

	autoBrowse <- 0
	clust <- 1
	while (clust %in% seq_along(binPerc)) {
	
		if (is.null(main)) {
			binMain <- sprintf("%s, %s, Bin Pareto", obj$tab[[sfName$tech]][1], obj$tab[[sfName$maskset]][1])
			if (names(binPerc)[clust]!="all") {
				binMain <- sprintf("%s, %s", binMain, names(binPerc)[clust])	
			}
		} else {
			binMain <- main		
		}

		barplot(binPerc[[clust]]$perc, main=binMain, ylim=c(-max(binPerc[[clust]]$perc)*0.04, max(binPerc[[clust]]$perc)*1.04),
			space=sfPar$bin$user$space, 
			col=sfPar$bin$user$colors$box, 
			border=sfPar$bin$user$colors$border, xlab="Bin",
			ylab="",
			xaxt="n", xpd=FALSE)
		box(bty=sfPar$all$user$par$bty)
		mtext("% of Yield Loss Explained", side=2, line=3, col=sfPar$bin$default$colors$box)

		ticksIdx <- 1:nrow(binPerc[[clust]]) - 0.5
		ticks <- ticksIdx + cumsum(rep(sfPar$bin$user$space, length(ticksIdx)))
		axis(1, at=ticks, labels=binPerc[[clust]]$bin)	

		par(new=TRUE)
		plot(x=1:nrow(binCum[[clust]]), y=binCum[[clust]]$perc, type="l", xaxt="n", yaxt="n", xlab=NA, ylab=NA, 
				col=sfPar$bin$default$colors$line, lwd=sfPar$bin$default$line$lwd)
		axis(4)
		mtext("Cumulative % of Yield Loss Explained", side=4, line=3, col=sfPar$bin$default$colors$line)


		if (length(binPerc)>1) { 
			browse <- plotBrowsing(clust, idxMax=length(binPerc), autoBrowse=autoBrowse)
			autoBrowse <- browse$auto
			clust <- browse$idx
			if (is.na(clust)) { break }
		}

		clust <- clust + 1

	}

	return(invisible())
}

#' Site Wafer Map Plot
#'
#' @description Plots parameters distribution centers (medians by default) per site. 
#' Darkest neighborhood color corresponds to highest parameter center value.
#' Site yield values are determined by counting good dice in the neighborhood rectangles.
#'
#' @template stobjParam
#' @param ... Data columns to be used. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' Browsing mode is activated if multiple columns are provided.
#' @param color Character. Color mode:
#' \itemize{
#'   \item{ "heat" } { Darkest neighborhood color corresponds to highest parameter center value (default). }
#'   \item{ "single" }{ Single color is used for all neighborhood rectangles. } 
#' } 
#' @param fun Function. Centering function applied on data columns per site. For example
#' 			\code{median} (default), \code{mean}, \code{sd}.
#' @template mainParam
#' @template numMainParam
#' @param siteMap Advanced: \code{semiFrame}. Specifies underlying wafer map (default: \code{NULL}).
#' @param nbrhdIdx Advanced: Matrix. Specifies site neighborhood indexes (default: \code{NULL}).
#' @param sitePoints Advanced: Logical. If \code{TRUE} shows site dice (default: \code{FALSE}).
#' @param siteMapSignal Advanced: Character. Specifies column that determines which siteMap
#' is going to be used if multiple siteMaps are provided (default: \code{NULL}).
#' @param siteCoor Advanced: List. User defined site coordinates.
#' @template autoNormParam
#' @template useDataTypeParam
#' @template colParam
#'
#' @template sitePlotExamples
#' 
#' @return If no data columns are provided, site statistics are returned. Plot otherwise.
#' 
#' @export
sitePlot <- function(obj, ..., color="heat", fun=median, main=NULL, numMain=TRUE, siteMap=NULL, nbrhdIdx=NULL, sitePoints=FALSE,
						siteMapSignal=NULL, siteCoor=NULL, autoNorm=TRUE, useDataType=sfUseDataType, col=NULL) {
		
	checkCharInput(color, c("heat", "single"))	

	drawText <- function(diexyTab, textData) {
		y <- diexyTab[[sfName$diey]]+correct[1]
		x <- diexyTab[[sfName$diex]]+correct[2]
		y <- 1-y/emptyMapDim[1]
		x <- x/emptyMapDim[2]
		yOrder <- order(100*y+x)
		yOrder <- sapply(seq_along(yOrder), function(i) match(i, yOrder) )

		if (length(siteText$pos)!=length(x)) {
	 		textPos <- rep(siteText$pos[1], length(x))
		} else {
	 		textPos <- siteText$pos
		}
		textPos <- textPos[yOrder]
		
		if (length(siteText$offset)!=length(x)) {
			textOffset <- rep(siteText$offset[1], length(x))
		} else {
	 		textOffset <- siteText$offset
		}
		textOffset <- textOffset[yOrder]

		if (any(y<=0.1)) {
			textPos[which(y<=0.1)] <- siteText$flatPos
			textOffset[which(y<=0.1)] <- siteText$flatOffset
		}

		if (!is.null(siteText$xOffset)) {
			x <- x+siteText$xOffset
		}

		if (length(colors)==1) { colors <- rep(colors, length(x)) }

		Map(text, x=x, y=y, labels=textData, cex=sfPar$site$user$text$cex, pos=textPos, offset=textOffset, col=colors) 

		return(invisible())
	}

	if (is.null(siteMap)) { siteMap <- obj$aid$siteMap }
	if (is.null(siteMap)) { stop("Provide siteMap") }
	
	if (is.null(siteMapSignal)) { siteMapSignal <- siteMap$aid$siteMapSignal }
	if (!is.null(siteMapSignal)) { 
		pcmMapSignalValue <- unique(obj$tab[[siteMapSignal]])
		if (length(pcmMapSignalValue)>1) { stop("More than one map type in data, wrong siteMapSignal") }
		siteMap <- siteMap[siteMap[[siteMapSignal]]==pcmMapSignalValue]
	}

	emptyMapDim <- matDim(siteMap)[, -1, with=FALSE]
	if (nrow(emptyMapDim)>1) { stop("More than one map type in data, use siteMapSignal") }
	emptyMapDim <- as.integer(emptyMapDim)

	if (color=="single") {
		mapType(siteMap) <- sfMapType$bsite
	}
	correct <- c(siteMap[, correctr], siteMap[, correctc])

	if (!is.null(siteMap$aid$siteText)) {
		siteText <- siteMap$aid$siteText[[siteMap[,siteText]]]
	} else {
		siteText <- list()
		siteText$pos <- sfPar$site$user$text$pos
		siteText$offset <- sfPar$site$user$text$offset
		siteText$flatPos <- sfPar$site$user$text$flatPos
		siteText$flatOffset <- sfPar$site$user$text$flatOffset
	}

	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType

	if (!length(col)) {
		
		if (is.null(siteCoor)) {
			sitesTypes <- siteSummary(obj)$type

		} else {
			sitesTypes <- siteCoor	
		}
		
		idx <- 1
		autoBrowse <- 0
		while (idx %in% 1:length(sitesTypes)) {
			sitesType <- sitesTypes[[idx]]

			if (is.null(main)) {
				siteMain <- paste(siteMap[[sfName$tech]], siteMap[[sfName$maskset]], sprintf("%s type %d", sfName$siteid, idx), sep=", ")
			}

			addPoints <- NULL
			if (sitePoints) {
				
				addPoints <- emptyMapDim[1]-matrix(sitesType[[sfName$diey]]+correct[1])
				addPoints <- cbind(addPoints, sitesType[[sfName$diex]]+correct[2])
				addPoints <- cbind(addPoints, 1.5)
			}

			colors <- "black"
			mapPlot(siteMap, main=siteMain, showLegend=FALSE, addPoints=addPoints)
			drawText(sitesType, sitesType[[sfName$siteid]])

			if (length(sitesTypes)>1) { 
				idx <- plotBrowsing(idx, autoBrowse=autoBrowse)
				autoBrowse <- idx$auto
				idx <- idx$idx
				if (is.na(idx)) { break }
			}
			idx <- idx + 1

		}
		return(invisible())
	}

	if (is.null(nbrhdIdx)) {
		nbrhdIdx <- siteMap$aid$nbrhdIdx[[siteMap[,nbrhdIdx]]]
	}
	if (is.null(nbrhdIdx)) { stop("Provide site neighborhood indexes") }

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))
	par(xpd=TRUE)

	j <- 1
	autoBrowse <- 0
	while (j %in% seq_along(col)) {
		
		siteMain <- perColPlotMain(main, numMain, col[j], useDataType, objDataType)
		
		siteData <- obj$tab[, lapply(.SD, function(colData) { return(as.numeric(do.call(fun, list(colData, na.rm=TRUE)))) }  ), by=c(sfName$diexy), .SDcols=col[j]]
		siteData <- autoNormalize(siteData, col[j], normTable(obj, verbose=FALSE), autoNorm)
		siteData <- siteData[diexy%in%names(nbrhdIdx)]
		siteColData <- signif(siteData[[col[j]]], sfPar$site$user$text$signif)
		siteData[, c(col[j]):=list(siteColData)]
		extractDieRef(siteData)

		siteProfile <- siteData[[col[j]]]

		# siteProfile <- 1-(max(siteProfile)-siteProfile)/max(siteProfile)
		if (max(siteProfile)-min(siteProfile)!=0) {
			siteProfile <- (siteProfile-min(siteProfile))/(max(siteProfile)-min(siteProfile))
		} else {
			siteProfile <- rep(0, nrow(siteData))
		}
		
		siteData[, c("siteProfile"):=list(siteProfile)]

		for (i in 1:nrow(siteData)) {
			if (siteData[i, diexy]%in%names(nbrhdIdx)) {
				siteMap$mat[[1]][1, nbrhdIdx[[siteData[i, diexy]]]] <- 1+siteData[i, siteProfile]
			}
		}

		if (color=="single") {
			colors <- sfPar$site$user$text$colors$single
		} else {
			breaks <- funVec(sfPar$site$user$text$colors$breaks, sfPar$site$user$text$colors$n)
			colors <- funVec(sfPar$site$user$text$colors$br, sfPar$site$user$text$colors$n)
			colorClass <- siteData[,siteProfile]
			for (i in 1:length(colorClass)) { colorClass[i] <- which.min(colorClass[i]>breaks)-1 }
			colors <- colors[colorClass]
		}

		mapPlot(siteMap, main=siteMain, showLegend=FALSE)
		drawText(siteData, siteData[[col[j]]])

		if (length(col)>1) { 
			j <- plotBrowsing(j, idxMax=length(col), autoBrowse=autoBrowse)
			autoBrowse <- j$auto
			j <- j$idx
			if (is.na(j)) { break }
		}
		j <- j + 1

	}


	return(invisible())
}

#' Histogram Plot
#'
#' @description Plots histogram of a \code{semiTable} or \code{semiFrame} variable (data column). 
#' Chart colors are based on \code{z} axis column. 
#'
#' @template stsfobjParam
#' @param ... Variables (data columns) to be plotted. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' Browsing mode is activated if multiple columns are provided.
#' @template xBrParams
#' @template xLevelParam
#' @template outlierParam
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template zOutlierParam
#' @param chart Character. Type of chart:
#' \itemize{
#'   \item{ "hist" } { Plots histogram layers. }
#'   \item{ "density" } { Plots density layers (default). }
#'   \item{ "beside" }{ Plots histogram groups for each bin. } 
#' } 
#' @template yLimParam
#' @template mainParam
#' @template numMainParam
#' @template showLegendParam
#' @template autoNormParam
#' @template useDataTypeParam
#' @template colParam
#' 
#' @template histPlotExamples
#' 
#' @return Plot.
#'
#' @export
histPlot <- function(obj, ..., xBr=NULL, xBrMethod="hist", xLevel=NULL, outlier=NULL, z=c("ProbeYield", "UPYield"), zBr=2, zBrMethod="yieldHeat", zLevel=NULL, zOutlier=NULL,  
						chart="density", yLim=NULL, main=NULL, numMain=TRUE, showLegend=TRUE, autoNorm=TRUE, useDataType=sfUseDataType, col=NULL) {
	

	checkCharInput(chart, c("hist", "density", "beside"))
	
	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType

	if (is.null(z)) {
		objTab <- tab(obj)
		zBr <- 1
		colorPalette <- funVec(sfPar$hist$user$colors$zLevel, zBr)
	} else {
		z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=z)$col[1]

		if (is.null(zLevel)) {
			objTab <- tab(outlierFilter(obj, z, zOutlier))
			objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
			breaks <- breakData(objTab[[z]], zBr, zBrMethod)
			zBr <- length(breaks)-1
			colorPalette <- funVec(sfPar$hist$user$colors$zBr, zBr)
			legendText <- rev(makeIntervals(breaks, sfPar$hist$user$legend$signif))
		} else {
			objTab <- tab(obj)
			zLevel <- processLevel(objTab, z, zLevel)
			zBr <- length(zLevel)
			colorPalette <- funVec(sfPar$hist$user$colors$zLevel, zBr)
			legendText <- rev(as.character(zLevel))
		}
	}

	if (is.numeric(sfPar$hist$user$colors$border[[chart]])&&!is.na(sfPar$hist$user$colors$border[[chart]])) {
		borderPalette <- colorAlpha(colorPalette, coef=sfPar$hist$user$colors$border[[chart]])
	} else {
		borderPalette <- sfPar$hist$user$colors$border[[chart]]
	}

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	j <- 1
	autoBrowse <- 0
	while (j %in% seq_along(col)) {

		subObjTabCol <- col[j]
		if (!is.null(z)) { subObjTabCol <- c(z, subObjTabCol) }

		subObjTab <- na.omit(objTab[,subObjTabCol,with=FALSE])
		subObjTab <- autoNormalize(subObjTab, col[j], normTable(obj, verbose=FALSE), autoNorm)
		subObjTab <- outlierFilter(semiTableWrap(tab=subObjTab), col[j], k=outlier, output="tab")
		
		if (!is.null(xLevel)&&chart!="hist") { chart <- "hist" }
		
		if (chart!="density") {
			if (is.null(xLevel)) {
				histCut <- breakData(subObjTab[[col[j]]], ifelse(is.null(xBr), sfPar$hist$user$pbr[[chart]], xBr), xBrMethod, getMids=TRUE)
				paramCut <- cut(subObjTab[[col[j]]], breaks=histCut$br, labels=histCut$mids, include.lowest=TRUE)
			} else {
				xLevel <- processLevel(subObjTab, col[j], xLevel)
				histCut <- list(mids=xLevel)	
				subObjTab <- subObjTab[which(subObjTab[[col[j]]]%in%xLevel),]
				paramCut <- subObjTab[[col[j]]]
			}
		}

		if (is.null(z)) {
			colorCut <- rep(1, nrow(subObjTab))
			colorCutName <- 1
		} else {
			if (is.null(zLevel)) {
				colorCut <- cut(subObjTab[[1]], breaks=breaks, include.lowest=TRUE)
				colorCutName <- levels(colorCut)
			} else {
				colorCut <- subObjTab[[1]]	
				colorCutName <- zLevel
			}
		}
		

		if (chart!="density") {
			cutConTab <- table(paramCut, colorCut)

			
			if (sfPar$hist$user$axes$y$type=="perc") {
				cutConTab <- 100*cutConTab/sum(cutConTab)
			}		

			if (nrow(cutConTab)==1) {
				warning(sprintf("%s is constant", col[j]))
				break
			} 

			if (!is.null(zLevel)) {
				cutConTab <- cutConTab[,match(as.character(zLevel),colnames(cutConTab))]
			}
			
			if (is.null(yLim)) { 
				yLimit <- c(-max(cutConTab)*0.04,max(cutConTab)*1.04) 
			} else {
				yLimit <- yLim		
			}
		}	

		histMain <- perColPlotMain(main, numMain, col[j], useDataType, objDataType)
		colorPalette <- colorAlpha(colorPalette, ifelse(is.null(z), sfPar$hist$user$colors$alpha$single, sfPar$hist$user$colors$alpha[[chart]]))

		if (chart=="beside") {

			if (is.null(xLevel)) {
				ticksIdx <- pretty(0:(nrow(cutConTab)-1),n=sfPar$hist$user$axes$x$ticks)
				cutMids <- histCut$mids
				if (ticksIdx[length(ticksIdx)]>=nrow(cutConTab)) {
					tickDiff <- ticksIdx[length(ticksIdx)] - nrow(cutConTab) + 1
					cutMids <- c(cutMids, cutMids[length(cutMids)]+(1:tickDiff)*(cutMids[2]-cutMids[1]))
					for (k in 1:tickDiff) { cutConTab <- rbind(cutConTab, 0) }
				}

				xNum <- as.character(signif(cutMids, sfPar$hist$user$axes$x$signif))
				xNumFloatIdx <- grep("[.]", xNum)
				if (length(xNumFloatIdx)) {
					xNumFloatMax <- max(nchar(sub("^.*[.]", "", xNum[xNumFloatIdx])))
					xNum <- formatC(cutMids, digits=xNumFloatMax, format="f")
				}
			} else {
				ticksIdx <- 0:(nrow(cutConTab)-1)
				xNum <- rownames(cutConTab)
			}

		}


		if (chart=="beside") {
			barplot(t(cutConTab), main=histMain, ylim=yLimit, 
				space=sfPar$hist$user$space[[chart]],
				col=colorPalette, 
				border=borderPalette, xlab=col[j], 
				ylab=sfPar$hist$user$axes$y$lab[[sfPar$hist$user$axes$y$type]], 
				xaxt="n", beside=TRUE, xpd=FALSE)

			box(bty=sfPar$all$user$par$bty)
			ticks <- ticksIdx*(zBr+1)
			axis(1, at=ticks+(zBr/2+1), labels=xNum[ticksIdx+1])	
		} 

		if (chart=="hist") {

			# barplot(cutConTab[,zBr], main=histMain, ylim=yLimit, 
			# 	space=ifelse(is.null(xLevel), sfPar$hist$user$space[[chart]], sfPar$hist$user$space$xLevel), 
			# 	col=colorPalette[zBr], 
			# 	border=borderPalette[zBr], xlab=col[j],
			# 	ylab=sfPar$hist$user$axes$y$lab[[sfPar$hist$user$axes$y$type]],
			# 	xaxt="n", xpd=FALSE)
			# box(bty=sfPar$all$user$par$bty)

			# ticks <- ticksIdx + 0.5 + cumsum(rep(ifelse(is.null(xLevel), sfPar$hist$user$space$hist,sfPar$hist$user$space$xLevel), length(ticksIdx)))
			# axis(1, at=ticks, labels=xNum[ticksIdx+1])	

			# if (zBr>1) {
			# 	for (i in (zBr-1):1) {
			# 		barplot(cutConTab[,i], col=colorPalette[i], 
			# 			space=ifelse(is.null(xLevel), sfPar$hist$user$space[[chart]], sfPar$hist$user$space$xLevel), 
			# 			add=TRUE, xaxt="n", yaxt="n", 
			# 			border=borderPalette[i], xpd=FALSE)
			# 	}
			# }

			if (is.null(xLevel)) {
				
				histObj <- list(breaks=histCut$br, mids=histCut$mids, equidist=TRUE)
				class(histObj) <- "histogram"

				histObj$density <- cutConTab[,zBr]
				plot(histObj, main=histMain, ylim=c(0, max(cutConTab)),  
					col=colorPalette[zBr], 
					border=borderPalette[zBr], xlab=col[j],
					ylab=sfPar$hist$user$axes$y$lab[[sfPar$hist$user$axes$y$type]],
					xpd=FALSE, freq=FALSE)
				box(bty=sfPar$all$user$par$bty)

				if (zBr>1) {
					for (i in (zBr-1):1) {
						histObj$density <- cutConTab[,i]
						plot(histObj, col=colorPalette[i], 
							add=TRUE, xaxt="n", yaxt="n", 
							border=borderPalette[i], xpd=FALSE, freq=FALSE)
					}
				}

			} else {
				barplot(cutConTab[,zBr], main=histMain, ylim=yLimit, 
					space=ifelse(is.null(xLevel), sfPar$hist$user$space[[chart]], sfPar$hist$user$space$xLevel), 
					col=colorPalette[zBr], 
					border=borderPalette[zBr], xlab=col[j],
					ylab=sfPar$hist$user$axes$y$lab[[sfPar$hist$user$axes$y$type]],
					xaxt="n", xpd=FALSE)
				box(bty=sfPar$all$user$par$bty)

				ticksIdx <- 1:nrow(cutConTab) - 0.5
				ticks <- ticksIdx + cumsum(rep(ifelse(is.null(xLevel), sfPar$hist$user$space$hist,sfPar$hist$user$space$xLevel), length(ticksIdx)))
				axis(1, at=ticks, labels=rownames(cutConTab))	

				if (zBr>1) {
					for (i in (zBr-1):1) {
						barplot(cutConTab[,i], col=colorPalette[i], 
							space=ifelse(is.null(xLevel), sfPar$hist$user$space[[chart]], sfPar$hist$user$space$xLevel), 
							add=TRUE, xaxt="n", yaxt="n", 
							border=borderPalette[i], xpd=FALSE)
					}
				}
				
			}

		}

		if (chart=="density") {

			xlim <- c(+Inf, -Inf)
			ylim <- c(0, -Inf)

			densityPerColor <- list()
			colDataMin <- min(subObjTab[[col[j]]])
			colDataMax <- max(subObjTab[[col[j]]])
			for (i in 1:zBr) {
				idx <- which(colorCut==colorCutName[i])
				densityPerColor[[i]] <- density(subObjTab[[col[j]]][idx], from=colDataMin, to=colDataMax)
				densityPerColor[[i]]$y <- length(idx)*densityPerColor[[i]]$y/sum(densityPerColor[[i]]$y)
				densityPerColor[[i]]$y[1] <- 0
				densityPerColor[[i]]$y[length(densityPerColor[[i]]$y)] <- 0
				if (sfPar$hist$user$axes$y$type=="perc") {
					densityPerColor[[i]]$y <- 100*densityPerColor[[i]]$y/nrow(subObjTab)
				}	

				xlim <- c(min(xlim[1], min(densityPerColor[[i]]$x)), max(xlim[2], max(densityPerColor[[i]]$x)))
				ylim <- c(0, max(ylim[2], max(densityPerColor[[i]]$y)))
			}

			plot(NULL, main=histMain, xlab=col[[j]], ylab=sfPar$hist$user$axes$y$lab[[sfPar$hist$user$axes$y$type]], xlim=xlim, ylim=ylim)
		
			for (i in rev(1:zBr)) {
				polygon(densityPerColor[[i]]$x, densityPerColor[[i]]$y, col=colorPalette[i], border=borderPalette[i])	
			}

		}

		if (showLegend&!is.null(z)) {
			legend(sfPar$hist$user$legend$pos, legendText, 
				col=colorPalette[zBr:1], title=paste(sfPar$hist$user$legend$title[[chart]],z), lty=sfPar$hist$user$legend$lty, 
				lwd=sfPar$hist$user$legend$lwd, cex=sfPar$hist$user$legend$cex, bty=sfPar$hist$user$legend$bty,
				seg.len=sfPar$hist$user$legend$segLen, pch=sfPar$hist$user$legend$pch, 
				pt.cex=sfPar$hist$user$legend$ptCex)
		}

		if (length(col)>1) { 
			browse <- plotBrowsing(j, idxMax=length(col), autoBrowse=autoBrowse)
			autoBrowse <- browse$auto
			j <- browse$idx
			if (is.na(j)) { break }
		}
		j <- j + 1

	}
	return(invisible())
}


#' Timeseries Plot
#'
#' @description Plots a \code{semiTable} or \code{semiFrame} data column as timeseries. Chart colors are based
#' on \code{z} axis column. 
#'
#' @template stsfobjParam
#' @param ... Data columns to be plotted. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' If \code{yAxisEach = FALSE} browsing mode is activated if multiple columns are provided.
#' If \code{yAxisEach = TRUE} all columns are plotted into one chart.
#' @template outlierParam
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template zOutlierParam
#' @param smooth Character. Adds smoothing to the chart. Possible values are:
#' \itemize{
#'   \item{ "r" } { Rolling function (average by default). }
#'   \item{ "pol" }{ Polynomial fit. } 
#' } 
#' @param sequence Logical. If \code{FALSE} (default) \code{timeCol} is used for x axis. If TRUE points are 
#' plotted as equidistant sequence.  
#' @param yAxisEach Logical. If \code{TRUE} new y axis is provided for each data column (default: \code{FALSE}).
#' @param chart Character. Specifies chart type. Possible values are: 
#' \itemize{
#'   \item{ "p" } { Points (default). }
#'   \item{ "r" }{ Rolling function line (average by default). } 
#'   \item{ "pol" }{ Polynomial fit line. } 
#'   \item{ "box" }{ Box plot. } 
#' } 
#' @template mainParam 
#' @template numMainParam
#' @template showLegendParam
#' @param rWin Integer. Length of rolling window for rolling average charting (default: 10).
#' @param rFun Function applied to rolling window points. For example \code{mean} (default), 
#' \code{median} or \code{sd}.
#' @param rAlign Character. Specifies rolling window alignment. Possible values are 
#' "left", "center" and "right" (default).
#' @param polDegree Integer. Specifies degree of polynomial for polynomial fit charting (default: 20).
#' @template yLimParam
#' @param yLimRight Numeric. Custom right y axis range (default: \code{NULL}).
#' @template yLogParam
#' @template autoNormParam
#' @template drawOrderParam
#' @param timeCol Character. Specifies column for date&time x axis (default: "pcStartDate"). 
#' @template useDataTypeParam
#' @template colParam
#'
#' @template timePlotExamples
#' 
#' @return Plot.
#' 
#' @export
timePlot <- function(obj, ..., outlier=NULL, z=c("ProbeYield", "UPYield"), zBr=2, zBrMethod="yieldHeat", zLevel=NULL, zOutlier=NULL, 
								smooth=FALSE, sequence=FALSE, yAxisEach=FALSE, chart="p", main=NULL, numMain=TRUE, 
								showLegend=TRUE, rWin=10, rFun=mean, rAlign="right", polDegree=20, yLim=NULL, yLimRight=NULL, yLog=FALSE, 
								autoNorm=TRUE, drawOrder=NULL, timeCol="pcStartDate", useDataType=sfUseDataType, col=NULL) {

	checkCharInput(chart, c("p", "r", "pol", "box"))
	checkCharInput(smooth, c("r", "pol", "FALSE"))

	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType
	z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=z)$col[1]

	if (is.null(zLevel)) {
		objTab <- tab(outlierFilter(obj, z, zOutlier))
		objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
		breaks <- breakData(objTab[[z]], zBr, zBrMethod)
		zBr <- length(breaks)-1
		colorPalette <- funVec(sfPar$time$user$colors$zBr, zBr)
		legendText <- rev(makeIntervals(breaks, sfPar$time$user$legend$signif))
	} else {
		objTab <- tab(obj)
		zLevel <- processLevel(objTab, z, zLevel)
		zBr <- length(zLevel)
		colorPalette <- funVec(sfPar$time$user$colors$zLevel, zBr)
		legendText <- rev(as.character(zLevel))
	}

	pDrawOrder <- zBr:1
	if (!is.null(drawOrder)) {
		if (length(drawOrder)!=length(pDrawOrder)) { stop("Incompatible drawOrder length.") }
		pDrawOrder <- drawOrder
	}

	if (sfDataType$clust%in%names(dataType(obj))) {
		if (z%in%dataType(obj)[[sfDataType$clust]]) {
			pDrawOrder <- rev(pDrawOrder)
		}
	}

	newAxisPlot <- FALSE
	if (yAxisEach) {
		col <- col[1:2]
		colorPalette <- funVec(sfPar$time$user$colors$zLevel, length(col))
	}

	colorPalette <- colorAlpha(colorPalette, sfPar$time$user$colors$alpha$p)

	seedPlot <- function(xtsData, datesAsInt=NULL, ylimIdx=NULL, log=ifelse(yLog, "y", "")) {

		if (is.null(datesAsInt)) {
			datesAsInt <- as.vector(unclass(as.factor(zoo::index(xtsData))))
		}
		if (is.null(ylimIdx)) {
			ylimIdx <- 1:ncol(xtsData)
		}
		if (is.null(yLim)) {
			tsdata <- zoo::coredata(xtsData[,ylimIdx])
			ylim <- c(min(tsdata, na.rm=TRUE), max(tsdata, na.rm=TRUE))
			if (yLog&&ylim[1]<0) { 
				tsdata <- tsdata[which(tsdata>0)]
				ylim[1] <- min(tsdata, na.rm=TRUE)
			}
			
		} else {
			ylim <- yLim
		}

		xlim <- c(min(datesAsInt), max(datesAsInt))
		suppressWarnings(plot(x=NULL, y=NULL, log=log, type="p", main=timeMain, xaxt="n", xlab=timeCol, ylab=col[j], 
			xlim=xlim, ylim=ylim, cex.axis=sfPar$time$user$cexAxis, mgp=sfPar$time$user$mgp))

		timeAxis(xtsData, uniqueDates=TRUE, ticks=sfPar$time$user$ticks, gridXY=sfPar$time$user$grid$xy, 
					gridColor=sfPar$time$user$grid$color, gridLty=sfPar$time$user$grid$lty)

		return(datesAsInt)
	}

	smoothData <- function(smoothFunData) {
		
		if (is.null(zLevel)||chart=="p") {
			xtsLineData <- xtsMedianNAfill(objTabXtsData, consolidate=TRUE)
			xtsLineData <- do.call(smoothFunData, list(xtsLineData))
		} else {
			xtsLineData <- NULL
			for (i in 1:length(zLevel)) {
	 			xtsLine <- xtsMedianNAfill(objTabXtsData[,i], consolidate=TRUE)
				xtsLine <- do.call(smoothFunData, list(xtsLine))
				if (is.null(xtsLineData)) {
					xtsLineData <- xtsLine
				} else {
					xtsLineData <- merge(xtsLineData, xtsLine)
				}
				names(xtsLineData)[ncol(xtsLineData)] <- names(objTabXtsData)[i]
			}
		}
		return(xtsLineData)
	}

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	j <- 1
	autoBrowse <- 0
	while (j %in% seq_along(col)) {

		subObjTab <- na.omit(objTab[,c(z, col[j], timeCol),with=FALSE])
		subObjTab <- autoNormalize(subObjTab, col[j], normTable(obj, verbose=FALSE), autoNorm)
		subObjTab <- outlierFilter(semiTableWrap(tab=subObjTab), col[j], k=outlier, output="tab")
		
		if (is.null(zLevel)) {
			colorCut <- cut(subObjTab[[1]],breaks=breaks, labels=FALSE, include.lowest=TRUE)
		} else {
			colorCut <- subObjTab[[1]]
			levelIdx <- zLevel	
			if (is.character(colorCut)) {
				colorCut <- factor(colorCut)
				levelIdx <- sapply(zLevel, function(li)  match(li, levels(colorCut)) )
				colorCut <- as.numeric(colorCut)
			}
		}

		objTabXts <- xts::xts(cbind(subObjTab[[2]], colorCut), as.Date(subObjTab[[3]]))
		dates <- seq(from=zoo::index(objTabXts)[1], to=zoo::index(objTabXts)[nrow(objTabXts)], by=1)
		empty <- xts::xts(,dates)

		if (!newAxisPlot) {
			timeMain <- perColPlotMain(main, numMain, col[j], useDataType, objDataType)
			
			if (yAxisEach&&is.null(main)) {
				for (i in 2:length(col)) {
					timeMain <- paste(timeMain, perColPlotMain(main, numMain, col[i], useDataType, objDataType), sep=" and ")
				}
			}
		}
		
		if (chart=="p") {

			objTabXtsP <- objTabXts
			if (!sequence) { objTabXtsP <- merge(objTabXtsP, empty, all=TRUE) }
			datesAsInt <- seedPlot(objTabXtsP, ylimIdx=1)

			for (i in pDrawOrder) { 
				idxP <- which(zoo::coredata(objTabXtsP[,2])==ifelse(is.null(zLevel),i,levelIdx[i]))
				points(datesAsInt[idxP], zoo::coredata(objTabXtsP[,1])[idxP], pch=funVec(sfPar$time$user$pch,i), col=colorPalette[i]) 
			}
		}

		if (chart!="p"||smooth!=FALSE) {
			
			if (is.null(zLevel)||chart=="p") { 
				objTabXtsData <- objTabXts[,1]
			} else {
				objTabXtsData <- NULL
				for (i in 1:length(zLevel)) {
					if (is.null(objTabXtsData)) {
						objTabXtsData <- objTabXts[which(objTabXts[,2]==levelIdx[i]),][,1]
					} else {
						objTabXtsData <- merge(objTabXtsData, objTabXts[which(objTabXts[,2]==levelIdx[i]),][,1])
					}
					names(objTabXtsData)[ncol(objTabXtsData)] <- sprintf("%s_%s",z, as.character(levelIdx[i]))
				}
			}
			if (!sequence) { objTabXtsData <- merge(objTabXtsData, empty, all=TRUE)  } 
		}

		if (chart=="box") {

			objTabXtsBox <- xtsMedianNAfill(objTabXtsData, consolidate=FALSE)
			datesAsInt <- seedPlot(objTabXtsBox)
			boxplot(zoo::coredata(objTabXtsBox)~datesAsInt, xaxt="n", yaxt="n", xlab="", ylab="", 
				boxfill=sfPar$time$user$colors$box$fill, border=sfPar$time$user$colors$box$border, add=TRUE)
			showLegend <- FALSE
		}

		if (chart=="r"||smooth=="r") {

			smoothFunData <- function(xtsData) { return((zoo::rollapply(zoo::as.zoo(xtsData), rWin, rFun, align=rAlign, partial=TRUE))) }
			xtsLineData <- smoothData(smoothFunData)
		}

		if (chart=="pol"||smooth=="pol") {

			smoothFunData <- function(xtsData) { 
				polyfit <- lm(zoo::coredata(xtsData) ~ poly(1:nrow(xtsData), polDegree, raw=TRUE))
				return(xts::xts(predict(polyfit), zoo::index(xtsData)))
			}
			xtsLineData <- smoothData(smoothFunData)
		}	

		if (chart%in%c("r","pol")||smooth%in%c("r","pol")) {
			x <- 1:length(zoo::index(xtsLineData))

			if (is.null(zLevel)||chart=="p") {

				if (newAxisPlot) {
					par(new=TRUE)
					plot(x, zoo::coredata(xtsLineData), type="l", col=funVec(sfPar$time$user$colors$line, 2)[2], ylim=yLimRight, xaxt="n", xlab="", 
							yaxt="n", ylab="", lwd=sfPar$time$user$lwd)
					axis(4, cex.axis=sfPar$time$user$cexAxis)
					newAxisPlot <- FALSE
				} else {
					if (smooth==FALSE||yAxisEach) {
						seedPlot(xtsLineData, x)
						if (!yAxisEach) { showLegend <- FALSE }
					}
					lines(x, zoo::coredata(xtsLineData), type="l", col=funVec(sfPar$time$user$colors$line, 2)[1], lwd=sfPar$time$user$lwd)
					if (yAxisEach) { newAxisPlot <- TRUE }
				}
			} else {
				seedPlot(xtsLineData, x)
				for (i in 1:length(zLevel)) {
					lines(x, zoo::coredata(xtsLineData[,i]), type="l", col=colorPalette[i], lwd=sfPar$time$user$lwd)
				}
			}

		}

		if (showLegend) {
			if (newAxisPlot) {
				legendColor <- funVec(sfPar$time$user$colors$zLevel, 2)
				legendTitle <- NULL
				legendText <- col
			} else if (!yAxisEach) {
				legendColor <- rev(colorPalette)
				legendTitle <- z
			} else {
				legendColor <- NULL
			}
			if (!is.null(legendColor)) {
				legend(sfPar$time$user$legend$pos, legendText, col=legendColor, title=legendTitle, lty=sfPar$time$user$legend$lty, 
				lwd=sfPar$time$user$legend$lwd, cex=sfPar$time$user$legend$cex, bty=sfPar$time$user$legend$bty,
				seg.len=sfPar$time$user$legend$segLen, pch=sfPar$time$user$legend$pch, 
				pt.cex=sfPar$time$user$legend$ptCex)
			}
		}

		if (length(col)>1&&!yAxisEach) { 
			j <- plotBrowsing(j, idxMax=length(col), autoBrowse=autoBrowse)
			autoBrowse <- j$auto
			j <- j$idx
			if (is.na(j)) { break }
		}
		j <- j + 1
	}

	return(invisible())
}

#' Scatter Plot
#'
#' @description Plots xy scatter plot of a \code{semiTable} or \code{semiFrame} data column. Chart colors are based
#' on \code{z} axis column. 
#'
#' @template stsfobjParam
#' @param ... Data columns to be used. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' First column specifies x axis. Second column specifies y axis. Browsing mode is activated if multiple 
#' y axis columns are provided.
#' @template outlierParam
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template zOutlierParam
#' @template mainParam
#' @param linFit Logical. Adds linear fit to the chart (default: \code{FALSE}).
#' @template numMainParam
#' @template showLegendParam
#' @template yLogParam
#' @template xLimParam
#' @template yLimParam
#' @template autoNormParam
#' @template drawOrderParam
#' @template useDataTypeParam
#' @template colParam
#' 
#' @template xyPlotExamples
#'
#' @return Plot.
#' 
#' @export
xyPlot <- function(obj, ..., outlier=NULL, z=c("ProbeYield", "UPYield"), zBr=10, zBrMethod="yieldHeat", zLevel=NULL, zOutlier=NULL, main=NULL, 
								linFit=FALSE, numMain=TRUE, showLegend=TRUE, yLog=FALSE, xLim=NULL, yLim=NULL, 
								autoNorm=TRUE, drawOrder=NULL, useDataType=sfUseDataType, col=NULL) {
	
	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType
	if (length(col)<2) { stop("Provide x and y parameters") }
	z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=z)$col[1]
	zCol <- z

	if (is.null(zLevel)) {
		objTab <- tab(outlierFilter(obj, z, zOutlier))
		objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
		breaks <- breakData(objTab[[z]], zBr, zBrMethod)
		zBr <- length(breaks)-1
		colorPalette <- rev(funVec(sfPar$xy$user$colors$zBr, zBr))
		colorPalette <- colorAlpha(colorPalette, sfPar$xy$user$colors$alpha$zBr)
		legendText <- rev(makeIntervals(breaks, sfPar$xy$user$legend$signif))
		legendColor <- colorPalette
	} else {
		objTab <- tab(obj)
		zLevel <- processLevel(objTab, z, zLevel)
		zBr <- length(zLevel)
		colorPalette <- funVec(sfPar$xy$user$colors$zLevel, zBr)
		colorPalette <- colorAlpha(colorPalette, sfPar$xy$user$colors$alpha$zLevel)
		legendText <- rev(as.character(zLevel))
		legendColor <- rev(colorPalette)
	}
	pch <- funVec(sfPar$xy$user$pch, zBr)
	x <- col[1]
	xCol <- x
	col <- col[-1]

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	j <- 1
	autoBrowse <- 0
	while (j %in% seq_along(col)) {
		y <- col[j]

		yCol <- y
		
		subObjTabColName <- c(zCol, xCol, yCol)
		colDupl <- which(duplicated(subObjTabColName))
		if (length(colDupl)) {
			for (i in colDupl) { subObjTabColName[i] <- paste0(subObjTabColName[i], i) }
		}
		xCol <- subObjTabColName[length(subObjTabColName)-1]
		yCol <- subObjTabColName[length(subObjTabColName)]

		subObjTab <- na.omit(setnames(objTab[,c(z, x, y), with=FALSE], subObjTabColName))
		subObjTab <- autoNormalize(subObjTab, c(xCol, yCol), normTable(obj, verbose=FALSE), autoNorm)
		subObjTab <- outlierFilter(semiTableWrap(tab=subObjTab), xCol, k=outlier, output="tab")
		subObjTab <- outlierFilter(semiTableWrap(tab=subObjTab), yCol, k=outlier, output="tab")
		
		if (is.null(zLevel)) {
			colorCut <- cut(subObjTab[[zCol]], breaks=breaks, labels=FALSE, include.lowest=TRUE)
			colorCutLevel <- zBr:1
		} else {
			colorCut <- subObjTab[[1]]
			colorCutLevel <- zLevel	
		}

		drawOrderIdx <- seq_along(colorCutLevel)
		if (!is.null(drawOrder)) {
			if (length(drawOrder)!=length(drawOrderIdx)) { stop("Incompatible drawOrder length.") }
			drawOrderIdx <- length(drawOrder) - drawOrder + 1
		}

		subObjTab[,c(zCol):=list(colorCut)]

		xyMain <- perColPlotMain(main, numMain, x, useDataType, objDataType, 
				addEnd=paste0(" vs ", perColPlotMain(main, numMain, y, useDataType, objDataType)))

		subObjTabLevel <- subObjTab[subObjTab[[zCol]]==colorCutLevel[drawOrderIdx[1]]]
		
		if (is.null(xLim)) {
			xLim <- c(min(subObjTab[[xCol]], na.rm=TRUE), max(subObjTab[[xCol]], na.rm=TRUE))
		}
		if (is.null(yLim)) {
			yLimj <- c(min(subObjTab[[yCol]], na.rm=TRUE), max(subObjTab[[yCol]], na.rm=TRUE))
		} else {
			yLimj <- yLim
		}

		plot(subObjTabLevel[[xCol]], subObjTabLevel[[yCol]], col=colorPalette[drawOrderIdx[1]], 
				pch=pch[1], xlab=x, ylab=y, main=xyMain, log=ifelse(yLog, "y", ""),
				xlim=xLim, ylim=yLimj)

		for (i in drawOrderIdx[-1]) { 
			subObjTabLevel <- subObjTab[subObjTab[[zCol]]==colorCutLevel[i]]
			points(subObjTabLevel[[xCol]], subObjTabLevel[[yCol]], col=colorPalette[i], pch=pch[i]) 
		}

		if (linFit) {
			abline(lm(subObjTab[[yCol]]~subObjTab[[xCol]]), col=sfPar$xy$user$linFit$color, lwd=sfPar$xy$user$linFit$lwd)
		}

		if (showLegend) {
			legend(sfPar$xy$user$legend$pos, legendText, col=legendColor, title=z, lty=sfPar$xy$user$legend$lty, 
				lwd=sfPar$xy$user$legend$lwd, cex=sfPar$xy$user$legend$cex, bty=sfPar$xy$user$legend$bty,
				seg.len=sfPar$xy$user$legend$segLen, pch=sfPar$xy$user$legend$pch, 
				pt.cex=sfPar$xy$user$legend$ptCex)
		}

		if (length(col)>1) { 
			j <- plotBrowsing(j, idxMax=length(col), autoBrowse=autoBrowse)
			autoBrowse <- j$auto
			j <- j$idx
			if (is.na(j)) { break }
		}
		j <- j + 1

	}
	return(invisible())
}

#' Box Plot
#'
#' @description Plots box plot of a \code{semiTable} or \code{semiFrame} data column.
#'
#' @template stsfobjParam
#' @param ... Data columns to be used. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' @template outlierParam
#' @template xBrParams
#' @template xLevelParam
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template zOutlierParam
#' @template mainParam
#' @template numMainParam
#' @template showLegendParam
#' @template autoNormParam
#' @template useDataTypeParam
#' @template colParam
#'
#' @template boxPlotExamples
#' 
#' @seealso \code{\link{dataType}} for available data types. 
#' 
#' @return Plot.
#' 
#' @export
boxPlot <- function(obj, ..., outlier=NULL, xBr=5, xBrMethod="linear", xLevel=NULL, 
						z=NULL, zBr=10, zBrMethod="yieldHeat", zLevel=NULL, zOutlier=NULL, 
						main=NULL, numMain=TRUE, showLegend=TRUE, autoNorm=TRUE, useDataType=sfUseDataType, col=NULL) {
	
	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType
	if (length(col)<2) { stop("Provide x and y parameters") }
	x <- col[1]
	xCol <- x
	col <- col[-1]

	objTab <- tab(obj)

	if (is.null(xLevel)) {
		objTab <- outlierFilter(semiTableWrap(tab=objTab, dataType=dataType(obj)), x, outlier, output="tab")
		objTab <- autoNormalize(objTab, x, normTable(obj, verbose=FALSE), autoNorm)
		xBreaks <- breakData(objTab[[x]], xBr, xBrMethod)
		xBr <- length(xBreaks)-1
	} else {
		xLevel <- processLevel(objTab, x, xLevel)
		xBr <- length(xLevel)
		objTab <- objTab[objTab[[x]]%in%xLevel]
	}
	
	subObjTabCol <- x

	if (is.null(z)) {
		showLegend <- FALSE
		colorPalette <- funVec(sfPar$box$user$colors$zBr, 1)
		zBr <- 1
		zCol <- character()
	} else {
		z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=z)$col[1]
		zCol <- z
		subObjTabCol <- c(z, subObjTabCol)
		if (is.null(zLevel)) {
			objTab <- outlierFilter(semiTableWrap(tab=objTab, dataType=dataType(obj)), z, zOutlier, output="tab")
			objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
			breaks <- breakData(objTab[[z]], zBr, zBrMethod)
			zBr <- length(breaks)-1
			colorPalette <- funVec(sfPar$box$user$colors$zBr, zBr)
			legendText <- rev(makeIntervals(breaks, sfPar$box$user$legend$signif))
		} else {
			zLevel <- processLevel(objTab, zCol, zLevel)
			zBr <- length(zLevel)
			colorPalette <- funVec(sfPar$box$user$colors$zLevel, zBr)
			legendText <- rev(as.character(zLevel))
		}
		legendColor <- rev(colorPalette)
	}	

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))
	par(mar=sfPar$box$user$mar)

	j <- 1
	autoBrowse <- 0
	while (j %in% seq_along(col)) {
		y <- col[j]
		yCol <- y

		subObjTabColName <- c(zCol, xCol, yCol)
		colDupl <- which(duplicated(subObjTabColName))
		if (length(colDupl)) {
			for (i in colDupl) { subObjTabColName[i] <- paste0(subObjTabColName[i], i) }
		}
		xCol <- subObjTabColName[length(subObjTabColName)-1]
		yCol <- subObjTabColName[length(subObjTabColName)]

		subObjTab <- na.omit(setnames(objTab[, c(subObjTabCol, y), with=FALSE], subObjTabColName))
		subObjTab <- autoNormalize(subObjTab, yCol, normTable(obj, verbose=FALSE), autoNorm)
		subObjTab <- outlierFilter(semiTableWrap(tab=subObjTab), yCol, k=outlier, output="tab")

		if (is.null(xLevel)) {
			colorCut <- cut(subObjTab[[xCol]], breaks=xBreaks, labels=FALSE, include.lowest=TRUE)
			NAidx <- which(is.na(colorCut))
			if (length(NAidx)) {
				subObjTab <- subObjTab[-NAidx]
				colorCut <- colorCut[-NAidx]
			}
			
			if ("POSIXct"%in%class(subObjTab[[xCol]])) {
				xAxis <- .POSIXct(character())
				for (levCut in unique(colorCut)) {
					xAxis <- c(xAxis, median(subObjTab[levCut==colorCut][[xCol]]))
				}
				xAxis <- format(xAxis, "%y/%m")
			} else {
				xAxis <- as.character(signif(tapply(subObjTab[[xCol]], colorCut, median), sfPar$box$user$xSignif))
			}
		} else {
			colorCut <- subObjTab[[xCol]]
			xAxis <- xLevel
		}
		subObjTab[,c(xCol):=list(colorCut)]
		
		boxMain <- perColPlotMain(main, numMain, x, useDataType, objDataType, 
				addEnd=paste0(" vs ", perColPlotMain(main, numMain, y, useDataType, objDataType)))
		
		if (is.null(z)) {
			boxColor <- colorPalette
		} else {
			if (is.null(zLevel)) {
				colorCut <- cut(subObjTab[[zCol]], breaks=breaks, labels=FALSE)
			} else {
				colorCut <- subObjTab[[zCol]]
			}
			subObjTab[,c(zCol):=list(colorCut)]
			
			xzTab <- table(subObjTab[[xCol]], subObjTab[[zCol]])
			xzTab <- xzTab/rowSums(xzTab)
			
			boxColor <- colorPalette[apply(xzTab, 1, function(row) { which.max(row) } )]
		}

		boxplot(subObjTab[[yCol]]~subObjTab[[xCol]], main=boxMain, xaxt="n", xlab="", ylab=y, col=boxColor, 
			border=sfPar$box$user$colors$border, boxwex=sfPar$box$user$boxWex, varwidth=sfPar$box$user$varWidth)
		axis(1, at=1:xBr, labels=xAxis, las=sfPar$box$user$axis$x$las, cex.axis=sfPar$box$user$axis$x$cex)
		title(xlab=x, line=sfPar$box$user$axis$x$line)

		if (showLegend) {
			legend(sfPar$box$user$legend$pos, legendText, col=legendColor, title=z, lty=sfPar$box$user$legend$lty, 
				lwd=sfPar$box$user$legend$lwd, cex=sfPar$box$user$legend$cex, bty=sfPar$box$user$legend$bty,
				seg.len=sfPar$box$user$legend$segLen, pch=sfPar$box$user$legend$pch, 
				pt.cex=sfPar$box$user$legend$ptCex)
		}

		if (length(col)>1) { 
			j <- plotBrowsing(j, idxMax=length(col), autoBrowse=autoBrowse)
			autoBrowse <- j$auto
			j <- j$idx
			if (is.na(j)) { break }
		}
		j <- j + 1

	}
	return(invisible())
}


#' Model Plot
#'
#' @description Provides model visualizations. 
#'
#' @template stsfobjParam
#' @param chart Character. Type of visualization. 
#' 
#' CART (\code{cart}) model relevant types:
#' \itemize{
#'   \item{ "default" }{ Plots rpart decision tree. For more details see \code{?rpart.plot::prp} (default). }
#'   \item{ "plotmo" }{ Plots model's response when varying one or two predictors while
#'     holding the other predictors constant. For more details see \code{?plotmo::plotmo}. }
#'   \item{ "importance" }{ Plots predictors importance score calculated by mean gini decrease. }
#' }
#' Random forest (\code{forest}) model relevant types:
#' \itemize{
#'   \item{"default" }{ Plots predictors importance score calculated by mean gini decrease (default). }
#'   \item{ "plotmo" }{ Plots model's response when varying one or two predictors while
#'     holding the other predictors constant. For more details see \code{?plotmo::plotmo}. }
#'   \item{ "error" }{ Plots the error rates. }
#' }
#' XGBoost (\code{xboost}) model relevant types:
#' \itemize{
#'   \item{"default" }{ Plots predictors importance score (default). }
#'   \item{ "deepness" }{ Plots model's complexity. For more information see \code{?xgboost::xgb.plot.deepness}. }
#'   \item{ "trees" }{ Exports model's ensemble visualization diagram. For more information see \code{?xgboost::xgb.plot.multi.trees}. }
#' }
#' PCA (\code{pca}) model relevant types:
#' \itemize{
#'   \item{"default" }{ Plots percent of data variance explained by Principal Components (default). }
#'   \item{ "cor" }{ Plots rescaled loadings ie. correlations for a specified Principal Component. }
#'   \item{ "exp" }{ Plots squared eigenvector components ie. variables contribution to a specified Principal Component. }
#' }
#' @template mainParam
#' @param color Character. Color modes used in relevant chart types:
#' \itemize{
#'   \item{ "default" }{ Default coloring scheme. }
#'   \item{ "heat" }{ Heatmap coloring scheme (default). }
#'   \item{ "majority" }{ Binary response majority coloring scheme. }
#' }
#' @template dataParam
#' @param snip Logical. Allows to interactively trim the tree with mouse (default: \code{FALSE}). 
#' @template positiveIdxParam 
#' @param extra Numeric. Show extra information in the CART nodes. For more details see \code{?rpart.plot::prp} (default: 108).
#' @param varlen Integer. Length of variable names in text at the CART splits. For more details see \code{?rpart.plot::prp} (default: 0).
#' @param faclen Integer. Length of factor level names in the CART splits. For more details see \code{?rpart.plot::prp} (default: 0).
#' @template yLimParam
#' @param file Character. Passes file name for \code{xboost} \code{chart="trees"} export. If \code{NULL}
#' opens in browser directly (default: \code{NULL}).
#' @param first Integer. Specifies first n values for relevant charts to display (default: \code{NULL}).
#' @param PComp Integer. Specifies Principal Component, relevant for PCA models (default: \code{NULL}).
#' @param ... Passes parameters to relevant rendering functions.
#'
#' @template modelPlotExamples
#'
#' @return If snip mode is enabled, object with snipped CART is returned. Plot otherwise.
#' 
#' @export
modelPlot <- function(obj, chart="default", main=NULL, color="heat", data="last", snip=FALSE, positiveIdx=2, extra=108, varlen=0,
						faclen=0, ylim=NULL, file=NULL, first=NULL, PComp=NULL, ...) {

	checkCharInput(chart, c("default", "plotmo", "importance", "error", "deepness", "trees", "cor", "exp"))	
	checkCharInput(color, c("default", "heat", "majority"))	
	data <- checkData(obj$aid, data)
	
	defaultMain <- function(main) {
		if (!is.null(main)) { return(main) }
		return(paste(paste(unique(obj$tab[[sfName$tech]]), collapse=", "), 
				paste(unique(obj$tab[[sfName$maskset]]), collapse=", "),sep=", "))
	}

	type <- obj$aid$model[[data]]$type
	model <- obj$aid$model[[data]]$model
	dependLevel <- obj$aid$model[[data]]$dependLevel
	dependCol <- obj$aid$model[[data]]$dependCol
	method <- obj$aid$model[[data]]$method
	method <- ifelse(is.null(method), character(1), method)
	m <- modelM(obj)

	# argsList <- eval(substitute(alist(...)))

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	if (model%in%c("cart", "forest")&&chart=="plotmo") {

		main <- defaultMain(main)

		nresponse <- NA
		plotmoType <- NULL

		ylab <- dependCol
		if (type=="class") {
			nresponse <- dependLevel[positiveIdx]
			ylab <- nresponse
			plotmoType <- "prob"
			if (is.null(ylim)) { ylim <- c(0,1) }
		}

		plotmo::plotmo(m, type=plotmoType, nresponse=nresponse, caption=main, ylab=ylab, ylim=ylim, ...)
		return(invisible())
	}

	if (model=="cart") {

		main <- defaultMain(main)

		if (chart=="importance") {

			par(mar=c(5.1,10.1, 4.1, 2.1))
			plot(rev(m$variable.importance), 1:length(m$variable.importance), main=main, yaxt="n", ylab="", xlab="Importance")
			axis(2, at=1:length(m$variable.importance), labels=rev(names(m$variable.importance)), las=2)

			return(invisible())
		}

		if (method=="anova") { 
			if (extra==106) { extra <- 0 }
		}

		boxColor <- sfPar$model$user$colors$cart$default$box
		borderColor <- sfPar$model$user$colors$cart$default$border
		
		if (type=="class"&&obj$aid$model[[data]]$br!=2) {
			if (extra==106) { extra <- 0 }
		}

		if (type=="class"&&obj$aid$model[[data]]$br==2) {
			
			textColor <- funVec(sfPar$model$user$heat$textColors, 1)
			if (color=="heat") {
				breaks <- funVec(sfPar$model$user$heat$breaks, sfPar$model$user$heat$n)
				nodeClass <- m$frame$yval2[, 5]
				for (i in 1:length(nodeClass)) { nodeClass[i] <- which.min(nodeClass[i]>breaks)-1 }
				boxColor <- funVec(sfPar$model$user$heat$colors, sfPar$model$user$heat$n)
				textColor <- funVec(sfPar$model$user$heat$textColors, sfPar$model$user$heat$n)
			} else if (color=="majority") {
				nodeClass <- m$frame$yval
				boxColor <- sfPar$model$user$colors$cart$br2$box
			}

			if (color!="default") {
				if (names(findDataType(obj,obj$aid$model[[data]]$dependCol))==sfDataType$clust) {
					boxColor <- rev(boxColor)
				}
				boxColor <- boxColor[nodeClass]
				textColor <- textColor[nodeClass]
				borderColor <- boxColor
			}
		}

		if (type=="reg") {
			textColor <- 1
			extra <- FALSE
		}

		snipm <- rpart.plot::prp(m, snip=snip, main=main, extra=extra, varlen=varlen, faclen=faclen, box.col=boxColor, 
						border.col=borderColor, col=textColor, fallen.leaves=TRUE, type=4, round=1, under.percent=0, roundint=FALSE, ...)
		if (snip) {
			obj$aid$model[[data]]$m <- snipm$obj
			return(obj)
		}
	}

	if (model=="forest") {

		main <- defaultMain(main)

		if (chart=="error") {
			randomForest:::plot.randomForest(m, main=main, ...)
			return(invisible())			
		}

		if (is.null(first)) {
			n.var <- min(30, nrow(m$importance))
		} else {
			n.var <- first
		}

		randomForest::varImpPlot(m, main=main, n.var=n.var, ...)
	}

	if (model=="boost") {

		main <- defaultMain(main)

		par(las=1)
		par(cex.axis=sfPar$model$user$cexAxis$boost)
		linch <-  max(strwidth(colnames(obj), "inch")+0.4, na.rm = TRUE)
		par(mai=c(1.02,linch,0.82,0.42))

		importance <- gbm::summary.gbm(m, main=main, plotit=FALSE, ...)

		if (!is.null(first)) {
			importance <- importance[1:first,]
		}

		importance <- importance[nrow(importance):1,]

		barplot(importance[,2], horiz=TRUE, names.arg=importance[,1], main=main)

	}
	if (model=="xboost") {

		main <- defaultMain(main)

		if (chart=="default") {
	
			importance <- xgboost::xgb.importance(obj$aid$model[[data]]$featureName, model=m)
			
			if (!is.null(first)) {
				importance <- importance[1:first,]
			}
			
			xgboost::xgb.plot.importance(importance, main=main)
		}

		if (chart=="deepness") {

			xgboost::xgb.plot.deepness(m, main=main)
		}

		if (chart=="trees") {

			trees <- xgboost::xgb.plot.multi.trees(obj$aid$model[[data]]$featureName, model=m, ...)	
			if (is.null(file)) {
				print(trees)
			} else {
				htmltools::save_html(trees, file=file)
			}
		}
	}

	if (model=="pca") {

		if (chart=="cor" || chart=="exp") {

			if (is.null(PComp)) {
				stop("Specify Principal Component 'PComp'")
			}
			
			if (chart=="cor") {
				
				loadings <- m$rotation[,PComp]*m$sdev[PComp]
				loadings <- sort(loadings)

				if (!is.null(first)) {
					first <- floor(first/2)
					if (first>floor(length(loadings)/2)) { first <- floor(length(loadings)/2) }
					loadings <- c(loadings[1:first], loadings[(length(loadings)-first+1):length(loadings)])
				}

				mainPCA <- "PCA: Rescaled Loadings of PComp%s"
				xlab <- "Variable-PComp%s Correlation"
				subMain <- NULL

			} else {

				loadings <- m$rotation[,PComp]^2
				loadings <- sort(100*loadings)
				
				if (!is.null(first)) {
					if (first>length(loadings)) { first <- length(loadings) }
					loadings <- loadings[(length(loadings)-first+1):length(loadings)]
				}

				mainPCA <- "PCA: Squared Eigenvector Components of PComp%s"
				xlab <- "%% of PComp%s's Variance Explained by Variable"
				subMain <- sprintf("Total: %s%%", as.character(round(sum(loadings), 1)))
			}

			if (is.null(main)) {
				main <- sprintf(mainPCA, as.character(PComp))
			}
			xlab <- sprintf(xlab, as.character(PComp))

			par(las=1)
			par(cex.axis=sfPar$model$user$cexAxis$dr)
			linch <-  max(strwidth(colnames(obj), "inch")+0.4, na.rm = TRUE)
			par(mai=c(1.02,linch,0.82,0.42))

			barplot(loadings, horiz=TRUE, las=1, axis.lty=1, space=0,
				main=main, xlab=xlab, sub=subMain,
				col=sfPar$model$default$colors$dr$box, border=sfPar$model$default$colors$dr$border)


		} else {

			importance <- summary(m)$importance
			
			if (is.null(main)) {
				main <- paste("PCA: % of Data Variance Explained by PComps")
			}

			if (!is.null(first)) {
				importance <- importance[,1:first]
			}
			par(mar=c(5.1, 4.1, 5.1, 4.1))
			histObj <- list(breaks=(1:(ncol(importance)+1)-0.5), mids=1:ncol(importance), equidist=TRUE, density=importance[2,])
			class(histObj) <- "histogram"
			plot(histObj, freq=FALSE, xlab="PComp", ylab=NA, main=main, 
				col=sfPar$model$default$colors$dr$box, border=sfPar$model$default$colors$dr$border)

			mtext("% of Data Variance Explained by PComp", side=2, line=3, col=sfPar$model$default$colors$dr$box)

			par(new=TRUE)
			plot(x=1:ncol(importance), y=importance[3,], type="l", xaxt="n", yaxt="n", xlab=NA, ylab=NA, col=sfPar$model$default$colors$dr$line, lwd=2)
			axis(4)
			mtext("Cumulative % of Data Variance Explained", side=4, line=3, col=sfPar$model$default$colors$dr$line)

		}
	}

	return(invisible())
}

#' Partial Dependence Plot
#'
#' Plots response's partial dependence by either sampling from the data after variable's distribution 
#' adjustments or by using a model for averaging through the data.
#'
#' @template stsfobjParam
#' @param ... Data columns to be used as \code{x} variable. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' @param method Character. Partial plot method:
#' \itemize{
#'   \item{ "model" }{ Previously trained model is used for averaging through the data at points specified by \code{xInput} (default). }
#'   \item{ "data" }{ Adjusted distribution shifting is used for evaluating estimates at distribution's centers specified by \code{xInput}. }
#'   \item{ "modelForest" }{ Same as "model" method, except non-parallelized function from the \code{randomForest} package is used for averaging. }
#' }
#' @param xInput Numeric. Input points for estimates corresponding to \code{x} variable. If single value
#' is provided (\code{length(xInput)==1}) variable's range is divided by this number (default: 5). Depending on \code{method} these inputs are:
#' \itemize{
#'   \item{ for "model": }{ points. }
#'   \item{ for "data": }{ distribution centers. }
#' }
#' @template xNarrowParam
#' @template xCenterFunParam
#' @template xSizeRatioParam
#' @param y Character. Response column to estimate (default: "ProbeYield").
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template dataParam
#' @template zCenterParam
#' @template zNarrowParam 
#' @template zCenterFunParam
#' @template zSizeRatioParam
#' @param positiveIdx Numeric. Determines whether maximum or minimum is assigned
#' to the green color. Possible values 1 or 2 (default: 2).
#' @param classProb Logical. Relevant for \code{method="model"}. If TRUE, computes probability from logit value (default: \code{TRUE}).
#' @param estimateFun Character. Estimate function for \code{method="data"} (default: \code{mean}).
#' @param confLevel Numeric. Confidence interval settings (default: 0.95).
#' @param bootR Numeric. The number of bootstrap replicates (default: 999).
#' @template mainParam
#' @template numMainParam
#' @param showLegend Logical. When \code{TRUE}, legend is shown (default: \code{TRUE}).
#' @template autoNormParam 
#' @template useDataTypeParam 
#' @template colParam
#' @template coreParam 
#'
#' @template partialPlotExamples
#'
#' @return Plot.
#' 
#' @export
partialPlot <- function(obj, ..., method="model", xInput=5, xNarrow=NULL, xCenterFun=median, xSizeRatio=ifelse(is.null(xNarrow), 0.5, xNarrow),
							y="ProbeYield",
							z=NULL, zBr=2, zBrMethod="median", zLevel=NULL,  
							zCenter=NULL, zNarrow=NULL, zCenterFun=median, zSizeRatio=ifelse(is.null(zNarrow), 0.5, zNarrow), 
							data="last", positiveIdx=2, classProb=TRUE, estimateFun=mean,
							confLevel=0.95, bootR=999, main=NULL, numMain=TRUE, showLegend=TRUE, autoNorm=TRUE, 
							useDataType=sfUseDataType, col=NULL, core=1) {
	
	
	checkCharInput(method, c("model", "data", "modelForest"))

	if (method=="model") { data <- checkData(obj$aid, data) }
	
	computeModelPartialForest <- function() {
		return(eval(substitute(randomForest::partialPlot(modelM(obj), predData[zRow], var.name, 
						which.class=whichClass, plot=FALSE), list(var.name=col))))
	}

	computeModelPartial <- function(modelName, modelType) {
		
		if (modelName=="forest") {

			if (modelType=="class") {
				type <- "prob"
			} else {
				type <- "response"
			}
			modelFun <- randomForest:::predict.randomForest

		} else if (modelName=="svm") {

			if (modelType=="class") {
				type <- "probabilities"
			} else {
				type <- "response"
			}
			modelFun <- kernlab:::predict

		} else if (modelName=="xboost") {

			type <- NULL
			modelFun <- function(...) {
				argsList <- eval(substitute(alist(...)))
				argsList$newdata <- as.matrix(argsList$newdata)
				return(do.call(xgboost:::predict.xgb.Booster, argsList))
			}

		} else {
			stop(sprintf("model %s not supported", modelName))		
		}

		partialLoopGut <- function(x) {
			
			modelFunPar$newdata <- predData
			modelFunPar$newdata[,col] <- x
			y <- do.call(modelFun, modelFunPar) 
			if (is.matrix(y)) { y <- y[,positiveIdx]  }
			y <- mean(y)
			return(c(x, y))
		}

		modelFunPar <- list(modelM(obj))
		modelFunPar$type <- type
		predData <- predData[zRow]
		xInput <- inputThroughRange(xInput, c(min(predData[[col]]), max(predData[[col]])))
		
		if (core>1) {
			
			registerDoMC(core)
			on.exit(registerDoSEQ())

			xy <- foreach(x=xInput, .combine=rbind) %dopar% {
				partialLoopGut(x)					
			}

		} else {

			xy <- t(sapply(xInput, function(x) partialLoopGut(x) ))
		}

		return(list(x=xy[,1], y=xy[,2]))
	}

	computeDataPartial <- function() {
		objZRow <- obj[zRow]
		xInput <- inputThroughRange(xInput, c(min(objZRow[[col]])+IQR(objZRow[[col]]), max(objZRow[[col]])-IQR(objZRow[[col]])))
		dsObj <- distrShiftPlot(objZRow, col=col, xCenter=xInput, xNarrow=xNarrow, xCenterFun=xCenterFun, xSizeRatio=xSizeRatio, plot=FALSE)
		shiftMat <- dsObj$mats[[1]]
		sampleSize <- ncol(shiftMat)
		colData <- obj$tab[[y]][zRow]

		bootFun <- function(data) {  
			sapply(1:length(data), function(i) { do.call(estimateFun, list(data[[i]], na.rm=TRUE)) } ) 
		}

		bootRanGen <- function(data, mle) {
			return(lapply(1:length(data),function(i) {  sample(data[[i]], sampleSize, replace=TRUE) } ))
		}

		colData <- lapply(1:nrow(shiftMat), function(i) { colData[shiftMat[i,]] } )
		bootObj <- boot::boot(colData, bootFun, R=bootR, sim="parametric", ran.gen=bootRanGen)
		bootObj <- boot::envelope(bootObj, level=ifelse(is.null(confLevel),0.95, confLevel))

		return(list(x=dsObj$center, y=bootObj$overall))
	}

	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType

	col <- col[1]
	if (!is.null(z)) {
		z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=z)$col[1]
	}

	objTab <- tab(obj)

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))
	
	if (is.null(zCenter)) {
		if (is.null(z)) {
			showLegend <- FALSE
			colorPalette <- funVec(sfPar$partial$user$colors$zLevel, 2)
			zBr <- 1
		} else {
			
			if (is.null(zLevel)) {
				objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
				breaks <- breakData(objTab[[z]], zBr, zBrMethod)
				zBr <- length(breaks)-1
				colorPalette <- funVec(sfPar$partial$user$colors$zBr, zBr)
				legendText <- rev(makeIntervals(breaks, sfPar$partial$user$legend$signif))
			} else {
				zLevel <- processLevel(objTab, z, zLevel)
				zBr <- length(zLevel)
				colorPalette <- funVec(sfPar$partial$user$colors$zLevel, zBr)
				legendText <- rev(as.character(zLevel))
			}
			
			if (is.null(zLevel)) {
				colorCut <- cut(objTab[[z]], breaks=breaks, labels=FALSE)
			} else {
				colorCut <- objTab[[z]]	
			}
		}
	} else {
		zRowsCentre <- distrShiftPlot(obj, col=z, xCenter=zCenter, xNarrow=zNarrow, xCenterFun=zCenterFun, 
						xSizeRatio=zSizeRatio, plot=FALSE)$mats[[1]]
		zBr <- nrow(zRowsCentre)
		colorPalette <- funVec(sfPar$partial$user$colors$zBr, zBr)
		legendText <- rev(as.character(zCenter))
	}


	main <- perColPlotMain(main, numMain, col[1], useDataType, objDataType, addStart="Partial Dependence on ")

	if (method=="model") {
		xlab <- col
		if (obj$aid$model[[data]]$type=="class") {
			whichClass <- obj$aid$model[[data]]$dependLevel[positiveIdx]
			ylab <- whichClass
		} else {
			whichClass <- NULL
			ylab <- obj$aid$model[[data]]$dependCol
			classProb <- FALSE
		}
	} else {
		xlab <- paste(col, "center")
		ylab <- paste(y, "estimate")
	}

	predData <- objTab[, objDataType[[useDataType]], with=FALSE]

	plotData <- list()
	xlim <- c(+Inf, -Inf)
	ylim <- xlim 
	for (i in 1:zBr) {

		if (is.null(zCenter)) {
			zRow <- TRUE
			if (!is.null(z)) {
				zRow <- which(colorCut==ifelse(is.null(zLevel), i, zLevel[i]))	
			}
		} else {
			zRow <- zRowsCentre[i, ]	
		}
		
		if (method=="model") {

			if (obj$aid$model[[data]]$model=="forest"&&method=="modelForest") {
				
				plotData[[i]] <- computeModelPartialForest()
				if (classProb) {
					plotData[[i]]$y <- 1/(1+exp(-plotData[[i]]$y))
				}

			} else {

				plotData[[i]] <- computeModelPartial(obj$aid$model[[data]]$model,
									obj$aid$model[[data]]$type)

			}

			if (autoNorm) {
				plotData[[i]]$x <- autoNormalize(setnames(data.table(plotData[[i]]$x), col), col, 
										normTable(obj, verbose=FALSE), autoNorm)[[1]]
			}
		} else {
			plotData[[i]] <- computeDataPartial()
		}

		xlim <- c(min(xlim[1], min(plotData[[i]]$x)), max(xlim[2], max(plotData[[i]]$x)))
		ylim <- c(min(ylim[1], min(plotData[[i]]$y)), max(ylim[2], max(plotData[[i]]$y)))
	}

	if (method=="model") {
		plot(plotData[[1]]$x, plotData[[1]]$y, type="l", col=colorPalette[1], lwd=sfPar$partial$user$lwd, 
					main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
		plotData[[1]] <- NULL

		if (length(plotData)) {
			for (i in 1:length(plotData)) {
				lines(plotData[[i]]$x, plotData[[i]]$y, col=colorPalette[i+1], lwd=sfPar$partial$user$lwd)
			}
		}
	} else {
		plot(x=NULL, y=NULL, type="p", main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
		for (i in 1:length(plotData)) {
			if (!is.null(confLevel)) {
				polygon(c(plotData[[i]]$x, rev(plotData[[i]]$x)), c(plotData[[i]]$y[1,],rev(plotData[[i]]$y[2,])), 
					col=sfPar$partial$user$colors$confInterv , border=FALSE)
			}
			lines(plotData[[i]]$x, colMeans(plotData[[i]]$y), col=colorPalette[i], lwd=sfPar$partial$user$lwd)
		}
	}


	if (showLegend) {
		legend(sfPar$partial$user$legend$pos, legendText, 
			col=colorPalette[zBr:1], title=paste(z,ifelse(is.null(zCenter), "", "center")), lty=sfPar$partial$user$legend$lty, 
			lwd=sfPar$partial$user$legend$lwd, cex=sfPar$partial$user$legend$cex, bty=sfPar$partial$user$legend$bty,
			seg.len=sfPar$partial$user$legend$segLen, pch=sfPar$partial$user$legend$pch, 
			pt.cex=sfPar$partial$user$legend$ptCex)
	}

	return(invisible())
}

#' Distribution Shifting and Narrowing Plot
#'
#' @description Plots shifted and narrowed distribution of a variable possibly depending on \code{z} variable's
#' breaks or shifting and narrowing as well.
#'
#' @template stsfobjParam
#' @param ... Data columns to be used as \code{x} variable. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' @param chart Character. Type of chart:
#' \itemize{
#'   \item{ "hist" } { Plots histogram layers. }
#'   \item{ "density" } { Plots density layers (default). }
#' } 
#' @param xCenter Numeric. Centers of \code{x} variable's shifted distribution. If more than one center point
#' is provided browsing mode will be initiated (default: \code{NULL}).
#' @template xNarrowParam
#' @template xCenterFunParam
#' @template xSizeRatioParam
#' @template zParam
#' @template zBrParams
#' @template zLevelParam
#' @template zCenterParam
#' @template zNarrowParam 
#' @template zCenterFunParam
#' @template zSizeRatioParam
#' @param freq Logical. When \code{TRUE}, \code{y} axis provides density, frequency otherwise (default: \code{FALSE}).
#' @template mainParam
#' @template showLegendParam
#' @param zShowLegend Logical. When \code{TRUE} legend corresponding to \code{z} variable is displayed (default: \code{TRUE}). 
#' @template autoNormParam 
#' @template useDataTypeParam 
#' @param plot Logical. When \code{FALSE}, function returns shifted distribution's data (default: \code{TRUE}).
#' @template colParam
#'
#' @template distrShiftPlotExamples
#'
#' @return Plot by default, if \code{plot=FALSE} function returns shifted distribution's data.
#' 
#' @export
distrShiftPlot <- function(obj, ..., chart="density", xCenter=NULL, xNarrow=NULL, xCenterFun=median, 
							xSizeRatio=ifelse(is.null(xNarrow), 0.5, xNarrow), 
							z=NULL, zBr=2, zBrMethod="median", zLevel=NULL, zCenter=NULL, zNarrow=NULL, 
							zCenterFun=median, zSizeRatio=ifelse(is.null(zNarrow), 0.5, zNarrow), 
							freq=FALSE, main=NULL, showLegend=TRUE, zShowLegend=TRUE, 
							autoNorm=TRUE, useDataType=sfUseDataType, plot=TRUE, col=NULL) {

	checkCharInput(chart, c("hist", "density"))

	if (!"distrShift"%in%class(obj)) {

		userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=dataType(obj), col=col)
		col <- userCol$col
		useDataType <- userCol$useDataType

		if (!is.null(z)) {
			z <- userColInput(colName=colnames(obj), useDataType=useDataType, dataType=dataType(obj), col=z)$col[1]
		}

		objTab <- tab(obj)

		if (is.null(zCenter)) {

			if (is.null(z)) {
				zBr <- 1
				legendText <- "all"
			} else {
				if (is.null(zLevel)) {
					objTab <- autoNormalize(objTab, z, normTable(obj, verbose=FALSE), autoNorm)
					breaks <- breakData(objTab[[z]], zBr, zBrMethod)
					zBr <- length(breaks)-1
					legendText <- makeIntervals(breaks, sfPar$distrShift$user$signif)
				} else {
					zBr <- length(zLevel)
					zLevel <- processLevel(objTab, z, zLevel)
					legendText <- as.character(zLevel)
				}

				if (is.null(zLevel)) {
					colorCut <- cut(objTab[[z]], breaks=breaks, labels=FALSE)
				} else {
					colorCut <- objTab[[z]]	
				}
			}

		} else {
			zRowsCentre <- distrShiftPlot(obj, col=z, xCenter=zCenter, xNarrow=zNarrow, xCenterFun=zCenterFun, 
							xSizeRatio=zSizeRatio, plot=FALSE)$mats[[1]]
			zBr <- nrow(zRowsCentre)
			legendText <- as.character(zCenter)
		}

		if (autoNorm) {
			objTab <- autoNormalize(objTab, col, normTable(obj, verbose=FALSE), autoNorm)
		}
		
		colDataAll <- objTab[[col]]
		shiftMats <- list()
		zRows <- list()
		for (i in 1:zBr) {

			if (is.null(zCenter)) {
				zRow <- TRUE
				if (!is.null(z)) {
					zRow <- which(colorCut==ifelse(is.null(zLevel), i, zLevel[i]))	
				}
			} else {
				zRow <- zRowsCentre[i, ]	
			}

			colData <- colDataAll[zRow]
			
			size <- round(length(colData)*xSizeRatio)

			if (is.null(xNarrow)) {
				distrSample <- sample(colData, size, replace=TRUE)
			} else { 
				distrDensity <- density(colData)
				distrDensity$x <- distrDensity$x[1]+c(0,cumsum(xNarrow*diff(distrDensity$x)))
				distrSample <- sample(distrDensity$x, prob=distrDensity$y, size=size, replace=TRUE)	
			}

			shiftMat <- matrix(numeric(), 0, size)

			for (newCentre in xCenter) {
				distrSample <- distrSample - do.call(xCenterFun, list(distrSample, na.rm=TRUE)) + newCentre
				shiftMat <- rbind(shiftMat, sapply(distrSample, function( sampVal ) { which.min((colData-sampVal)^2) } ))
			}

			shiftMats[[i]] <- shiftMat
			zRows[[i]] <- zRow
		}

		if (!plot) { 
			
			distrShiftObj <- list()
			distrShiftObj$data <- colDataAll
			distrShiftObj$col <- col
			distrShiftObj$z <- z
			distrShiftObj$center <- xCenter
			distrShiftObj$mats <- shiftMats
			distrShiftObj$zRows <- zRows
			distrShiftObj$legendText <- legendText
			class(distrShiftObj) <- "distrShift"
			return(distrShiftObj) 
		}
	} else {

		shiftMats <- obj$mats
		zRows <- obj$zRows
		legendText <- obj$legendText
		col <- obj$col
		z <- obj$z
		xCenter <- obj$center
		colDataAll <- obj$data
	}

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	j <- 1
	autoBrowse <- 0
	while (j %in% 1:length(shiftMats)) {


		i <- 1
		while (i %in% seq_along(xCenter)) {
			shiftMat <- shiftMats[[j]]
			colData <- colDataAll[zRows[[j]]]
			oldCentreData <- colData
			oldCentre <- do.call(xCenterFun, list(oldCentreData, na.rm=TRUE))

			newCentreData <- oldCentreData[shiftMat[i,]]

			if (is.null(main)) {
				shiftMain <- sprintf("%s center shift from %s to %s", col, signif(oldCentre, sfPar$distrShift$user$signif), 
								signif(xCenter[i], sfPar$distrShift$user$signif))
			} else {
				shiftMain <- main
			}


			if (chart=="hist") {
				colMin <- min(oldCentreData, newCentreData)
				colMax <- max(oldCentreData, newCentreData)
				breaks <- seq(colMin, colMax, length.out=sfPar$distrShift$user$pbr)

				hist1 <- hist(oldCentreData, breaks=breaks, plot=FALSE, include.lowest=TRUE)
				hist2 <- hist(newCentreData, breaks=breaks, plot=FALSE, include.lowest=TRUE)
				if (freq) {
					ylim <- c(0, max(hist1$counts, hist2$counts))
				} else {
					ylim <- c(0, max(hist1$density, hist2$density))
				}

				hist(oldCentreData, breaks=breaks, col=sfPar$distrShift$user$colors$distr[1], freq=freq, ylim=ylim, 
						main=shiftMain, xlab=col, border=sfPar$distrShift$user$colors$border[1])
				hist(newCentreData, breaks=breaks, col=sfPar$distrShift$user$colors$distr[2], freq=freq, 
						border=sfPar$distrShift$user$colors$border[2], add=TRUE)

				box(bty=sfPar$all$user$par$bty)

			} 

			if (chart=="density") {

				oldCentreData <- density(oldCentreData)
				newCentreData <- density(newCentreData)
				
				oldCentreData$y[1] <- 0
				oldCentreData$y[length(oldCentreData$y)] <- 0
				newCentreData$y[1] <- 0
				newCentreData$y[length(newCentreData$y)] <- 0

				xlim <- c(min(oldCentreData$x, newCentreData$x), max(oldCentreData$x, newCentreData$x))
				ylim <- c(0, max(oldCentreData$y, newCentreData$y))
				
				plot(NULL, main=shiftMain, xlab=col, ylab="Density", xlim=xlim, ylim=ylim)
				polygon(oldCentreData$x, oldCentreData$y, col=sfPar$distrShift$user$colors$distr[1], 
						border=sfPar$distrShift$user$colors$border[1])
				polygon(newCentreData$x, newCentreData$y, col=sfPar$distrShift$user$colors$distr[2], 
						border=sfPar$distrShift$user$colors$border[2])
			}

		
			if (showLegend) {
				legend(sfPar$distrShift$user$legend$pos, as.character(c(signif(oldCentre, sfPar$distrShift$user$signif), 
							signif(xCenter[i], sfPar$distrShift$user$signif))), col=sfPar$distrShift$user$colors$distr, 
							title="center", lty=sfPar$distrShift$user$legend$lty, 
							lwd=sfPar$distrShift$user$legend$lwd, cex=sfPar$distrShift$user$legend$cex, bty=sfPar$distrShift$user$legend$bty,
							seg.len=sfPar$distrShift$user$legend$segLen, pch=sfPar$distrShift$user$legend$pch, 
							pt.cex=sfPar$distrShift$user$legend$ptCex)
			}
			if (zShowLegend&&!is.null(z)) {
				legend(sfPar$distrShift$user$zLegend$pos, legendText[[j]], title=paste(z,ifelse(is.null(zCenter),"","center")), 
							lty=sfPar$distrShift$user$legend$lty, 
							lwd=sfPar$distrShift$user$legend$lwd, cex=sfPar$distrShift$user$legend$cex, bty=sfPar$distrShift$user$legend$bty,
							seg.len=sfPar$distrShift$user$legend$segLen, pch=sfPar$distrShift$user$legend$pch, 
							pt.cex=sfPar$distrShift$user$legend$ptCex)
			}


			if (length(xCenter)>1||length(shiftMats)>1) { 
				idx <- plotBrowsing(i, j, length(xCenter), autoBrowse=autoBrowse)
				i <- idx[[1]]
				j <- idx[[2]]
				autoBrowse <- idx$auto
				if (is.na(i)) { break }
			}
			i <- i + 1
		}
		if (is.na(i)) { break }
		j <- j + 1
	}

	return(invisible())
}

#' ROC Plot
#'
#' @description Plots ROC curve for binary classification models stored in \code{semiTable} or \code{semiFrame} objects. 
#'
#' @template stsfobjParam
#' @param chart Character. Specifies if "train" (default) or "test" data are used for ROC curve.
#' @template mainParam
#' @template dataParam
#' @param modelThr Numeric. Specified threshold will be labeled (default: 0.5).
#' @param colorize Logical. Determines whether the ROC curve should be colorized according to
#' threshold (default: \code{TRUE}).
#' @param text.adj Numeric. Specifies adjustment of the labels (default: c(-0.2, 1.7), ...)).
#' @param ... Character. Additional parameters passed to \code{ROCR::plot} function.
#'
#' @return Plot.
#' 
#' @examples
#'
#' data(pcy)
#'
#' # Random forest data
#' pcy <- model(pcy, type="class", model="forest", splitRatio=0.8, modelFormula="ProbeYield~.")
#' 
#' # Train data used by default
#' rocPlot(pcy)
#' 
#' # Test data used, model threshold 0.7 labeled 
#' rocPlot(pcy, chart="test", modelThr=0.7)
#'
#' @export
rocPlot <- function(obj, chart="train", main=NULL, data="last", modelThr=0.5, colorize=TRUE, text.adj=c(-0.2, 1.7), ...) {

	checkCharInput(chart, c("train", "test"))		

	if (data=="last") { data <- obj$aid$lastData }
	if (!data%in%names(obj$aid$model)) { stop(sprintf("No model on %s data", data)) }

	if (obj$aid$model[[data]]$type!="class") { stop("Not a cllassification") }

	if (is.null(main)) { 
		main <- paste(paste(unique(obj$tab[[sfName$tech]]), collapse=", "), 
			paste(unique(obj$tab[[sfName$maskset]]), collapse=", "), sprintf("ROC %s", chart),sep=", ") 
	}

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	ROCR::plot(obj$aid$model[[data]]$stat[[chart]]$roc, main=main, colorize=colorize, print.cutoffs.at=modelThr, text.adj=text.adj,...)
	return(invisible())
}


#' Correlations Plot
#'
#' @description Plots correlation matrix or dendogram.
#'
#' @template stsfobjParam
#' @param ... Data columns to be used. If none are specified all columns from the data type
#' given by \code{useDataType} will be used. Each column can be entered in following forms:
#' \itemize{
#'   \item{ integers } { Specify column indexes in the column group given by
#'     \code{useDataType} parameter. }
#'   \item{ characters }{ Specify column names. Both character vectors or strings without quotes
#'     can be used. } 
#' } 
#' @param chart Character. Visualization type:
#' \itemize{
#'   \item{ "mat" }{ for correlation matrix (default). }
#'   \item{ "dend" }{ for correlation dendrogram. }
#' }
#' @param addYield Character. Specifies yield column to add (default: "ProbeYield").
#' @param matLimit Numeric. Relevant for \code{chart="mat"}. Only correlations with absolute values above this threshold are shown (default: 0).
#' @param matMethod Character. Relevant for \code{chart="mat"}. Supported visualization methods: "circle", "square", "ellipse", "number",
#' "pie", "shade" and "color" (default).
#' @param matType Character. Relevant for \code{chart="mat"}. Types are "full", "upper" or "lower" (default), for full matrix, 
#' upper triangular or lower triangular matrix, respectively.
#' @param matDiag Logical. Relevant for \code{chart="mat"}. TRUE to add diagonal (default: FALSE). 
#' @param matOrder Character. Relevant for \code{chart="mat"}. Ordering method of the correlation matrix.
#' \itemize{
#'   \item{ "hclust" }{ hierarchical clustering order (default). }
#'   \item{ "original" }{ original order. }
#'   \item{ "AOE" }{ the angular order of the eigenvectors. }
#'   \item{ "FPC" }{ first principal component order. }
#'   \item{ "alphabet" }{ alphabetical order. }
#' }
#' @param hclustLinkage Character. Relevant for \code{chart="dend"}. Agglomeration method for dendrogram:
#' "ward.D", "ward.D2", "single", "complete" (default), "average", "mcquitty", "median" or "centroid". 
#' @param dendThr Numeric. Relevant for \code{chart="dend"}. Threshold for red boxes (default: 0.9). 
#' @param dendMeasure Character. Relevant for \code{chart="dend"}. Choose one of "pos", "neg" and "abs". Then zero
#' distance in dendogram represents correlation +1,-1 and -1 or +1, respectively (default: "abs"). 
#' @template mainParam
#' @template useDataTypeParam
#' @template colParam
#'
#' @template corPlotExamples
#' 
#' @return Plot.
#'
#' @export
corPlot <- function(obj, ..., chart="mat", addYield="ProbeYield", matLimit=0, matMethod="color", matType="lower", matDiag=FALSE, 
						matOrder="hclust", hclustLinkage="complete", dendThr=0.9, dendMeasure="abs", main=NULL, 
						useDataType=sfUseDataType, col=NULL) {
 	
	checkCharInput(chart, c("mat", "dend"))

	objDataType <- dataType(obj)
	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=objDataType, col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType

	if (!length(col)) { col <- objDataType[[useDataType]] }

	if (!is.null(addYield)) { col <- c(col, addYield) }

	corMat  <- cor(na.omit(obj$tab[,col, with=FALSE]))

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	if (chart=="dend") {

		dcor <- 1-switch(dendMeasure, pos=corMat, neg=-corMat, abs(corMat))
		dcor <- as.dist(dcor)
		hc <- hclust(dcor, method=hclustLinkage)

		if (is.null(main)) { main <- "Correlation dendrogram" }

		savePar <- par(mar=sfPar$cor$user$dend$mar)
		on.exit(par(savePar))

		plot(hc, main=main, sub=NA, xlab="", 
			ylab=switch(dendMeasure, pos="1 - correlation", neg="1 + correlation", "1 - |correlation|"), 
			cex=sfPar$cor$user$dend$cex)

		if (!is.null(dendThr)) {
			ct <- cutree(hc,h=(1-dendThr))
			cto <- ct[hc$order]
			ctt <- table(ct)
			xc <- integer()
			for (i in seq_along(ct)) {
			  if (ctt[cto[i]]>1) { xc <- c(xc,i) }
			}

			if (length(xc)) {
				rect.hclust(hc,h=(1-dendThr),x=xc,border=sfPar$cor$user$dend$thrColor)
			}
		}

		return(invisible())
	}

	logMat <- abs(corMat) >= matLimit 
	corMatSub <- corMat * logMat

	if (is.null(main)) { main <- "Correlation matrix" }

	corrplot::corrplot(corMatSub, method=matMethod, type=matType, diag=matDiag, addgrid.col=sfPar$cor$user$mat$gridColor,
	   title=main, order=matOrder, hclust.method=hclustLinkage, tl.cex=sfPar$cor$user$mat$tl$cex, 
	   tl.col=sfPar$cor$user$mat$tl$color, mar=sfPar$cor$user$mat$mar)
    
	return(invisible())
}

#' Maps Overlays Plotting
#'
#' @description Overlays maps represented by \code{semiFrame} object rows. 
#'
#' @param ... \code{semiFrame} objects. If one object is specified, one composite map is constructed
#' from all rows. When multiple objects are provided, shading is added to display boundary overlaps. 
#' @param mapAlpha Numeric. Transparency for overlay (default: 0.5).
#' @template mapBorderParam
#' @param mapBorderNum Numeric. Determines to which map object \code{mapBorder} correction will be applied (default: 1).
#' @template mainParam
#' @template numMainRowParam
#'
#' @template overlayMapPlotExamples
#' 
#' @return Plot.
#'
#' @export
overlayMapPlot <- function(..., mapAlpha=0.5, mapBorder=NULL, mapBorderNum=1, main=NULL, numMain=TRUE) {

	argsList <- list(...)

	color <- c(sfPar$map$user$colors$bin0, sfPar$overlayMap$user$colors)
	on.exit(mapPlotPar(default))

	argsList[[mapBorderNum]] <- cropMap(argsList[[mapBorderNum]], mapBorder=mapBorder)

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))
	saveBin0Color <- sfPar$map$user$colors$bin0
	on.exit(eval(parse(text=paste0("sfPar$map$user$colors$bin0<-", "\"", as.character(saveBin0Color), "\"")), envir=sfPar))

	add <- FALSE
	for (i in 1:length(argsList)) {
		mapObj <- argsList[[i]]

		eval(parse(text=paste0("sfPar$map$user$colors$bin0<-", "\"", as.character(color[i]), "\"")), envir=sfPar)

		for (j in 1:nrow(mapObj)) {
			if (i!=1||j!=1) { main <- "" }
			mapPlot(mapObj[j], mapAlpha=geti(mapAlpha, i), add=add, main=main, numMain=numMain, showLegend=FALSE)
			add <- TRUE
		}

	}
	return(invisible())
}

#' Inherited Defects Percentages Plot
#'
#' @description Plots inherited defects percentages per specified intervals.
#' 
#' @param obj \code{semiTable} object. Applicable to defects dataset.
#' @template stepParam
#' @param col Character. Specifies column to break into intervals (default: "DefArea").
#' @param breaks Numeric. Cut points of intervals of the variable specified in \code{col} (default: \code{c(0:10, +Inf)}).
#' @template coreParam
#' @template mainParam
#' @template showLegendParam
#'
#' @return Plot.
#' 
#' @template defInheritedPlotExamples
#' 
#' @export
defInheritedPlot <- function(obj, steps, col="DefArea", breaks=c(0:10, +Inf), core=(length(breaks)-1),
								main=NULL, showLegend=TRUE) {

	if (length(steps) < 2) { stop("Length of 'steps' should be > 1.") }
	if (length(breaks) < 3) { stop("Length of 'breaks' should be > 2.") }

	coreIdx <- getCoreIdx(seq_along(breaks)[-length(breaks)], core)

	inheritedLoop <- function(idx, breaks) {

		imat <- NULL

		for (i in idx) {

			imatRow <- t(defInherited(obj[eval(as.name(col))>breaks[i]&eval(as.name(col))<=breaks[i+1]], steps, verbose=FALSE))
			imatRow <- imatRow[nrow(imatRow),,drop=FALSE]
			rownames(imatRow) <- NULL
			imatRow <- cbind(imatRow, i)
			colnames(imatRow)[ncol(imatRow)] <- "idx"

			if (is.null(imat)) {
				imat <- imatRow
			} else {
				imat <- rbind(imat, imatRow)
			}
		}

		return(imat)
	}

	if (length(coreIdx)>1) {

		on.exit(registerDoSEQ())

		imat <- foreach (core=seq_along(coreIdx), .combine=rbind) %dopar% {
			inheritedLoop(coreIdx[[core]],breaks)
		}

	} else {
		imat <- inheritedLoop(coreIdx[[1]], breaks)
	}

	imat <- imat[order(imat[,"idx"]),][,-ncol(imat), drop=FALSE]	

	colors <- do.call(sfPar$defInherited$user$level, list(ncol(imat)))
	 if (is.null(main)) {
	 	main <- sprintf("%s, Percentages of Inherited Defects", obj[[sfName$maskset]][1])
	 }

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))


	plot(NULL, main=main, xlab=col, ylab="% Inherited", xlim=c(1, nrow(imat)), 
		ylim=c(min(imat,na.rm=TRUE), max(imat,na.rm=TRUE)), xaxt="n")

	for (j in 1:ncol(imat)) {
		lines(1:nrow(imat), imat[,j], col=colors[j], lwd=sfPar$defInherited$user$lwd)
	}

	ilabels <- sapply(seq_along(breaks)[-length(breaks)], function(i) sprintf("(%s, %s%s", as.character(breaks[i]), 
		as.character(breaks[i+1]), ifelse(breaks[i+1]==Inf, ")", "]")) )
	axis(1, at=1:nrow(imat), labels=ilabels)	

	if (showLegend) {
		legendText <- list(title="Step(s):", legend=colnames(imat))
		legend(sfPar$defInherited$user$legend$pos, legend=legendText$legend, 
			col=colors, title=legendText$title, lty=1, lwd=7, cex=0.9, bty="n")
	}	


	return(invisible())
}

#' Defects Die Heatmap
#' 
#' @description Plots a defects die heatmap
#' 
#' @param obj \code{semiFrame} object representing the defects dataset.
#' @param bins Integer. Specifies which bins to use. For all available bins use keyword "all"
#'        (default: "all").
#' @param balance Numeric. Balances defects in failed dice with defects in good dice. Sampled number of defects in good dice is defined as total number of defects in bad dice multiplied by \code{balance} (default: \code{NULL}).
#' @template mainParam
#' @param res Integer. Defines resolution of the heatmap (default: \code{c(300, 300)}).
#' @param mode Character. Defines mode: "smooth" (default) for neighborhood smoothing or "raw" for raw point display.
#' @param smoothType Character. Indicates the type of smoothing filter to be used: "circle",  "Gauss" (default) or "rectangle".
#' @param smoothParam Numeric. If \code{type=circle}, the radius of the circle (in units
#'           of the CRS). If \code{type=rectangle} the dimension of the
#'           rectangle (one or two numbers). If \code{type=Gauss} the size of
#'           \code{sigma}, and optionally another number to determine the size of
#'           the matrix used (default: \code{c(1,2)}).
#' @param smoothFun Function. Applied to calculate focal values for the smoothing neighborhood of
#'           focal cells using the matrix of weights defined by \code{smoothType} and \code{smoothParam} (default: sum).
#' 
#' @return Plot.
#' 
#' @examples 
#' 
#' # Defects dataset
#' data(def)
#'      
#' # Defect maps dataset
#' data(dem)
#'   
#' # Defects per die dataset
#' ded <- defDie(wbm, dem, def=def)
#' 
#' # Create a dataset which pairs the information on particul bin and corresponding defects.
#' defb <- transfer(ded, def, c("bin", "fail"), key="lotWaferStepDie")
#' 
#' # Create defects per die heatmap
#' defDiePlot(defb[step=="STEP1"], res=c(400,400), smoothParam=c(0.5,1), main="STEP1, All bins")
#' 
#' @export
defDiePlot <- function(obj, bins="all", balance=NULL, main=NULL, res=c(300,300), mode="smooth", smoothType="Gauss", smoothParam=c(1,2), smoothFun=sum) {

	if (!is.null(balance)) {

		zeroBinRow <- which(obj[[sfName$bin]]==0)
		binRow <- which(obj[[sfName$bin]]>0)
		binLen <- round(balance*length(binRow))
		if (binLen < length(zeroBinRow)) {
			zeroBinRow <- sample(zeroBinRow, binLen)
		}
		nrowobj <- nrow(obj)
		obj <- obj[c(zeroBinRow, binRow)]
		ncatn(sprintf("Die-fail balanced from %d defects to %d defects", nrowobj, nrow(obj)))
	}

	if (is.null(main)) {
		main <- sprintf("%s, Bin(s): %s", paste(unique(unique(obj[[sfName$step]])),collapse=","),paste(bins, collapse=", "))
	}

	if (all(bins!="all")) {
		bins <- c(0, bins)
		obj <- obj[eval(as.name(sfName$bin))%in%bins]
	}
	bins <- as.integer(obj[[sfName$bin]]>0)

	cutTab <- data.table(b=bins, r=cut(obj[[sfName$yrel]], res[1], labels=FALSE, include.lowest=TRUE),
				c=cut(obj[[sfName$xrel]], res[2], labels=FALSE, include.lowest=TRUE))
	cutTab[,c("rc"):=list(paste(r,c,sep="."))]
	cutTab <- cutTab[order(rc)]
	keyTab <- cutTab
	idx <- which(duplicated(keyTab[["rc"]]))
	if (length(idx)) {
		keyTab <- keyTab[-idx]
	}
	mat <- matrix(0, res[1], res[2])
	for (i in 1:nrow(keyTab)) {
		# bf <- cutTab[rc==keyTab[["rc"]][i], b]
		bf <- cutTab[["b"]][which(cutTab[["rc"]]==keyTab[["rc"]][i])]
		mat[keyTab[["r"]][i], keyTab[["c"]][i]] <- (sum(bf==0)-sum(bf))/length(bf)
	}

	if (mode=="smooth") {
		ras <- raster::raster(nrow=res[1], ncol=res[2], xmn=0, xmx=res[2], ymn=0, ymx=res[1])
		raster::values(ras) <- as.vector(mat)
		focalW <- raster::focalWeight(ras, smoothParam, type=smoothType)
		ras <- raster::focal(ras, focalW, fun=smoothFun, pad=TRUE, padValue=0)
		mat <- raster::values(ras)
		dim(mat) <- res
	}

	mat <- t(mat)
	mat <- mat[nrow(mat):1,]
	mat <- mat[,ncol(mat):1]
	color <- RColorBrewer::brewer.pal(11,"RdYlGn")
	# color <- RColorBrewer::brewer.pal(11,"RdGy")
	color[6] <- "white"
	breaks <- -1 + 0:11*2/11
	breaks[1] <- -1.5
	breaks[length(breaks)] <- 1.5

	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	image(mat, breaks=breaks, col=color, axes=FALSE, main=main)
	
	return(invisible())
}

#' Naïve Bayes Model Plot
#' 
#' @description Plots conditional probabilities from naïve Bayes model of die failure given defects per die count.
#' 
#' @param obj `semiTable` object representing defects per die dataset.
#' @template stepParam
#' @param bins Character, Numeric. Specifies response bin or bins. Use keyword "all" for all the bad dice (default: "all").
#' @template defCountParam
#' @param chart Character. Specifies chart type: "prob" (default) for viewing conditional probability of a bad die 
#'              or "yield" for depiction of yield loss percentage.
#' @template mainParam
#' @template showLegendParam
#' @param diePerc Logical. If \code{TRUE} prevalence percentages of corresponding defects per die cases are shown along the lines (default: \code{TRUE}).
#' @param diePercDefCount Integer. Specifies which prevalence percentages are shown along the lines (default: \code{defCount[-1]}).
#' @param dieDenom Integer. User defined dice count denominator (default: \code{NULL}).
#' @param priorIndices List. User defined indices for prior calculation (default: list(\code{TRUE})).
#' @param varCol Character. Column carrying input variable. Can be used to specify defect type (default: "ALL").
#' @param plot Logical. If \code{FALSE}, table of bad die probability for given defect count, prior probability and
#'                   percentage of yield loss is returned instead of the plot (default: \code{FALSE}).
#' @template coreParam
#' @param legendText Character. User defined legend (default: \code{NULL}).
#' @param priorCoef Numeric. User defined multiplicative coefficients for prior probabilities (default: 1).
#' @param probData Probability data from previous run of \code{defBayesPlot} with \code{plot=FALSE} (default: \code{NULL}).
#' @param laplace Numeric. Laplace regularization term for the naïve Bayes model (default: 0).
#' @template verbose
#'
#' @return Plot or probability data.
#' 
#' @template defBayesPlotExamples
#' 
#' @export
defBayesPlot <- function(obj, steps, bins="all", defCount=0:3, chart="prob", main=NULL, showLegend=TRUE, 
				diePerc=TRUE, diePercDefCount=defCount[-1], dieDenom=NULL, priorIndices=list(TRUE), 
				varCol="ALL", plot=TRUE, core=length(steps), legendText=NULL, priorCoef=1, 
				probData=NULL, laplace=0, verbose=TRUE) {
	

	defBayesLoop <- function(klaStep, priorIdx=TRUE) {
		
		ncatn("Step:", klaStep,verbose=verbose)
		if (klaStep=="all") {
			klaChipStep <- copy(obj[priorIdx])
		} else {
			klaChipStep <- obj[priorIdx&eval(.sfName$step)==klaStep]
		}
		delCol <- setdiff(dataType(klaChipStep)[[sfDataType$def]], varCol)
		if (length(delCol)) {
			klaChipStep <- variable(klaChipStep, delCol, NULL)
		}
		dicePerDefCount <- klaChipStep[[varCol]]
		dicePerDefCount[which(dicePerDefCount>=max(defCount))] <- max(defCount)
		klaChipStep$tab[,c(varCol):=list(dicePerDefCount)]
		dicePerDefCount <- table(dicePerDefCount)
		nprintn(dicePerDefCount, verbose=verbose)
		if (!any(bins=="all")) {
			klaChipStep$tab[,c(sfName$fail):=list(as.integer(eval(.sfName$bin)%in%bins))]
		}

		klaChipStep <- model(klaChipStep, type="class", model="bayes", splitRatio=1, formula=paste0(sfName$fail,"~."), 
							dataType=sfDataType$def, laplace=laplace, verbose=FALSE, genStat=FALSE)

		newdata <- data.frame(factor(defCount))
		names(newdata) <- varCol
		conProbStep <- round(100*e1071:::predict.naiveBayes(modelM(klaChipStep), newdata=newdata, type="raw")[,sfName$fail],1)
		if (is.null(dieDenom)) {
			chipsDenom <- sum(dicePerDefCount)
		} else {
			chipsDenom <- dieDenom
		}
		percPerDefCount <- signif(100*dicePerDefCount/chipsDenom, sfPar$defBayes$user$text$signif)
		
		return(list(conProbStep=conProbStep, percPerDefCount=percPerDefCount))
	}

	if (is.null(probData)) {
		if (core>length(steps)) { core <- length(steps) }
		
		if (core>1) {

			registerDoMC(core)
			on.exit(registerDoSEQ())

			defBayesLoopCombine <- function(obj1, obj2) {
				for (i in seq_along(obj1)) {
					obj1[[i]] <- rbind(obj1[[i]], obj2[[i]])
				}
				return(obj1)
			}

			bdf <- foreach (klaStep=steps, .combine=defBayesLoopCombine) %dopar% {
				defBayesLoop(klaStep)
			}

			conProb <- bdf$conProb
			percPerDefCounts <- bdf$percPerDefCount

		} else {
			
			conProb <- NULL
			percPerDefCounts <- NULL
				
			for (klaStep in steps) {

				for (i in seq_along(priorIndices)) {

					bdf <- defBayesLoop(klaStep, priorIndices[[i]])

					if (is.null(conProb)) {
						conProb <- matrix(bdf$conProbStep, 1, length(bdf$conProbStep))
						percPerDefCounts <- matrix(bdf$percPerDefCount, 1, length(bdf$percPerDefCount))
					} else {
						conProb <- rbind(conProb, bdf$conProbStep)
						percPerDefCounts <- rbind(percPerDefCounts, bdf$percPerDefCount)
					} 
				}

			}
		}

		colnames(conProb) <- as.character(defCount)
		if (nrow(conProb)==length(steps)) {
			rownames(conProb) <- steps
		}
		colnames(percPerDefCounts) <- as.character(defCount)
		if (nrow(percPerDefCounts)==length(steps)) {
			rownames(percPerDefCounts) <- steps
		}
		
	} else {
		conProb <- probData$prob
		percPerDefCounts <- probData$prior
	}
	yieldMat <- round((conProb-conProb[,1])*percPerDefCounts*priorCoef/100,2)
	bzIdx <- which(yieldMat<0, arr.ind=TRUE)
	if (nrow(bzIdx)) {
		yieldMat[bzIdx] <- 0
	}
	yieldMat <- yieldMat[,-1]

	if (!plot) {
		return(list(prob=conProb, prior=percPerDefCounts, yield=yieldMat))
	}

	xlab <- "Defect Count Per Die"
	ylab <- "Bad Die Probability Given Defect Count"
	if (any(bins=="all")) {
		mainB <- bins
	} else {
		mainB <- paste(paste0("B", bins), collapse=", ")
		ylab <- paste(mainB,ylab) 
	}

	
	savePP <- savePlotPar()
	on.exit(loadPlotPar(savePP))

	color <- funVec(sfPar$defBayes$user$color[[chart]]$level, length(steps)*length(priorIndices))
	if (chart=="yield") {
		
		if (is.null(main)) {
			main <- paste("Yield Loss on Bin(s):", mainB)
		}
		color <- colorAlpha(color, sfPar$defBayes$user$color$yield$alpha)
		if (is.vector(yieldMat)) {
			xlab <- character()
		}

		if (showLegend) {
			legText <- steps
		} else {
			legText <- FALSE
		}

		barplot(yieldMat, beside=TRUE, col=color, main=main, xlab=xlab, ylab="Percent Yield Loss", 
			legend.text=legText, border=FALSE, args.legend=list(border=FALSE, bty="n", 
				x=sfPar$defBayes$user$legend$pos), las=sfPar$defBayes$user$las)

	} else if (chart=="yieldProb") {

		if (is.null(main)) {
			main <- paste("Yield Loss on Bin(s):", mainB)
		}

		if (is.vector(yieldMat)) {
			xlab <- character()
		}

		if (showLegend) {
			legText <- steps
		} else {
			legText <- FALSE
		}

		par(mar=c(5.1, 4.1, 5.1, 4.1))
		# barplot(yieldMat, beside=TRUE, col=color, main=main, xlab=xlab, ylab="Percent Yield Loss", 
		# 	legend.text=legText, border=FALSE, args.legend=list(border=FALSE, bty="n", 
		# 		x=sfPar$defBayes$user$legend$pos), las=sfPar$defBayes$user$las)

		percPerDefCounts <- percPerDefCounts[,"1"]
		plot(x=1:length(percPerDefCounts), y=percPerDefCounts, type="l", xaxt="n", yaxt="n", 
				xlab=expression(paste("Defect Area Threshold (", mu,"m"^{2}, ")")), ylab=NA, 
				col=color[1], lwd=sfPar$defBayes$user$lwd)

		idx <- c(1, length(percPerDefCounts)%/%3, 2*length(percPerDefCounts)%/%3, length(percPerDefCounts))
		idx <- idx[!duplicated(idx)]
		shadowText(x=idx, y=percPerDefCounts[idx], labels=paste0(percPerDefCounts[idx], "%"),
					col=color[1], bg="white", cex=1.2)

		par(new=TRUE)
		plot(yieldMat, col=color[2], main=main, type="l", xaxt="n", xlab=xlab, 
			ylab=NA,  ylim=c(0,max(yieldMat)), lwd=sfPar$defBayes$user$lwd)
		axis(1, labels=names(yieldMat), at=1:length(yieldMat))
		mtext("Percent Yield Loss Explained (%)", side=2, line=3, col=color[2])

		conProb <- conProb[,"1"]
		par(new=TRUE)
		plot(x=1:length(conProb), y=conProb, type="l", ylim=c(0,max(conProb)), xaxt="n", yaxt="n", xlab=NA, ylab=NA, 
				col=color[3], lwd=sfPar$defBayes$user$lwd)
		axis(4)
		mtext("Bad Die Probability Given 1 or More Defects (%)", side=4, line=3, col=color[3])


	} else {

		if (is.null(main)) {
			main <- paste("Conditional Probabilities on Bin(s):", mainB)
		}

		plot(NULL, xlim=c(min(defCount), max(defCount)), ylim=c(min(conProb), max(conProb)), 
			ylab=ylab, main=main, xaxt='n', xlab=xlab)
		axis(1, at=defCount, labels=defCount)	

		for (i in 1:nrow(conProb)) {
			lines(defCount, conProb[i,], col=color[i], lwd=sfPar$defBayes$user$lwd)
			if (diePerc) {
				idx <- which(defCount%in%diePercDefCount)
				shadowText(defCount[idx], conProb[i,idx], paste0(percPerDefCounts[i,idx],"%"), 
					pos=sfPar$defBayes$user$text$pos, col=color[i], 
					bg=sfPar$defBayes$user$text$bg)
			}
		}

		if (showLegend) {
			if (is.null(legendText)) {
				legendText <- list(title="Step(s):", legend=steps)
			}
			legend(sfPar$defBayes$user$legend$pos, legend=legendText$legend, 
				col=color, title=legendText$title, lty=1, lwd=7, cex=0.9, bty="n")
		}	
	}

	return(invisible())
}

 
#'Bayes Defectivity Optimization Plot
#' 
#' @description Bayes defectivity optimization using conditional probabilities and yield loss explained.
#' 
#' @param obj List of \code{semiTable} objects representing defects per die datasets each constituting one optimization step.
#' @param step Character. Defines single manufacturing step.
#' @template defCountParam
#' @param chart Character. Specifies chart type: "prob" (default) for conditional probability optimization 
#'           or "yield" for yield loss explained optimization.
#' @template mainParam
#' @param plot Logical. If \code{FALSE} optimization object is returned. If in addition \code{returnProb=TRUE}, probability data is returned (default: \code{TRUE}).
#' @param returnProb Logical. If \code{TRUE} in combination with \code{plot=FALSE}, probability data is returned (default: \code{FALSE}).
#' @template coreParam
#' @param optimObj \code{semiTable} optimization object (default: NULL).
#' @template showLegendParam
#' @param forceGC Logical. If \code{TRUE} garbage collection will be forced during computations (default: \code{TRUE}).
#' @template verbose
#' @param ... Parameters passed to \code{defBayesPlot}.
#'
#' @return Plot or probability data.
#' 
#' @template defBayesOptimPlotExamples
#' 
#' @export
defBayesOptimPlot <- function(obj, step, defCount=0:3, chart="prob", main=NULL, plot=TRUE, returnProb=FALSE, core="auto", optimObj=NULL, 
								showLegend=TRUE, forceGC=TRUE, verbose=TRUE, ...) {

	if (!is.list(obj)) { stop("list containing semiTable objects required.") }

	useStep <- step
	if (is.null(optimObj)) {

		for (itemName in names(obj)) {

			optimItemObj <- obj[[itemName]][step==useStep]			
			itemNameProcessed <- paste0(">", format(as.numeric(sub("[^0-9.]*", "", itemName)), nsmall=2))
			optimItemObj$tab[,"step":=itemNameProcessed]

			if (is.null(optimObj)) {

				optimObj <- optimItemObj
			} else {

				optimObj <- rbind(optimObj, optimItemObj)
			}
		}
	
		if (forceGC) { gc() }
	}

	if (!plot&&!returnProb) {
		return(optimObj)
	}

	on.exit(defBayesPlotPar(default))

	factors <- unique(optimObj[[sfName$step]])

	if (chart%in%c("yield","yieldProb")) {
		color <- rep(sfPar$defBayesOptim$user$color$yield, length(factors))
		showLegend <- FALSE
		defCount <- 0:1
		eval(parse(text=paste0("sfPar$defBayes$user$color$yield$level<-", deparse(color))), envir=sfPar)	
	} else {
		color <- funVec(sfPar$defBayesOptim$user$color$prob, length(factors))
		eval(parse(text=paste0("sfPar$defBayes$user$color$prob$level<-", deparse(color))), envir=sfPar)	
	}


	if (is.character(core)) {
		if (core=="auto") {
			core <- length(factors)
		} else {
			stop("Invalid 'core' value.")		
		}
	}

	legendText <- list(title="DefArea Thr", legend=factors)

	if (forceGC) { gc() }
	probData <- defBayesPlot(optimObj, steps=factors, defCount=defCount, chart=chart, main=main, core=core, plot=plot, 
		legendText=legendText, showLegend=showLegend, verbose=verbose, ...)

	if (!is.null(probData)) { return(probData) }

	return(invisible())
}


#
# Standard Generics Methods
#

#
# Functions
#

processLevel <- function(objTab, col, level) {

	if (any(level=="all")) {
		level <- sort(unique(objTab[[col]]))
	}
	if (length(level)>500) { stop(sprintf("%s is not a categorical variable", col)) }
	return(level)
}

autoNormalize <- function(objTab, col, normTab, autoNorm) {

	if (is.null(normTab)||!autoNorm) { return(objTab) }

	normType <- names(normTab)[[1]]
	normTab <- normTab[[1]]

	for (colName in col) {
		
		normTabCol <- normTab[parameter==colName,]
		if (!nrow(normTabCol)) { next }
		
		if (normType=="norm") {
			colData <- objTab[[colName]]*normTabCol[, spread]+normTabCol[, center]
			objTab[, c(normCol):=list(colData)]

		} else {
			normTabCol <- 10^normTabCol
			objTab[, c(colName):=list(normTabCol[,magnitude]*objTab[[colName]])]
		}

	}

	return(objTab)
}


timeAxis <- function(xtsData, uniqueDates=FALSE, formatDates="%Y-%m", ticks=5, gridXY=c(TRUE, TRUE), 
						gridColor="grey", gridLty=3, cexAxis=sfPar$time$user$cexAxis) {

	dates <- zoo::index(xtsData)
	if (uniqueDates) { dates <- unique(dates) }
	x <- 1:length(dates)
	ticks <- ticks-1
	xIdx <- 1+floor((length(x)-1)/ticks)*(0:ticks)
	axis(1, at=x[xIdx], labels=strftime(dates, formatDates)[xIdx], cex.axis=cexAxis)	

	if (gridXY[1]) { abline(v=x[xIdx], col=gridColor, lty=gridLty) }
	if (gridXY[2]) { grid(NA, NULL, col=gridColor, lty=gridLty) }
	return(invisible())
}

perColPlotMain <- function(main, numMain, col, useDataType, dataType, addStart="", addEnd="") {

	if (!is.null(main)) { return(main) }

	main <- col
	if (numMain) {
		if (useDataType%in%names(dataType)) {
			colMatch <- match(col, dataType[[useDataType]])
			if (!is.na(colMatch)) {
				main <- sprintf("%d: %s",colMatch,main)
			}
		}
}

	main <- paste0(addStart, main, addEnd)

	return(main)
}

# user input
# wait <- function() {
# 	btnFlag <- 0

# 	btnPrevPress <- function() {
# 		tcltk::tkdestroy(tt)
# 		btnFlag <<- 1
# 	}
# 	btnStopPress <- function() {
# 		tcltk::tkdestroy(tt)
# 		btnFlag <<- 2
# 	}

#     tt <- tcltk::tktoplevel()
#     tcltk::tkpack( tcltk::tkbutton(tt, text='Next', command=function()tcltk::tkdestroy(tt)), side='top')
#     tcltk::tkpack( tcltk::tkbutton(tt, text='Prev', command=btnPrevPress), side='top')
#     tcltk::tkpack( tcltk::tkbutton(tt, text='Stop', command=btnStopPress), side='bottom')
#     tcltk::tkbind(tt,'<Key>', function()tcltk::tkdestroy(tt) )
#     tcltk::tkbind(tt,'<Left>', btnPrevPress )
#     tcltk::tkbind(tt,'<Escape>', btnStopPress )
#     tcltk::tkwait.window(tt)
#     btnFlag
# }

wait <- function(autoBrowse, caller) {

	flag <- 0
	value <- 1
	cluster <- NULL

	if (autoBrowse>0) {
		
		Sys.sleep(autoBrowse)
	
	} else {

		key <- readline("Browsing... ([h] for help):")

		if (key%in%c("h","help")) {
			
			browsingHelp <- sfBrowsingHelp$default

			if (caller=="mapPlot") {
				browsingHelp <- paste0(browsingHelp, sfBrowsingHelp[[caller]])
			}

			ncatn(browsingHelp, k1=1, k2=2)
			
			return(wait(autoBrowse=autoBrowse, caller=caller))
		}

		if (key%in%c("x","exit", "quit")) { 
			flag <- 2 
		} else if (key=="-") { 
			flag <- 1 
		} else if (grepl("^[+]?[0-9]+",key)) {
			value <- as.integer(sub("[+]","",key))
		} else if (grepl("-[0-9]+",key)) {
			flag <- 1 
			value <- as.integer(sub("-","",key))
		} else if (key=="auto") {
			autoBrowse <- 1
		} else if (grepl("^auto[ ]*[0-9]+",key)) {
			autoBrowse <- as.numeric(sub("auto","",key))
		}

		if (caller=="mapPlot") {
			clust <- NULL
			clustAction <- NULL
			if (grepl("^add",key)) {
				clustAction <- "add"
			} else if (grepl("^move",key)) {
				clustAction <- "move"
			} else if (grepl("^del",key)) {
				clustAction <- "del"
			} else if (grepl("^remove",key)) {
				clustAction <- "remove"
			} else if (grepl("^summary",key)) {
				clustAction <- "summary"
			}
			if (!is.null(clustAction)) {
				clust <- sub("^add|^move|^del|^remove|^summary", "", key)
				clust <- sub("^[ ]*", "", clust)
				cluster <- list(action=clustAction, clust=clust)
			}
		}
	}

	return(list(flag=flag, value=value, auto=autoBrowse, cluster=cluster))
}

plotBrowsing <- function(idx, idx2=NULL, idxMax=NULL, autoBrowse=0, caller="default") {

	fig <- par()$fig
	if (!(fig[2]==1 && fig[3]==0) && idx!=idxMax) {
		return(list(idx=idx, idx2=idx2, auto=autoBrowse, cluster=cluster))
	}

	waitData <- wait(autoBrowse=autoBrowse, caller=caller)
	btnFlag <- waitData$flag
	btnValue <- waitData$value
	autoBrowse <- waitData$auto
	cluster <- waitData$cluster
	
	if (is.null(idx2)) {
		
		if (btnFlag==2) {
			idx <- NA
		} else if (btnFlag==1) {
			idx <- idx - btnValue - 1
			if (idx<0) { idx <- 0 }
		} else {
			idx <- idx + btnValue - 1 
		}
		return(list(idx=idx, auto=autoBrowse, cluster=cluster))
	}

	if (btnFlag==2) {
		idx <- NA
	} else if (btnFlag==1) {
		idx <- idx - btnValue - 1
		if (idx<0) { 
			if (idx2>1) { 
				idx <- idxMax-1 
				idx2 <- idx2 - 1
			} else {
				idx <- 0
			}
		}
	} else {
		idx <- idx + btnValue - 1 
	}

	return(list(idx=idx, idx2=idx2, auto=autoBrowse, cluster=cluster))
}

makeIntervals <- function(treshold, sigDig) {

	if (is.numeric(treshold)) {
		treshold <- as.character(signif(treshold, sigDig))
	} else if ("POSIXct"%in%class(treshold)) {
		treshold <- format(treshold, "%y/%m")
	} else {
		treshold <- as.character(treshold)
	}

	return(sapply(1:(length(treshold)-1), function(i) { sprintf("%s%s, %s]",ifelse(i==1, "[", "("),treshold[i], treshold[i+1]) } ))
}

savePlotPar <- function() {
	
	pp <- list(par=par()[sfPar$all$user$parSave], pal=palette())
	par(sfPar$all$user$par)
	newPal <- pp$pal
	newPal[1] <- sfPar$all$user$black
	palette(newPal)
	return(pp)
}

loadPlotPar <- function(pp) {

	par(pp$par)
	palette(pp$pal)
	return(invisible())
}


checkData <- function(aid, data) {
	
	checkCharInput(data, c("par", "map", "last"), "classes")	
	if (data=="last") { data <- aid$lastData }
	if (!data%in%names(aid$model)) { stop(sprintf("No model on %s data", data)) }
	return(data)
}

inputThroughRange <- function(input, range) {

	if (is.null(input)) { stop("Specify input.") }

	if (length(input)==1) {
		if (input<2) { input <- 2 }
			input <- seq(range[1], range[2], length.out=input)
	}
	
	return(input)
}