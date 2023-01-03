#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# Class semiFrame
# 
# 0.109.1
# 

#
# Imports
#

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import doMC
NULL

#' @import foreach
NULL

#
# Constants
#

# semiFrame names
sfName <- list(matRows="sf.matRows", matCols="sf.matCols", clust="C", clustLot="Cluster",  
	tech="technology", maskset="maskset", lot="lot", wafer="wafer", siteid="siteid", lotWafer="lotWafer", 
	lotWaferSiteid="lotWaferSiteid", part="part", partClean="partClean", partBase="partBase", partBaseLast="partBaseLast", pcStartDate="pcStartDate", 
	probeStartDate="probeStartDate", probeEndDate="probeEndDate", partial="partial", beSite="beSite", rowCount="rowCount", 
	colCount="colCount", pdpw="pdpw", badDice="badDice", goodDice="goodDice", probeSite="probeSite", flat="flatPosition", 
	diex="diex", diey="diey", diexy="diexy", slot="slot", step="step", defDate="defDate", area="area", defClass="defClass", defNum="defNum", 
	defCnt="DefCount", defDens="DefDens", fail="fail", bin="bin", die="die", lehStartDate="lehStartDate", startDate="StartDate", 
	laserscribe="laserscribe", klaStartDate="klaStartDate", lotWaferStep="lotWaferStep", lotWaferStepClass="lotWaferStepClass",
	toolGroup="toolGroup", tool="tool", toolDesc="toolDesc", toolType="toolType", chamberFG="chamberFG", chamber="chamber", 
	chamberDesc="chamberDesc", layer="layer", 	sequence="sequence", recipe="recipe", lotWaferDie="lotWaferDie", 
	sampleProbe="sampleProbe", diceSkipSP="diceSkipSP", pdpwSP="pdpwSP", testBatchid="testBatchid", time="time", probeType="probeType",
	figure="figure", lotSlotRecipeStepFigure="lotSlotRecipeStepFigure", recipeStepFigure="recipeStepFigure", fgStartDate="fgStartDate", 
	ancestor="Ancestor", recipeAncestor="recipeAncestor", count9WFRSClean="count9WFRSClean", countTRNClean="countTRNClean",
	operator="operator", lotSequenceFigure="lotSequenceFigure", sequenceFigure="sequenceFigure",
	defid="defid", xidx="xidx", yidx="yidx", classn="classn", xrel="xrel", yrel="yrel", xyrel="xyrel", 
	lotWaferDieXYRel="lotWaferDieXYRel", lotWaferStepDieXYRel="lotWaferStepDieXYRel",
	dieXYRel="dieXYRel", lotWaferStepDie="lotWaferStepDie", lotWaferDieIdx="lotWaferDieIdx",
	program="program", testerType="testerType", partVerAvailable="partVerAvailable", partVerType="partVerType",
	inspector="inspector", klaOrient="klaOrient", klaTestPlanRefXY="klaTestPlanRefXY",
	defect="defect", dieRow="dieRow", dieCol="dieCol", image="image")

# semiFrame names as.name
.sfName <- lapply(sfName, as.name)

# map type
# B   - Bin
# BB  - Binary Bin
# H   - Heatmap
# RB  - Representation Bin
# RBB - Representation Binary Bin
# RH  - Representation Heatmap
# RDEF  - Representation Heatmap
# WS  - Wafer Sort
# DR  - Dim Reduction
sfMapType <- list(bin="B", bbin="BB", heat="H", site="SITE", bsite="BSITE", rbin="RB", rbbin="RBB", rheat="RH", rsite="RSITE", rbsite="RBSITE", rdef="RDEF", wsort="WS", dr="DR")
sfMapTypes <- do.call(c, sfMapType)
sfMapTypeGroup <- with(sfMapType, list(bin=c(bin, rbin), bbin=c(bbin, rbbin), heat=c(heat, rheat), site=c(site, rsite), bsite=c(bsite, rbsite)))

# data type
# META- Meta data
# PC  - PCM
# PCJ - PCM Junk
# MET - Metrology
# LEH - Lot equipment history
# DEF - Defectivity
# Y   - Yield
# C   - Cluster
# FG  - FabGuard
# DR  - Dim Reduction
# U   - user
sfDataType <- list(meta="META", pc="PC", pcj="PCJ", met="MET", leh="LEH", def="DEF", y="Y", clust="C", fg="FG", dr="DR", u="U")
sfDataTypes <- do.call(c, sfDataType)
sfUseDataType <- setdiff(sfDataTypes, sfDataType$meta)

# 
# Util
# 

getCoreIdx <- function(idx, cores, registerCores=TRUE) {


	if (!length(idx)) { return(NULL) }
	if (cores<1) { cores <- 1 }
	if (cores==1) { return(list(idx)) }

	coreIdx <- list()
	idx <- as.integer(idx)
	cores <- as.integer(cores)
	if(length(idx) < cores) {
		
		cores <- length(idx)
		coreIdx <- lapply(1:cores, function(core) { return(idx[core]) } )
	} else {

		idxPerCore <- length(idx)%/%cores
		idxRest <- length(idx) - cores*idxPerCore
		idxRest <- cumsum(c(rep(1, idxRest), rep(0, cores-idxRest)))

		coreIdx <- lapply(1:cores, function(core) { 
			return(idx[(1+(core-1)*idxPerCore + ifelse(core==1, 0, idxRest[core-1])):(core*idxPerCore + idxRest[core])]) 
		})
	}

	if (registerCores) {
		registerDoMC(cores)
	}

	return(coreIdx)
}


#
# Classes Constructors
#

# Wraps semiTable components
semiTableWrap <- function(tab=data.table(), dataType=list(), aid=NULL, objClass=NULL) {
	
	obj <- list()
	obj$tab <- tab
	if (is.null(aid)) {
		obj$aid <- list(dataType=dataType)
	} else {
		obj$aid <- aid
	}

	if (!is.null(objClass)) {
		class(obj) <- objClass
	} else {
		class(obj) <- c("semiTable", "tableList")
	}

	return(obj)
}

# Wraps semiFrame components
semiFrameWrap <- function(tab=data.table(), mat=list(), matDim=data.table(), mapType=character(), 
							dataType=list(), setKey=FALSE, objClass=NULL) {
	
	obj <- list()
	obj$tab <- tab
	obj$mat <- mat
	obj$matDim <- matDim
	obj$aid <- list(mapType=mapType, dataType=dataType)

	if (setKey) {
		setkeyv(obj$tab, c(tmName$matN, tmName$matRow))
		setkeyv(obj$matDim, tmName$matN)
	}

	if (!is.null(objClass)) {
		class(obj) <- objClass
	} else {
		class(obj) <- c("semiFrame", "tableMatrix", "tableList")
	}

	return(obj)
}

#
# Generics
#

# S3 semiFrame generic to get or set mapType attribute
#' @export
mapType <- function(...) { UseMethod("mapType") }
#' @export
'mapType<-' <- function(...) { UseMethod("mapType<-") }

# S3 semiFrame generic to get or set dataType attribute
#' @export
dataType <- function(...) { UseMethod("dataType") }
#' @export
'dataType<-' <- function(...) { UseMethod("dataType<-") }

# S3 semiFrame generic to get mat row matrix attribute
#' @export
getRowMat <- function(...) { UseMethod("getRowMat") }

# S3 semiFrame generic to get raster from a row of the matrix attribute
#' @export
getRowRas <- function(...) { UseMethod("getRowRas") }

# S3 semiFrame generic to get model output
#' @export
modelM <- function(...) { UseMethod("modelM") }

# S3 semiFrame generic to get rows used for model training
#' @export
modelTrainRow <- function(...) { UseMethod("modelTrainRow") }

# S3 semiFrame generic for object binding
#' @export
bind <- function(...) { UseMethod("bind") }

# S3 semiFrame generic for viewing data.table
#' @export
view <- function(...) { UseMethod("view") }

#
# Methods Functions
#

viewTable <- function(obj, n, options, class, ...) {
  
  showObj <- obj
  if (n > 0 && nrow(obj) > 2*n) {
    empty <- obj[0]
    empty[1,] <- NA
    showObj <- rbind(head(obj, n=n), obj[0L][1L], tail(obj, n=n))
    rownames(showObj) <- c(as.character(1:n), "...",as.character((nrow(obj)- n + 1):nrow(obj)))
  } 
  
  DT::datatable(showObj, class=class, options=options, ...)
}

#' View semiTable
#'
#' View semiTable
#' @export
view.semiTable <- function(obj, n=5L, options=list(pageLength = 25), class="display nowrap", ...) {	
  
  return(viewTable(obj$tab, n=n, options=options, class=class, ...))
}

#' View semiFrame
#'
#' View semiFrame
#' @export
view.semiFrame <- function(obj, n=5L, options=list(pageLength = 25), class="display nowrap", ...) {	
  
  return(viewTable(obj$tab[, setdiff(colnames(obj$tab),do.call(c,tmName)), with=FALSE], n=n, options=options, class=class, ...))
}

#' View data.table
#'
#' View data.table
#' @export
view.data.table <- function(obj, n=5L, options=list(pageLength = 25), class="display nowrap", ...) {	
  
  return(viewTable(obj, n=n, options=options, class=class, ...))
}

mergeDataTypeRef <- function(objTab, dtX, dtY) {

	if (!length(dtY)) { return(dtX) }

	objDataType <- list()

	dTypes <- intersect(names(dtX), names(dtY))
	if (length(dTypes)) {
		for (dType in dTypes)  { objDataType[[dType]] <- union(dtX[[dType]], dtY[[dType]]) }
	}
	dTypes <- setdiff(names(dtX), names(dtY))
	if (length(dTypes)) {
		for (dType in dTypes)  { objDataType[[dType]] <- dtX[[dType]] }
	}
	dTypes <- setdiff(names(dtY), names(dtX))
	if (length(dTypes)) {
		for (dType in dTypes)  { objDataType[[dType]] <- dtY[[dType]] }
	}

	if (sfDataType$meta%in%names(objDataType)) {
		colShiftRef(objTab, objDataType[[sfDataType$meta]], 1, destInSrc=1)

		metaEndIdx <- max(colj(objTab, objDataType[[sfDataType$meta]]))+1

		if (sfDataType$y%in%names(objDataType)) {
			colShiftRef(objTab, objDataType[[sfDataType$y]], metaEndIdx, destInSrc=metaEndIdx)
		}
		
		if (sfDataType$clust%in%names(objDataType)) {
			colShiftRef(objTab, objDataType[[sfDataType$clust]], metaEndIdx, destInSrc=metaEndIdx)
		}
	}

	return(objDataType)
}

updateDataType <- function(tabName, checkDataType) {

	for (dType in names(checkDataType)) {
		checkDataType[[dType]] <- intersect(tabName, checkDataType[[dType]])
		if (!length(checkDataType[[dType]])) { checkDataType[[dType]] <- NULL }
	}
	return(checkDataType)
}

#' Get or set table attribute
#'
#' semiTable method to get or set table attribute
#' @rdname tab.semiTable
#' @export
tab.semiTable <- function(obj, dataType=NULL, addRow=FALSE) {	

	objTab <- obj$tab
	if (!truelength(objTab)) {
		setDT(objTab)
	}

	if (!is.null(dataType)) {
		objTab <- objTab[, obj$aid$dataType[[dataType]], with=FALSE]
	} else {
		objTab <- copy(objTab)
	}

	if (addRow) {
		objTab[,c(tmName$allRow):=.I]		
	}

	return(objTab)
}
#' @rdname tab.semiTable
#' @export
'tab<-.semiTable' <- function(obj, value) {
	
	if (!is.data.table(value)) { stop("data.table object required") }
	obj$tab <- value
	return(obj) 
}


#' Get or Set Map Type Attribute
#'
#' @description Gets or sets \code{mapType} attribute.
#' 
#' @template sfobjParam
#' 
#' @details See \code{\link{mapPlotPar}} for more information on map types.
#' 
#' @aliases mapType
#' 
#' @return \code{mapType} attribute of the \code{semiFrame} object.
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Get map type
#' mapType(wbm)
#' 
#' @rdname mapType.semiFrame
#' 
#' @export
mapType.semiFrame <- function(obj) {	

	return(obj$aid$mapType)
}
#' @rdname mapType.semiFrame
#' @export
'mapType<-.semiFrame' <- function(obj, value) {
	
	obj$aid$mapType <- value	
	return(obj)
}

#' Manage Data Types
#'
#' @description Gets or sets \code{dataType} attribute of a \code{semiTable} or a \code{semiFrame} object.
#' 
#' @template stsfobjParam
#'  
#' @details Data types stored in the \code{dataType} attribute represent groups of variables.
#' If applicable, the following data types can be used:
#' \itemize{
#'   \item{ "META" }{ for Meta parameters. }
#'   \item{ "PC" }{ for PCM parameters. }
#'   \item{ "PCJ" }{ for PCM Junk parameters. }
#'   \item{ "MET" }{ for Metrology parameters. }
#'   \item{ "LEH" }{ for Lot Equipment History parameters. }
#'   \item{ "DEF" }{ for Defectivity parameters. }
#'   \item{ "Y" }{ for Yield variables. }
#'   \item{ "C" }{ for Clusters variables. }
#'   \item{ "FG" }{ for Fab Guard parameters. }
#'   \item{ "DR" }{ for Dimensionality Reduction variables. }
#'   \item{ "U" }{ for User-defined variables. }
#' }
#' 
#' @aliases dataType
#' 
#' @return A list of data types with corresponding variables.
#' 
#' @examples
#'  
#' data(pcy)
#' 
#' # Get the dataType attribute of the pcy object
#' dataType(pcy)
#' 
#' # Show data types
#' names(dataType(pcy))
#' 
#' # Show variable names from the 'PC' data type
#' dataType(pcy)$PC
#' 
#' @rdname dataType.semiFrame
#' 
#' @export
dataType.semiFrame <- function(obj) {	

	return(obj$aid$dataType)
}
#' @rdname dataType.semiFrame
#' @export
'dataType<-.semiFrame' <- function(obj, value) {
	
	obj$aid$dataType <- value	
	return(obj)
}
#' @rdname dataType.semiFrame
#' @export
dataType.semiTable <- function(obj) {	

	return(obj$aid$dataType)
}
#' @rdname dataType.semiFrame
#' @export
'dataType<-.semiTable' <- function(obj, value) {
	
	obj$aid$dataType <- value	
	return(obj)
}

#' Get the matrix corresponding to a row
#'
#' semiFrame method to get the matrix corresponding to a row
#' @export
getRowMat.semiFrame <- function(obj, i=NULL, repo=NULL) {	

	repo <- getRowRepo(obj, i, repo)
	rowDim <- getRowDim(obj, i, repo)
	rowMat <- getRow(obj, i, repo)
	rowMat <- matrix(rowMat, rowDim[1], rowDim[2], byrow=TRUE)
	return(rowMat)
}

#' Get the matrix corresponding to a row
#'
#' semiFrame method to get the matrix corresponding to a row
#' @export
getRowRas.semiFrame <- function(obj, i=NULL, repo=NULL) {	

	repo <- getRowRepo(obj, i, repo)
	rasDim <- getRowDim(obj, i, repo)
	ras <- raster::raster(nrow=rasDim[1], ncol=rasDim[2], xmn=0, xmx=rasDim[2], ymn=0, ymx=rasDim[1])
	raster::values(ras) <- getRow(obj, i, repo)
	return(ras)
}


#' Get Model Output
#'
#' @description Gets model output of the \code{\link{model}} function ie. the model itself.
#' 
#' @template stsfobjParam
#' 
#' @aliases modelM
#' 
#' @examples
#' data(pcy)
#' 
#' # Train CART model
#' pcy2 <- model(pcy, type="class", model="cart")
#' 
#' # Get the model itself
#' modelM(pcy2)
#' 
#' @return The model specific output.
#' 
#' @rdname modelM.semiTable
#' @export
modelM.semiTable <- function(obj) {

	return(obj$aid$model$par$m)
}

#' @template dataParam
#' @rdname modelM.semiTable
#' @export
modelM.semiFrame <- function(obj, data="last") {

	if (data=="last") { data <- obj$aid$lastData }
	return(obj$aid$model[[data]]$m)
}


#' Get Model Training Rows
#' 
#' @description Gets data rows employed in model training using the \code{\link{model}} function.
#' Useful for train-set reconstruction in train/test splits and response balancing cases.
#' 
#' @template stsfobjParam
#' 
#' @return Rows used for the training of a model.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Train a CART model
#' pcy2 <- model(pcy, type="class", model="cart", splitRatio=0.1)
#' 
#' # 10% of the data used for training
#' length(modelTrainRow(pcy2))
#' 
#' @rdname modelTrainRow.semiTable
#' 
#' @aliases modelTrainRow
#' 
#' @export
modelTrainRow.semiTable <- function(obj) {

	return(modelTrainRow.semiFrame(obj, data="par"))
}

#' @template dataParam
#' @rdname modelTrainRow.semiTable
#' @export
modelTrainRow.semiFrame <- function(obj, data="last") {

	if (data=="last") { data <- obj$aid$lastData }
		
	if (is.null(obj$aid$model[[data]]$balanceRow)) {
		return(which(obj$aid$model[[data]]$spl))
	} 

	return(obj$aid$model[[data]]$balanceRow[obj$aid$model[[data]]$spl])
}

#
# Standard Generics Methods
#

#' Bracket
#' 
#' S3 semiTable method passes data.table parameters to the table attribute
#' @export
'[.semiTable' <- function(x, ...) {

	x <- NextMethod()
	if (!is.null(nrow(x))) { 
		x$aid$dataType <- updateDataType(colnames(x$tab), x$aid$dataType)
	}
	return(x)
}

#' Bracket
#' 
#' S3 semiFrame method passes data.table parameters to the table attribute
#' @export
'[.semiFrame' <- function(x, ...) {

	x <- NextMethod()
	if (!is.null(nrow(x))) { 
		x$aid$dataType <- updateDataType(colnames(x$tab), x$aid$dataType)
	}
	return(x)
}

#' Merging semiTable
#' 
#' Merging semiTable objects
#' @export
merge.semiTable <- function(x,y, key="default", ...) {

	x <- copy(x)
	key <- findKey(x$tab, y$tab, key)
	x$tab <- merge(x$tab, y$tab, by=key, ...)
	x$aid$dataType <- mergeDataTypeRef(x$tab, dataType(x), dataType(y))
	return(x)
}

#' Merging semiFrame
#' 
#' Merging semiFrame and semiTable
#' @export
merge.semiFrame <- function(x,y, key="default", ...) {

	key <- findKey(x$tab, y$tab, key)
	x <- tableMatrix:::merge.tableMatrix(x, y$tab, key=key, ...)
	x$aid$dataType <- mergeDataTypeRef(x$tab, dataType(x), dataType(y))
	return(x)
}



#' Column-wise Binding
#' 
#' @description Binds \code{tab} part columns of two \code{semiTable} and/or \code{semiFrame} objects.
#' 
#' @param x \code{semiTable} or \code{semiFrame} object.
#' @param y \code{semiTable} or \code{semiFrame} object.
#' 
#' @details Part \code{aid} and if applicable parts \code{mat} and \code{matDim} are all inherited from the \code{x} argument.
#' 
#' @return A \code{semiTable} or \code{semiFrame} object with \code{tab} part columns binded.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Two semiTable objects with different PC parameters
#' pcyX <- rename(pcy, dataType(pcy)$PC, paste0("x_", dataType(pcy)$PC))
#' pcyY <- rename(pcy, dataType(pcy)$PC, paste0("y_", dataType(pcy)$PC))
#' 
#' # pcyX's and pcyY's PC parameters binded
#' pcyB <- cbind(pcyX, pcyY[,dataType(pcyY)$PC, with=FALSE])
#'  
#' @rdname cbind.semiTable
#' 
#' @aliases cbind
#' 
#' @export
cbind.semiTable <- function(x,y) {

	x <- copy(x)
	x$tab <- cbind(x$tab, y$tab)
	x$aid$dataType <- mergeDataTypeRef(x$tab, dataType(x), dataType(y))
	return(x)
}


#' @rdname cbind.semiTable
#' @export
cbind.semiFrame <- function(x,y) {

	x <- cbind.semiTable(x, y)
	return(x)
}

#
# Functions
#

#' semiTable testing
#' 
#' Tests if passed object is of class semiTable
#' @export
is.semiTable <- function(obj) {

	if ("semiTable"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' semiFrame testing
#' 
#' Tests if passed object is of class semiFrame
#' @export
is.semiFrame <- function(obj) {

	if ("semiFrame"%in%class(obj)) return(TRUE)
	return(FALSE)
}

#' Get Normalization Table 
#' 
#' @description Returns normalization table if present.
#' 
#' @template stsfobjParam 
#' @template verbose 
#' 
#' @return A \code{data.table} 
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Get normalization table
#' normTable(pcy)
#' 
#' @export
normTable <- function(obj, verbose=TRUE) {

	normTab <- list()
	normTab$norm <- obj$aid$normalize
	normTab$magn <- obj$aid$magnitude
	if (length(normTab)) { return(normTab) }
	ncatn("No normalization table", verbose=verbose)
	return(NULL)
}

extractDieRef <- function(diexyTab) {
	
	diexyTab[, c(sfName$diex):=list(as.integer(sub("[/].*$", "", diexy)))]
	diexyTab[, c(sfName$diey):=list(as.integer(sub("^.*[/]", "", diexy)))]
	return(invisible())
}

#' Crop Map
#' 
#' @description Crops or extends maps for alignment purposes. 
#' 
#' @template sfobjParam
#' @template mapBorderParam
#' @template coreParam
#' @param printProgress Integer. Defines the number of progress steps to be printed out (default: 10).
#' @template verbose
#' 
#' @return Copy of the \code{semiFrame} object with cropped (extended) maps.  
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Crop in all directions 
#' wbmCrop <- cropMap(wbm[1], mapBorder=rep(-3, 4))
#' # Extend in all directions
#' wbmExt <- cropMap(wbm[1], mapBorder=rep(3, 4))
#' 
#' # Compare maps
#' mapPlot(wbm[1], wbmCrop, wbmExt)
#' 
#' @export
cropMap <- function(obj, mapBorder=NULL, core=1, printProgress=10, verbose=TRUE) {

	if (is.null(mapBorder)) { return(obj) }

	cropLoop <- function(rows, progressRows) {


		cropObj <- tableMatrixWrap()
		for (i in rows) {
			mapMat <- getRowMat(obj, i)
			if (mapBorder[1]>0) {
				mapMat <- rbind(matrix(-1, mapBorder[1], ncol(mapMat)), mapMat)
			} else if (mapBorder[1]<0) {
				mapMat <- mapMat[-c(1:abs(mapBorder[1])),]
			}
			if (mapBorder[2]>0) {
				mapMat <- cbind(mapMat, matrix(-1, nrow(mapMat), mapBorder[2]))
			} else if (mapBorder[2]<0) {
				mapMat <- mapMat[,-c((ncol(mapMat)-abs(mapBorder[2])+1):ncol(mapMat))]
			}
			if (mapBorder[3]>0) {
				mapMat <- rbind(mapMat, matrix(-1, mapBorder[3], ncol(mapMat)))
			} else if (mapBorder[3]<0) {
				mapMat <- mapMat[-c((nrow(mapMat)-abs(mapBorder[3])+1):nrow(mapMat)),]
			}
			if (mapBorder[4]>0) {
				mapMat <- cbind(matrix(-1, nrow(mapMat), mapBorder[4]), mapMat)
			} else if (mapBorder[4]<0) {
				mapMat <- mapMat[,-c(1:abs(mapBorder[4]))]
			}
			cropObj <- rbind(cropObj, tableMatrixWrap(obj$tab[i][,c(tmName$matN, tmName$matRow):=list(as.integer(1), as.integer(1))], 
				list(t(as.vector(t(mapMat)))), setnames(data.table(1, nrow(mapMat), ncol(mapMat)), c(tmName$matN, sfName$matRows, sfName$matCols)), 
				setKey=TRUE))

			if (!is.null(progressRows)) {
				if (i %in% progressRows[,row]) {
					ncatn(progressRows[row==i, perc])
				}
			}
		}
	
		return(cropObj)
	}

	rowIdx <- getCoreIdx(1:nrow(obj), core)
	if (core>1) { on.exit(registerDoSEQ()) }
	
	progressRows <- NULL
	if (!is.null(printProgress) && verbose) {
		progressIdx <- unique(ceiling((1:printProgress)*length(rowIdx[[1]])/printProgress))
		progressPerc <- sprintf("%d%%",round(100*progressIdx/length(rowIdx[[1]])))
		if (length(progressPerc)>1) {
			progressPerc[1:(length(progressPerc)-1)] <- paste0("...", progressPerc[1:(length(progressPerc)-1)], "...")
		}
		progressPerc[length(progressPerc)] <- paste0("..", progressPerc[length(progressPerc)])
		progressRows <- data.table(row=(1:nrow(obj))[rowIdx[[1]][progressIdx]], perc=progressPerc)
	}

	# loop
	ncatn(sprintf("Cropping %d row(s), using %d core(s)...", nrow(obj), core), verbose=verbose)
	if (core>1) {
		
		cropObj <- foreach (coreIdx=seq_along(rowIdx), .combine=rbind) %dopar% {
			cropLoop((1:nrow(obj))[rowIdx[[coreIdx]]], progressRows)
		}

	} else {
		
		cropObj <- cropLoop(rowIdx[[1]], progressRows)
	
	}


	cropObj$aid <- obj$aid
	class(cropObj) <- class(obj)

	return(cropObj)
}

#' Map to Die Table
#' 
#' @description Creates a die table from a map.
#' 
#' @export
mapToDieTab <- function(obj, lotWaferSub=NULL, addCol=NULL, core=1) {

	loop <- function(rows, obj, mapMat, die, addCol) {

		dieTab <- NULL
		
		for (i in rows) {
			dieTabWafer <- data.table::data.table(lotWaferDie=paste(obj[["lotWafer"]][i], die, sep="-"))
			if (!is.null(addCol)) {
				dieTabWafer <- cbind(dieTabWafer, obj$tab[i,addCol,with=FALSE])	
			}
			dieTabWafer <- cbind(dieTabWafer, data.table::data.table(bin=mapMat[i,]))
			dieTabWafer <- data.table::data.table(dieTabWafer)		
			if (is.null(dieTab)) {
				dieTab <- dieTabWafer
			} else {
				dieTab <- rbind(dieTab, dieTabWafer)
			}
		}
		return(dieTab)
	}
	
	if (!is.null(lotWaferSub)) {
		obj <- obj[lotWafer %in% lotWaferSub]
	}
	
	mapDim <- as.integer(tableMatrix::matDim(obj)[,-1])
	dieRow <- rep(1:mapDim[1], each=mapDim[2])
	dieCol <- rep(1:mapDim[2], mapDim[1])
	die <- paste0("D", dieRow, "_", dieCol)
	
	mapMat <- obj$mat[[1]]
	idx <- which(mapMat[1,]<0)
	die <- die[-idx]
	mapMat <- mapMat[,-idx]
		
	rowIdx <- getCoreIdx(1:nrow(obj), core)

	if (core>1) {
		on.exit(registerDoSEQ())
		
		dieTab <- foreach (coreIdx=seq_along(rowIdx), .combine=rbind) %dopar% {
			loop(rowIdx[[coreIdx]], obj, mapMat, die, addCol)
		}
		
	} else {
		
		dieTab <- loop(rowIdx[[1]], obj, mapMat, die, addCol)
	}
	
	dieTab[,c("fail"):=list(as.integer(bin>0))]
	
	dieObj <- inLabZero:::semiTableWrap(dieTab, list(META="lotWaferDie", Y=c("bin", "fail")))
	
	return(dieObj)

}


#' Defectivity per Die 
#' 
#' @description Matches defect maps with wafer bin maps and creates die-level table ie. each row represents a die.
#' 
#' @param wbmObj `semiFrame` object representing the wafer bin map dataset.
#' @param demObj `semiFrame` object representing the defect maps dataset.
#' @param wbmAlign Integer. User defined alignment of wafer bin maps. 
#'                 First to the fourth element corresponds to the top, right, bottom, left side of the map (default: \code{NULL}).
#' @param demAlign Integer. User defined alignment of defect maps. 
#'                 First to the fourth element corresponds to the top, right, bottom, left side of the map (default: \code{NULL}).
#' @param NAtoZero Logical. If \code{TRUE}, all the NAs will be set to zero (default: \code{FALSE}).
#' @param def `semiFrame` object representing defects dataset. Will be used to fill inheriting information if provided. (default: \code{NULL}).
#' @param ded `semiFrame` object representing defects per die dataset from previous run of \code{defDie} (default: \code{NULL}).
#' @template coreParam
#' @template verbose
#' 
#' @return Die-level \code{semiTable} object. Column \code{ALL} represents defect count on the die.
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
#' @export
defDie <- function(wbmObj, demObj, wbmAlign=NULL, demAlign=NULL, NAtoZero=FALSE, 
						def=NULL, ded=NULL, core=8, verbose=TRUE) {

	uniqSteps <- unique(demObj[[sfName$step]])

	if (is.null(wbmAlign)) {
		if (!is.null(demObj$aid$wbmAlign)) {
			wbmAlign <- demObj$aid$wbmAlign
		}
	}

	if (is.null(demAlign)) {
		if (!is.null(demObj$aid$demAlign)) {
			demAlign <- demObj$aid$demAlign
		}
	}

	if (length(uniqSteps)>1) {

		if (core>length(uniqSteps)) { core <- length(uniqSteps) }

		if (is.null(ded)) {

			if (core>1) {
				registerDoMC(core)
				on.exit(registerDoSEQ())

				chipTableCombine <- function(obj1, obj2) {
					obj <- rbind(obj1, obj2, fill=TRUE)
					dataType(obj)[[sfDataType$def]] <- union(dataType(obj1)[[sfDataType$def]], dataType(obj2)[[sfDataType$def]])
					return(obj)
				}

				chipObjAllSteps <- foreach (uniqStep=uniqSteps, .combine=chipTableCombine) %dopar% {
					defDie(wbmObj=wbmObj, demObj=demObj[eval(.sfName$step)==uniqStep], 
								wbmAlign=wbmAlign, demAlign=demAlign, NAtoZero=NAtoZero)
				}
				gc()

			} else {

				chipObjAllSteps <- tableListWrap()
				for (uniqStep in uniqSteps) {
					chipObjStep <- defDie(wbmObj=wbmObj, demObj=demObj[eval(.sfName$step)==uniqStep], 
								wbmAlign=wbmAlign, demAlign=demAlign, NAtoZero=NAtoZero)

					chipObjAllSteps <- rbind(chipObjAllSteps, chipObjStep, fill=TRUE)
					dataType(chipObjAllSteps)[[sfDataType$def]] <- union(dataType(chipObjAllSteps)[[sfDataType$def]],
																		dataType(chipObjStep)[[sfDataType$def]])
				}
			}
		} else {
			chipObjAllSteps <- copy(ded)
		}
		if (!is.null(def)) {
			ncatn("Defects inheriting...", verbose=verbose)
			fromCols <- colnames(def)[grep("^From",colnames(def))]
			if (!length(fromCols)) { 
				ncatn("No defects inheriting data",verbose=verbose)
				return(chipObjAllSteps)
			} 
			for (fromCol in fromCols) {
				ncatn(fromCol, verbose=verbose)
				chipObjAllSteps <- variable(chipObjAllSteps, fromCol, as.integer(NA), sfDataType$def)
				idx <- which(def[[fromCol]]=="no")
				if (length(idx)) {
					lwschMatch <- match(unique(def[[sfName$lotWaferStepDie]][idx]), chipObjAllSteps[[sfName$lotWaferStepDie]])
					lwschMatch <- lwschMatch[-which(is.na(lwschMatch))]
					if (length(lwschMatch)) {
						chipObjAllSteps$tab[lwschMatch,c(fromCol):=list(0L)]
					}
				}

				idx <- which(def[[fromCol]]!="no")
				if (!length(idx)) {
					ncatn("No defects inherited" ,verbose=verbose)
					next
				}
				toFromTab <- table(def[[sfName$lotWaferStepDie]][idx], rep(1L, length(idx)))
				lwschMatch <- match(rownames(toFromTab), chipObjAllSteps[[sfName$lotWaferStepDie]])
				tabMatchNA <- is.na(lwschMatch)
				if (all(tabMatchNA)) {
					ncatn("No match in lotWaferStepDie for inherited defects",verbose=verbose)
					next
				}
				tabMatchNA <- which(tabMatchNA)
				toFromTab <- toFromTab[-tabMatchNA]
				lwschMatch <- lwschMatch[-tabMatchNA]

				ncatn(sprintf("%d dice across steps inherited a defect", length(lwschMatch)))
				chipObjAllSteps$tab[lwschMatch,c(fromCol):=list(toFromTab)]
			}
		}

		return(chipObjAllSteps)
	}

	ncatn(sprintf("Step %s",uniqSteps),verbose=verbose)

	chipObj <- data.table()

	lotWaferIntersect <- intersect(wbmObj[[sfName$lotWafer]], demObj[[sfName$lotWafer]])

	if (!length(lotWaferIntersect)) { stop("No wbm and kla intersect") }

	wbmObj <- wbmObj[eval(.sfName$lotWafer)%in%lotWaferIntersect]
	demObj <- demObj[eval(.sfName$lotWafer)%in%lotWaferIntersect]

	for (i in 1:length(lotWaferIntersect)) {
		wbm <- cropMap(wbmObj[eval(.sfName$lotWafer)==lotWaferIntersect[i]], mapBorder=wbmAlign, verbose=FALSE)
		dem <- cropMap(demObj[eval(.sfName$lotWafer)==lotWaferIntersect[i]], mapBorder=demAlign, verbose=FALSE)

		if (any(getRowDim(wbm, 1)!=getRowDim(dem, 1))) { stop("wbma and kla dimensions do not match") }
		
		wbmVec <- wbm$mat[[1]][1,]
		klaVec <- dem$mat[[1]]
		indexIntersect <- which(wbmVec>=0&klaVec[1,]>=0)

		chipWafer <- setnames(data.table(indexIntersect, as.integer(wbmVec[indexIntersect]>0), wbmVec[indexIntersect]),c(sfName$die, sfName$fail, sfName$bin))
		for (j in 1:nrow(klaVec)) {
			
			chipWafer[,c(dem[[sfName$defClass]][j]):=list(klaVec[j, indexIntersect])]

		}

		chipWafer <- cbind(dem$tab[1,c(sfName$lotWafer,sfName$lot, sfName$wafer, sfName$step, sfName$inspector, sfName$tech, sfName$maskset),with=FALSE], chipWafer)		

		chipObj <- rbind(chipObj, chipWafer, fill=TRUE)
	}

	chipCol <- paste0("D",formatC(chipObj[[sfName$die]], width=ceiling(log(max(chipObj[[sfName$die]]),10)), format="d", flag="0"))
	lotWaferDie <- paste(chipObj[[sfName$lotWafer]], chipCol,sep="-")
	colShiftRef(chipObj[,c(sfName$lotWaferDie):=list(lotWaferDie)], sfName$lotWaferDie, sfName$lotWafer)
	lotWaferStepDie <- paste(chipObj[[sfName$lotWafer]], chipObj[[sfName$step]], chipCol, sep="-")
	colShiftRef(chipObj[,c(sfName$lotWaferStepDie):=list(lotWaferStepDie)], sfName$lotWaferStepDie, sfName$lotWaferDie)

	metaCol <- colnames(chipObj)[1:match(sfName$bin, colnames(chipObj))]
	defCol <- setdiff(colnames(chipObj), metaCol)

	if (NAtoZero) {
		for (col in defCol) {
			na2zero <- chipObj[[col]]
			na2zero[is.na(na2zero)] <- 0
			chipObj[, c(col):=list(na2zero)]
		}
	}

	chipObjDataType <- list(setdiff(metaCol,c(sfName$bin, sfName$fail)), c(sfName$bin, sfName$fail), defCol)
	names(chipObjDataType) <- c(sfDataType$meta, sfDataType$clust, sfDataType$def)
	
	chipObj <- semiTableWrap(chipObj, chipObjDataType)

	return(chipObj)
}

#' Defects Inheriting
#'
#' @description Computes percentages of defects inherited from previous manufacturing steps from defect inspection data.
#' 
#' @param obj \code{semiTable} object. Applicable to defects dataset or to defects per die dataset.
#' @template stepParam
#' @param type Character. Specifies the type of dataset: "auto" (default),
#'      "def" corresponding to defects dataset or "die" corresponding to defects per die dataset.
#' @template verbose
#'
#' @return Percentages of defects inherited or percentages of dice that inherit one or more defect.
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Defects dataset
#' data(def)
#' # Defect maps dataset
#' data(dem)
#' # Defects per die dataset
#' ded <- defDie(wbm, dem, def=def)
#' 
#' # Percentages of defects inherited
#' defInherited(def, steps=c("STEP1", "STEP2"))
#' # Percentages of dice that inherit one or more defect
#' defInherited(ded, steps=c("STEP1", "STEP2"))
#' 
#' @export
defInherited <- function(obj, steps, type="auto", verbose=TRUE) {
	
	checkCharInput(type, c("def", "die", "auto"), "values")

	stepInherited <- function(obj, from, to, type) {

		obj <- obj$tab[step==to]
		fromIdx <- which(steps==from)
		toIdx <- which(steps==to)

		denom <- TRUE
		for (i in 1:(toIdx-1)) {
			if (type=="def") {
				denom <- denom & !is.na(obj[[fsteps[i]]])
			} else {
				denom <- denom & obj[[fsteps[i]]]>=0
			}
		}

		denom <- which(denom)
		if (!length(denom)) { return(as.numeric(NA)) }

		obj <- obj[denom]

		numer <- TRUE
		if ((fromIdx-1)>0) {
			for (i in 1:(fromIdx-1)) {
				
				if (type=="def") {
					numer <- numer & obj[[fsteps[i]]]=="no"
				} else {
					numer <- numer & obj[[fsteps[i]]]==0
				}
			}
		}

		if (type=="def") {
			numer <- numer & obj[[fsteps[fromIdx]]]!="no"
		} else {
			numer <- numer & obj[[fsteps[fromIdx]]]>0
		}

		return(round(100*sum(numer)/length(denom),1))
	}

	if (type=="auto") {
		if (sfName$lotWaferStepDieXYRel %in% colnames(obj)) {
			type <- "def"
		} else {
			type <- "die"
		}
	}

	if (type=="def") {
		ncatn("Percentages of defects inherited", verbose=verbose)
	} else {
		ncatn("Percentages of dice that inherit one or more defect", verbose=verbose)
	}

	fsteps <- paste0("From", steps)
	percMat <- matrix(as.numeric(NA), length(steps)-1, length(steps)-1)
	colnames(percMat) <- steps[-length(steps)]
	rownames(percMat) <- steps[-1]

	for (i in 1:nrow(percMat)) {
		for (j in 1:ncol(percMat)) {
			if (j>i) { next }
			percMat[i,j] <- stepInherited(obj, from=colnames(percMat)[j], to=rownames(percMat)[i], type=type)
		}
	}

	percMat <- cbind(percMat, rowSums(percMat, na.rm=TRUE))	
	colnames(percMat)[ncol(percMat)] <- "total"

	return(percMat)
}

#
# End
#