#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# Tools
# 
# 0.70.0
# 


#
# Internal
#

userColInput <- function(..., colName, useDataType, dataType, col=NULL, envir=parent.frame()) {
		

	if (!is.null(useDataType)) {
		useDataType <- toupper(useDataType)
		if (!any(useDataType%in%sfDataTypes)) { stop(sprintf("Only %s data types supported", paste(sfDataTypes, collapse=", "))) }
		if (!any(useDataType%in%names(dataType))) { stop(sprintf("dataType %s not available", useDataType)) }
		useDataType <- useDataType[min(which(useDataType%in%names(dataType)))]

	}
	
	if (!is.null(col)) { 
		if (is.numeric(col)) {
			col <- colj(dataType[[useDataType]], list(j=col), sortj=FALSE, numj=FALSE)
			return(list(col=col, useDataType=useDataType)) 
		}
		return(list(col=col, useDataType=useDataType)) 
	}
	
	argsList <- eval(substitute(alist(...)))
	if (!length(argsList)) { return(list(col=character(), useDataType=useDataType)) }

	col <- character()
	for (i in 1:length(argsList)) {
		if (!is.character(argsList[[i]])) {
			argChar <- deparse(argsList[[i]])
		} else {
			argChar <- argsList[[i]]
		}
		if (length(grep("^c[(]", argChar))) { 
			argEval <- eval(parse(text=argChar), envir=envir)
			if (is.character(argEval)) {
				col <- c(col, colj(colName, list(j=argEval), sortj=FALSE, numj=FALSE))
			} else {
				col <- c(col, colj(dataType[[useDataType]], list(j=argEval), sortj=FALSE, numj=FALSE))
			}
			next
		}
		if (length(grep("[:]", argChar))) { 
			rangeLo <- sub("[:].*$","",argChar)
			rangeLoInt <- suppressWarnings(as.integer(rangeLo))
			rangeUp <- sub("^.*[:]","",argChar)
			rangeUpInt <- suppressWarnings(as.integer(rangeUp))
			if (is.na(rangeLoInt)||is.na(rangeUpInt)) {
				col <- c(col, colj(colName, list(r=c(rangeLo, rangeUp)), sortj=FALSE, numj=FALSE))
			} else {
				col <- c(col, colj(dataType[[useDataType]], list(r=c(rangeLoInt, rangeUpInt)), sortj=FALSE, numj=FALSE))
			}
			next	
		}
		argInt <- suppressWarnings(as.integer(argChar))
		if (is.na(argInt)) {
			col <- c(col, colj(colName, list(j=argChar), sortj=FALSE, numj=FALSE))
		} else {
			col <- c(col, colj(dataType[[useDataType]], list(j=argInt), sortj=FALSE, numj=FALSE))
		}
	}
	return(list(col=col, useDataType=useDataType))
}

xtsMedianNAfill <- function(xtsData, consolidate=TRUE) {

	xtsDates <- zoo::index(xtsData)
	uniqDates <- unique(xtsDates)
	medsByUniqDates <- numeric()
	lastMedian <- 0
	for (uniqDate in uniqDates) {
		idxDate <- which(xtsDates==uniqDate)
		if (length(idxDate)>1) {
			lastMedian <- median(xtsData[idxDate],na.rm=TRUE)
			medsByUniqDates <- c(medsByUniqDates, lastMedian)
		} else {
			if (is.na(xtsData[idxDate])) {
				xtsData[idxDate] <- lastMedian
				medsByUniqDates <- c(medsByUniqDates, lastMedian)
			} else {
				medsByUniqDates <- c(medsByUniqDates, xtsData[idxDate])
			}
		}
	}

	if (consolidate) {
		xtsData <- xts::xts(medsByUniqDates, uniqDates)
	}

	return(xtsData)
}

findKey <- function(xTab, yTab, key, getKeys=FALSE) {

	if (!any(key=="default")) { return(key) }

	fKey <- function(object) {
		return(colj(object,with(sfName,c(lotWaferSiteid, lotWafer, lot)), sortj=FALSE, numj=FALSE))
	}

	xKey <- fKey(xTab)
	yKey <- fKey(yTab)
	key <- intersect(xKey, yKey)
	if (!length(key)) { stop("There is no key intersect") }

	if (getKeys) { return(list(key=key[1], xKey=xKey, yKey=yKey)) }

	return(key[1])
}

findDataType <- function(obj, col) {

	objDataType <- dataType(obj)
	colDataType <- list()
	for (dType in names(objDataType)) {
		for (colName in col) {
			if (colName%in%objDataType[[dType]]) {
				if (is.null(colDataType[[dType]])) {
					colDataType[[dType]] <- colName
				} else {
					colDataType[[dType]] <- c(colDataType[[dType]], colName)
				}
			}
		}
	}
	return(colDataType)
}

#' Outliers' Filtering 
#' 
#' @description Filters outliers using IQR multiples.
#' 
#' @template stsfobjParam
#' @param col Character. Specifies single column for outlier filtering.
#' @param k Either a single value \code{kl=kr} or a vector of two values \code{c(kl, kr)}.
#'               These \code{k} values are then used for the definition of outliers i.e. data points outside the interval:
#'               \deqn{<(Q1 - kl * IQR), (Q3 + kr * IQR)>,} where \code{Q1} and \code{Q3} are quartiles,
#'               \code{IQR} is the interquartile range (default: 3).
#' @param output Character. Possible values:
#' \itemize{ 
#'   \item{ "obj" }{ Returns copy of \code{obj} without outlier rows.} 
#'   \item{ "data" }{ Returns column data with outliers as \code{NAs}. } 
#'   \item{ "logical"}{ Returns logical vector with \code{FALSE} values indicating outliers. } 
#'   \item{ "tab" }{ Returns \code{tab} part of a \code{semiTable} or \code{semiFrame} object without outlier rows. } 
#' }
#' @param colData Internal. Direct data input (default: \code{NULL}).
#' 
#' @details  For yield variables, all values below 0 are set to 0 and all values above 100 are set to 100. 
#'           Then, remaining outliers are filtered.
#' 
#' @return Copy of the \code{semiTable} or \code{semiFrame} object with filtered out outliers by default.
#' See parameter \code{output} for more options.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Filter out PARAM43 outliers and return copy of pcy
#' outlierFilter(pcy, "PARAM43", k = 2)
#' 
#' # Get column 'PARAM01' data with outliers denoted as NAs
#' outlierFilter(pcy, "PARAM01", k = 3, output="data")
#' 
#' @export
outlierFilter <- function(obj, col, k=3, output="obj", colData=NULL) {


	outlierLo <- function(colData, k) { 
		if (is.na(k[1])) { return(logical(length(colData))) } 
		return(colData<(quantile(colData,0.25, na.rm=TRUE)-k[1]*IQR(colData, na.rm=TRUE))) 
	}

	outlierHi <- function(colData, k) { 
		if (is.na(k[2])) { return(logical(length(colData))) } 
		return(colData>(quantile(colData,0.75, na.rm=TRUE)+k[2]*IQR(colData, na.rm=TRUE))) 
	}

	outlierLH <- function(colData, k) {	return(outlierLo(colData, k)|outlierHi(colData, k)) }

	applyCon <- function(colData, con, val) {
		con <- which(con)
		if (length(con)) { colData[con] <- val }
		return(colData)
	}

	if (is.null(colData)) { colData <- obj$tab[[col]] }

	if (!is.null(k)) {  

		if (length(k)==1) { k <- rep(k, 2) }

		if (col%in%dataType(obj)[[sfDataType$y]]) {
			colData <- applyCon(colData, colData>100, 100)
			colData <- applyCon(colData, colData<0, 0)
			colData <- applyCon(colData, outlierLo(colData, k=k), NA)
		} else {
			colData <- applyCon(colData, outlierLH(colData, k=k), NA)
		}
	}

	if (output=="data") { return(colData) }
	if (output=="logical") { return(!is.na(colData)) }
	if (output=="tab") { return(obj$tab[!is.na(colData)]) }
	return(obj[!is.na(colData)])
}

breakData <- function(colData, br, brMethod, duplSignif=3, getMids=FALSE) {

	
	checkCharInput(brMethod, c("median", "mean", "thr", "yieldHeat", "linear", "hist"))

	minCol <- min(colData)
	maxCol <- max(colData)
	medianCol <- median(colData, na.rm=TRUE)
	meanCol <- mean(colData, na.rm=TRUE)

	breakLinear <- function() { return(minCol + (0:br)*(maxCol-minCol)/br) }
	breakSpecial <- function(str) {
		br <- numeric()
		for (i in 1:length(str)) {
			if (length(grep("^seq[(]", str[i]))) {  
				strEval <- str[i]
				strEval <- sub("min", "minCol", strEval)
				strEval <- sub("max", "maxCol", strEval)
				strEval <- sub("median", "medianCol", strEval)
				strEval <- sub("mean", "meanCol", strEval)
				br <- c(br, eval(parse(text=strEval)))
				next
			}
			if (str[i]=="min") { br <- c(br, minCol); next }
			if (str[i]=="max") { br <- c(br, maxCol); next }
			if (str[i]=="median") { br <- c(br, medianCol); next }
			if (str[i]=="mean") { br <- c(br, meanCol); next }
			strNum <- suppressWarnings(as.numeric(str[i]))
			if (!is.na(strNum)) {  br <- c(br, strNum) }
		}
		return(br)
	}

	if ("POSIXct"%in%class(colData)) {
		if (brMethod=="yieldHeat") { brMethod <- "linear" }
	}

	mids <- numeric()
	if (length(br)>1) {
		if (is.character(br)) { br <- breakSpecial(br) }
	} else if (brMethod=="thr") {
		br <- c(minCol, br, maxCol)	
	} else if (brMethod=="yieldHeat") {
		br <- breakSpecial(c(sprintf("seq(min, median, length.out=%d)", br),"max"))	
	} else if (brMethod=="hist") {
		br <- hist(colData, breaks=br, include.lowest=TRUE, plot=FALSE) 
		mids <- br$mids
		br <- br$breaks
	} else if (br>2) { 
		br <- breakLinear()
	} else if (br==1) { 
		br <- c(minCol, maxCol)				
	} else if (brMethod=="median") {
		br <- c(minCol, medianCol,maxCol)				
	} else if (brMethod=="mean") {
		br <- c(minCol, meanCol,maxCol)	
	} else {
		br <- breakLinear()
	}

	if (any(duplicated(br))) {
		for (i in 1:length(br)) {
			bri <- which(br[i]==br)
			if (length(bri)>1) {
				br[i] <- br[i]-10^floor(log(signif(br[i],1), base=10))/10^duplSignif
			}
		}
	}

	if (getMids) {
		if (!length(mids))  {
			for(i in seq_along(br)[-length(br)]) {
				mids[i] <- mean(br[i], br[i+1])
			}
		}
		return(list(br=br, mids=mids))
	}

	return(br)
}

checkCharInput <- function(inputVal, inputVals, label="types", allowNULL=TRUE, inputName=NULL) {

	if (allowNULL&&is.null(inputVal)) { return(invisible()) }

	if (!inputVal%in%inputVals) {

		inputVals <- sapply(inputVals, function(type) paste0("\"", type, "\"") )
		if (length(inputVals)>2) {
			oneStrTypes <- paste(inputVals[1:(length(inputVals)-1)], collapse=", ")
		} else {
			oneStrTypes <- inputVals[1]
		}
		oneStrTypes <- paste(oneStrTypes, inputVals[length(inputVals)], sep=" and ")

		if (is.null(inputName)) {
			inputName <- deparse(substitute(inputVal))
		}

		stop(sprintf("Supported %s %s: %s.", inputName, label, oneStrTypes)) 
	}

	return(invisible())
}

#
# Public
#

#' Removing character parts
#'
#' Removing character parts, from left or from right
#' @export
remChar <- function(obj, ..., remLeft=0, remRight=0, keep=0, charLen=NULL, inRegex=NULL, exRegex=NULL, regexCol=NULL, useDataType=c("PC","MET","LEH","DEF"), col=NULL) {

	userCol <- userColInput(..., colName=colnames(obj), useDataType=useDataType, dataType=dataType(obj), col=col)
	col <- userCol$col
	useDataType <- userCol$useDataType

	objTab <- tab(obj)

	row <- TRUE
	if (!is.null(charLen)) {
		row <- nchar(objTab[[col]])%in%charLen
	}

	if (!is.null(inRegex)||!is.null(exRegex)) {
		if (is.null(regexCol)) { regexCol <- col }
	}

	if (!is.null(inRegex)) {
		row <- row&grepl(inRegex, objTab[[regexCol]])
	}
	
	if (!is.null(exRegex)) {
		row <- row&!grepl(exRegex, objTab[[regexCol]])
	}

	if (!any(row)) { return(obj) }

	if (keep==0) {
		objTab[row, c(col):=list(substr(objTab[[col]][row], remLeft+1, nchar(objTab[[col]][row])-remRight))]
	} else {
		if (remLeft>0) {
			objTab[row,c(col):=list(substr(objTab[[col]][row], remLeft+1, keep+remLeft))]
			remRight <- 0
		}
		if (remRight>0) {
			objTab[row,c(col):=list(substr(objTab[[col]][row], 
				nchar(objTab[[col]][row])-remRight-keep, nchar(objTab[[col]][row])-remRight))]
		}
	}

	tab(obj) <- objTab
	return(obj)
}

#' Degrade to Lower Level
#' 
#' @description Degrades to lot or wafer level and aggregates column values.
#' 
#' @template stsfobjParam
#' @param degradeTo Character. Specifies the level to which the data should be degraded to: 
#' \itemize{
#'   \item{ "wafer" }{ for wafer level degradation. }
#'   \item{ "lot" }{ for lot level degradation. }
#' }
#' @param fun Function. Specifies aggregation function. For example \code{median} (default) or \code{mean}.
#' 
#' @return \code{semiTable} or \code{semiFrame} object
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Degrade site PCM data to wafer level
#' degrade(pcy, "wafer")
#' 
#' # Degrade site PCM or wafer level data to lot level
#' degrade(pcy, "lot")
#' 
#' @export
degrade <- function(obj, degradeTo, fun=median) {

	objDataType <- dataType(obj)
	if (degradeTo=="wafer") {
		degradeTo <- sfName$lotWafer
		metaCol <- setdiff(objDataType[[sfDataType$meta]], with(sfName, c(lotWaferSiteid, siteid, lotWafer)))
	} else if (degradeTo=="lot") {
		degradeTo <- sfName$lot
		metaCol <- setdiff(objDataType[[sfDataType$meta]], with(sfName, c(lotWaferSiteid, siteid, lotWafer, wafer, lot)))
	} else {
		return(obj)
	}	
	dataCol <- character()
	for (i in 1:length(objDataType)) {
		if (names(objDataType)[i]!=sfDataType$meta) {
			dataCol <- c(dataCol, objDataType[[i]])
		}
	}

	degrObjData <- obj[,lapply(.SD, function(subCol) { return(do.call(fun, list(subCol, na.rm=TRUE))) } ), 
							by=degradeTo, .SDcols=c(dataCol)]
	degrObjMeta <- obj[,lapply(.SD, function(subCol) { return(subCol[1]) } ), 
							by=degradeTo, .SDcols=c(metaCol)]

	return(merge(degrObjMeta, degrObjData))
}

#' Bin Manipulation in Maps
#'
#' @description Bin manipulation in maps represented by a \code{semiFrame} object. In the case of Wafer Bin Maps
#' zero bin occurrences represent good dice. Any non zero bin represents a failed die.
#' 
#' @template sfobjParam
#' @param old Integer, Character. Specifies bin or bins for manipulation. These bins are always cleared i.e. set to 0 after manipulation 
#' (except if \code{new} bin is in \code{old} bins).
#' Use keyword "all" for all the bins. Based on extract regime:
#' \itemize{
#'   \item{ \code{extract=TRUE} }{ all the bins will be cleared i.e. set to 0 after manipulation (except the \code{new} bin). }
#'   \item{ \code{extract=FALSE} }{ bins not specified in \code{old} will not be altered after manipulation. }
#' }
#' @param new Integer. Specifies single target bin. If the target bin is not present, it will be created (default: 1). 
#' @param extract Logical. Relevant for \code{new > 0}. Provides automated housekeeping. If TRUE then all the bins (except target bin spcified in \code{new}) will be cleared i.e. set to 0 after the manipulation process (default: TRUE).
#' 
#' @return Returns a \code{semiFrame} object with new bin values.
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Create 1 fail map: Combine all the bins into bin 1
#' bin(wbm, "all", 1)
#' 
#' # Extract: Combine bins 2, 3, 5 into bin 1 and clear all other bins except bin 1
#' bin(wbm, c(2,3,5), 1)
#' 
#' # Merge: Combine bins 2, 3 into bin 2, clear bin 3 and retain all other bin occurrences
#' bin(wbm, c(2,3), 2, extract=FALSE)
#' 
#' # Erase: Clear bin 3 and retain all other bins
#' bin(wbm, 3, 0)
#' 
#' @export
bin <- function(obj, old, new=1, extract=TRUE) {

	if (new==0) { extract <- FALSE }

	for (matN in obj$matDim[[1]]) {
		matData <- obj$mat[[matN]]
		saveDim <- dim(matData)
		matData <- as.vector(matData)
		if (any(old=="all")) {
			idx <- which(matData>0)
			if (!length(idx)) { next }
			matData[idx] <- new
		} else {
			matDataInOld <- matData%in%old
			if (extract) {
				idx <- which((!matDataInOld)&matData>0)
				if (length(idx)) { matData[idx] <- 0 }
			}
			idx <- which(matDataInOld)
			if (length(idx)) { matData[idx] <- new }
		}
		dim(matData) <- saveDim
		obj$mat[[matN]] <- matData
	}
	
	if (any(old=="all")&&new==1) {
		obj$aid$mapType <- sfMapType$bbin		
	} 

	return(obj)
}

#' Transfer Variables
#'
#' @description Transfers variables between different level frames.
#' 
#' @param from \code{semiTable} or \code{semiFrame} object.
#' @param to \code{semiTable} or \code{semiFrame} object.
#' @param col Character. Specifies column name or names to be transferred. A data type can be used for whole group transfer.
#' See \code{\link{dataType}} for more information on data types.
#' @param filter Filter used for conditioning values of transffered variables (default: \code{NULL}).
#' @param method Character. Specifies aggregation function. For example \code{majority} (default), 
#' \code{mean}, \code{median}, \code{max}, \code{min}.
#' @param fromData Character. Defines source data ("map" or "par") (default: "par").
#' @param toData Character. Defines target data ("map" or "par") (default: "par").
#' @param key Character. Specifies merging key for the variable transfer between different 
#' level frames (default: "default" - key will be chosen automatically as the highest intersect of granularities).
#' @param toDataType Character. Used for specifying target \code{dataType} in \code{from=="map" && to=="par"} scenarios.
#' See \code{\link{dataType}} for more information on data types (default: \code{NULL}).
#' @param toColNameBase Character. Used for specifying new column names in \code{from=="map" && to=="par"} scenarios (default: "map").
#' @template verbose
#' 
#' @seealso \code{\link{dataType}} for available data types. 
#' 
#' @return Copy of the \code{semiTable} or \code{semiFrame} object.
#' 
#' @examples
#' 
#' data(pcy)
#' data(wbm)
#' 
#' ## Transfer to higher level 
#' # 1. Create a new cluster in wbm that will be used for transfer
#' wbm2 <- cluster(wbm, from=edge, to=newCluster)
#' # 2. Transfer cluster newCluster from wbm2 to pcy
#' transfer(wbm2, pcy, "newCluster")
#' # 3. Transfer requesting PCMYield below 80%
#' transfer(wbm2, pcy, "newCluster", filter=pcy[,PCMYield]<80)
#' 
#' ## Transfer to lower level
#' # 1. Create a new cluster in pcy2 that will be used for transfer
#' pcy2 <- cluster(pcy, from=edge, to=newPcyCluster)
#' # 2. Transfer cluster newPcyCluster from pcy2 to wbm
#' transfer(pcy2, wbm, "newPcyCluster", method="max")
#' # 3. Transfer mean for the PARAM62 from pcy to wbm
#' transfer(pcy, wbm, "PARAM62", method="mean")
#' 
#' @export
transfer <- function(from, to, col, filter=NULL, method="majority", fromData="par", toData="par", 
					key="default", toDataType=NULL, toColNameBase="map", verbose=TRUE) {


	checkCharInput(fromData, c("par", "map"))
	checkCharInput(toData, c("par", "map"))

	if (fromData=="map" && !is.semiFrame(from)) { fromData <- "par" }
	if (toData=="map" && !is.semiFrame(to)) { toData <- "par" }

	if (fromData=="map") {

		if (!is.integer(col)) { stop("Specify integer indices of map columns in 'col'.") }
		from <- copy(from)
		colIdx <- col
		col <- paste0(toColNameBase, colIdx)
		from$tab[,c(col):=lapply(colIdx, function(...) numeric(1))]		

		for (matN in from$matDim[[1]]) {
			from$tab[.(matN), c(col):=as.data.frame(from$mat[[matN]][,colIdx,drop=FALSE])]
		}		


	} else if (any(col %in% sfDataType)) {

		dtIdx <- col %in% sfDataType
		dtCols <- col[dtIdx]
		col <- col[!dtIdx]
		for (dtCol in dtCols) {
			col <- c(col, from$aid$dataType[[dtCol]])
		}
	}

	if (fromData=="map" && toData=="par") {

		if (is.null(toDataType)) { stop("Specify target dataType in 'toDataType'") }
		if (!toDataType%in%sfDataTypes) { stop(sprintf("Only %s data types supported", paste(sfDataTypes, collapse=", "))) }		

		from$aid$dataType[[toDataType]] <- col

	}

	keys <- findKey(from$tab, to$tab, key, getKeys=TRUE)

	if (is.character(keys)) {
		keys <- list(key=keys)
		keysCon <- TRUE
	} else {
		keysCon <- match(keys$key, keys$xKey)<=match(keys$key, keys$yKey)
	}

	if (keysCon) {
		
		if (!is.null(filter)) {
			if (!is.logical(filter)) { stop("Provide logical vector for filter") }
			if (length(filter)!=nrow(to)) { stop(sprintf("filter length and '%s' number of rows mismatch", deparse(substitute(to)))) }	
			filter <- as.integer(filter)
			to$tab <- tab(to, addRow=TRUE)
		}
		
		ncatn(sprintf("Merging: key = \"%s\"...", keys$key), verbose=verbose)
		
		if (any(col %in% colnames(to))) {
			eraseCol <- intersect(col, colnames(to))
			to <- variable(to, eraseCol, NULL)
		}

		toCol <- merge(to, semiTableWrap(from$tab[, c(keys$key, col), with=FALSE], findDataType(from, col)), key=keys$key)

		if (!is.null(filter)) {
			ncatn(sprintf("Applying filter to '%s'...",paste(col, collapse="', '")), verbose=verbose)
			filter <- filter[toCol$tab[[tmName$allRow]]]
			for (colName in col) {
				toCol$tab[,c(colName):=list(eval(as.name(colName))*filter)]
			}
			toCol$tab[,c(tmName$allRow):=list(NULL)]
		}

	} else {

		if (!is.null(filter)) { ncatn("filter N/A") }

		ncatn(sprintf("Degrading: '%s' to '%s', method = %s...", deparse(substitute(from)), keys$key, deparse(substitute(method))), verbose=verbose)

		fun <- method
		if (is.character(method)) {
			if (method=="majority") {
				fun <- function(subCol, na.rm) {
					subColHF <- as.data.frame(table(subCol), stringsAsFactors=FALSE)
					subColHF <- subColHF[which.max(subColHF[,2]),1]
					subColHF <- as(subColHF, class(subCol))
					return(subColHF)
				}
			}	
		}

		fromDegr <- from$tab[, c(keys$key, col), with=FALSE]

		for (colName in col) {
			if (is.integer(fromDegr[[colName]])) { fromDegr[,colName:=list(as.numeric(fromDegr[[colName]]))] }
		}

		fromDegr <- fromDegr[,lapply(.SD, function(subCol) { return(do.call(fun, list(subCol, na.rm=TRUE))) } ), 
							by=c(keys$key), .SDcols=c(col)]

		ncatn(sprintf("Merging: key = \"%s\"...", keys$key), verbose=verbose)
		
		if (any(col %in% colnames(to))) {
			eraseCol <- intersect(col, colnames(to))
			to <- variable(to, eraseCol, NULL)
		}
		
		toCol <- merge(to, semiTableWrap(fromDegr, findDataType(from, col)), key=keys$key)
	}

	to$tab <- toCol$tab
	to$aid$dataType <- dataType(toCol)
	
	if (is.semiFrame(to)) {
		to$mat <- toCol$mat
		to$matDim <- toCol$matDim
	}
	
	if (toData=="map") {

		for (matN in to$matDim[[1]]) {

			addCol <- as.matrix(to$tab[.(matN),col,with=FALSE])
			colnames(addCol) <- NULL
			to$mat[[matN]] <- cbind(to$mat[[matN]], addCol)
			
		}		

		to$tab[,c(col):=list(NULL)]
	}

	return(to)
}

#' Restore Values 
#'
#' @description Restore values prior normalization if relevant.
#' 
#' @template stsfobjParam 
#' @template verbose 
#' 
#' @return Copy of the \code{semiTable} or \code{semiFrame} object with restored values. 
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Restore values
#' deNorm(pcy)
#' 
#' @export
deNorm <- function(obj, verbose=TRUE) {

	normTab <- normTable(obj, verbose=verbose)
	if (is.null(normTab)) { return(obj) }
	
	if (names(normTab)[1]=="norm") {
		
		normTab <- normTab[[1]]
		objTab <- tab(obj)
		ncatn("Reversing normalization", verbose=verbose)
		for (j in 1:nrow(normTab)) {
			normCol <- normTab[j, parameter]
			colData <- objTab[[normCol]]*normTab[j, spread]+normTab[j, center]
			objTab[, c(normCol):=list(colData)]
		}
		obj$aid$normalize <- NULL
	} else {
		
		normTab <- normTab[[1]]
		objTab <- tab(obj)
		ncatn("Reversing magnitude correction", verbose=verbose)
		for (j in 1:nrow(normTab)) {
			magCol <- normTab[j, parameter]
			magMag <- 10^normTab[j, magnitude]
			objTab[, c(magCol):=list(objTab[[magCol]]*magMag)]
		}
		obj$aid$magnitude <- NULL
	}
	obj$tab <- objTab

	return(obj)
}

#' Yield Calculation 
#'
#' @description Creates yield variable from maps ie. number of good dice divided by PDPW.
#' 
#' @template sfobjParam
#' @param fromBin Character. Defines a specific bin or bins for yield calculation ie. which
#' bins constitute bad dice. Use keyword "all" (default) for all the bins.
#' @param allName Character. Specifies a variable name for the yield column for the case when \code{fromBin} equals "all" (default: "ProbeYield").
#' @param binName Character. User defined variable name for the yield column (default: \code{NULL}).
#' @param binNameRoot Character. Determines the root part of the variable name for the yield column (default: "Yield").
#' @param digits Integer. Specifies number of digits to keep (default: 2).
#' @param pdpwCol Character. Specifies the denominator PDPW column name (default: "pdpw").
#'
#' @return Copy of the \code{semiFrame} object with yield.
#' 
#' @examples
#' 
#' data(wbm)
#' 
#' # Create yield for bins 2 and 5 with default variable name
#' yield(wbm, c(2,5))
#' 
#' # Create yield for bin 2 with user defined name root and 3 digits rounding
#' yield(wbm, 2, binNameRoot="Y", digits=3)
#' 
#' @export
yield <- function(obj, fromBin="all", allName="ProbeYield", binName=NULL, binNameRoot="Yield", 
					digits=2, pdpwCol="pdpw") {

	objTab <- tab(obj)
	col <- allName
	if (!any(fromBin=="all")) {
		if (is.null(binName)) {
			col <- paste0(binNameRoot, "B", paste(fromBin,collapse="B"))
		} else {
			col <- binName
		}
	}

	for (matN in obj$matDim[[1]]) {
		
		matData <- obj$mat[[matN]]
		saveDim <- dim(matData)
		matData <- as.vector(matData)
		matData[which(matData<0)] <- 0
		if (!any(fromBin=="all")) {
			matData[which(!matData%in%fromBin)] <- 0
		} 
		matData[which(matData>0)] <- 1
		dim(matData) <- saveDim
		pdpw <- objTab[.(matN)][[pdpwCol]]
		yields <- round(100*(pdpw-rowSums(matData))/pdpw, digits)
		objTab[.(matN), c(col):=list(yields)]
	}
	
	obj$tab <- objTab
	yDataType <- list()
	yDataType[[sfDataType$y]] <- col
	obj$aid$dataType <- mergeDataTypeRef(obj$tab, dataType(obj), yDataType)

	return(obj)
}

#' Calculates and transfers site yield from maps 
#'
#' Calculates and transfers site yield from maps
#' @export
siteYield <- function(from, to, fromBin="all", allName="PCMYield", binName=NULL, binNameRoot=allName, 
						digits=2, siteMap=NULL, nbrhdIdx=NULL, siteMapSignal=NULL) {


	siteYieldChunk <- function(from, to, fromBin, nbrhdIdx, allName, binName, binNameRoot, digits) {

		from <- copy(from)
		from <- bin(from, fromBin)
		fromTab <- tab(from)
		fromTab[, c(names(nbrhdIdx)):=list(numeric(1))]

		for (matN in from$matDim[[1]]) {
			for (nbrhd in names(nbrhdIdx)) {
				nbrhdY <- length(nbrhdIdx[[nbrhd]])-rowSums(from$mat[[matN]][,nbrhdIdx[[nbrhd]]])
				nbrhdY <- round(100*nbrhdY/length(nbrhdIdx[[nbrhd]]), digits)
				fromTab[.(matN), c(nbrhd):=list(nbrhdY)]
			}
		}

		fromTab <- fromTab[, c(sfName$lotWafer, names(nbrhdIdx)), with=FALSE]
		mergeTab <- setnames(data.table(rep(fromTab[[sfName$lotWafer]], each=length(nbrhdIdx)),
						 rep(names(nbrhdIdx), nrow(fromTab))), c(sfName$lotWafer, sfName$diexy))
		mergeTabY <- t(as.matrix(fromTab[, names(nbrhdIdx), with=FALSE]))
		mergeTabY <- as.vector(mergeTabY)

		col <- allName
		if (!any(fromBin=="all")) {
			if (is.null(binName)) {
				col <- paste0(binNameRoot, "B", paste(fromBin,collapse="B"))
			} else {
				col <- binName
			}
		}

		mergeTab[, c(col):=list(mergeTabY)]
		mergeTabDataType <- list()
		mergeTabDataType[[sfDataType$y]] <- col
		return(merge(to, semiTableWrap(mergeTab, mergeTabDataType), key=c(sfName$lotWafer, sfName$diexy)))
	}

	
	if (is.null(nbrhdIdx)) { nbrhdIdx <- to$aid$siteMap$aid$nbrhdIdx }
	if (is.null(nbrhdIdx)) { stop("Provide pcm neighborhood indexes") }
	
	if (length(nbrhdIdx)>1) {
		
		if (is.null(siteMap)) { siteMap <- to$aid$siteMap }
		if (is.null(siteMap)) { stop("Provide siteMap") }
 
		if (is.null(siteMapSignal)) { siteMapSignal <- siteMap$aid$siteMapSignal }
		if (is.null(siteMapSignal)) { stop("Provide siteMapSignal") }

		siteTo <- tableListWrap()
		for (i in 1:nrow(siteMap)) {
			subFrom <- from[eval(as.name(siteMapSignal))==siteMap[[siteMapSignal]][i]]
			if (!nrow(subFrom)) { next }
			siteTo <- rbind(siteTo, siteYieldChunk(from=subFrom, to=to, 
								fromBin=fromBin, nbrhdIdx=nbrhdIdx[[siteMap[,nbrhdIdx][i]]], allName=allName, 
								binName=binName, binNameRoot=binNameRoot, digits=digits))
		}
		return(siteTo)
	} else {
		return(siteYieldChunk(from=from, to=to, fromBin=fromBin, nbrhdIdx=nbrhdIdx[[1]], 
					allName=allName, binName=binName, binNameRoot=binNameRoot, digits=digits))
	}

}

#' Manage Variables
#'
#' @description Adds, modifies or removes a variable in a \code{semiTable} or \code{semiFrame} object.
#' 
#' @template stsfobjParam
#' @param varName Character. Specifies variable's name. The variable will be added if not present.
#' @param varData Vector. Specifies variable's data. Use \code{NULL} to remove a variable.
#' @param dataType Character. Defines the data type of a variable. Mandatory for new variables. 
#' See \code{\link{dataType}} for more information on data types. (default: \code{NULL})
#'
#' @seealso \code{\link{dataType}} for available data types. 
#' 
#' @return Copy of the \code{semiTable} or \code{semiFrame} object.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Introduce a new variable myPARAM
#' pcy2 <- variable(pcy, "myPARAM", 2*pcy[,PARAM02]-pcy[,PARAM03]/2, "PC")
#' 
#' # Modify myPARAM variable
#' pcy2 <- variable(pcy2, "myPARAM", 2*pcy[,PARAM02])
#' 
#' # Remove 'myPARAM'
#' pcy2 <- variable(pcy2, "myPARAM", NULL)
#' 
#' 
#' @export
variable <- function(obj, varName, varData, dataType=NULL) {

	if (!varName%in%colnames(obj) && !any(varName %in% sfDataType) && is.null(dataType)) { stop("Provide dataType for a new variable") } 
	
	objTab <- tab(obj)
	
	if (any(varName %in% sfDataType)) {

		varName <- dataType(obj)[[varName]]		
	}

	if (is.null(varData)) { varData <- lapply(seq_along(varName), function(i) return(NULL) ) }
	if (!is.list(varData)) { varData <- list(varData) } 
	objTab[, c(varName):=varData]

	objDataType <- list()
	for (i in 1:length(varName)) {

		if (is.null(varData[[i]])) {

			if (varName[i] %in% colnames(obj)) {
				dType <- findDataType(obj, varName[i]) 
				obj$aid$dataType[[names(dType)]] <- obj$aid$dataType[[names(dType)]][-match(varName[i], 
														obj$aid$dataType[[names(dType)]])]
				if (!length(obj$aid$dataType[[names(dType)]])) {
					obj$aid$dataType[[names(dType)]] <- NULL
				}
			}

		} else {
			
			if (is.null(dataType)) { 
				dType <- findDataType(obj, varName[i]) 
			} else {
				dataType <- toupper(dataType)
				if (!dataType[i]%in%sfDataTypes) { stop(sprintf("Only %s data types supported", paste(sfDataTypes, collapse=", "))) }
				dType <- list()
				dType[[dataType[i]]] <- varName[i]
			} 
			
			if (names(dType)%in%names(objDataType)) {
				objDataType[[names(dType)]] <- c(objDataType[[names(dType)]], dType[[names(dType)]])
			} else {
				objDataType[[names(dType)]] <- dType[[names(dType)]]			
			}			
		}
	}	

	objDataType <- mergeDataTypeRef(objTab, dataType(obj), objDataType)

	obj$tab <- objTab
	obj$aid$dataType <- objDataType
	return(obj)
}

#' Rename Variables
#'
#' @description Renames variables (columns).
#' 
#' @template stsfobjParam
#' @param old Character. Specifies a single variable name or group of variable names to be renamed.
#' @param new Character. Specifies new variable name or names to be set correspondingly.
#' 
#' @return Copy of the \code{semiTable} or \code{semiFrame} object.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Set the cluster name from "clear" to "clear2"
#' pcy2 <- rename(pcy, "clear", "clear2")
#' 
#' # Rename multiple variables
#' pcy2 <- rename(pcy, c("clear", "center", "PARAM01"), c("clear2", "center2", "PARAM99"))
#'  
#' @export
rename <- function(obj, old, new) {

	if (length(old) != length(new)) {
		stop("Lengths of 'old' and 'new' do not match.")
	}

	if (length(old) != length(intersect(old, colnames(obj)))) {
		stop("Some of the variables specified in 'old' were not found.")
	}

	obj <- copy(obj)
	setnames(obj$tab, old, new)

	for (dType in names(obj$aid$dataType)) {
		sourceIdx <- which(old %in% obj$aid$dataType[[dType]])
		if (!length(sourceIdx)) { next }
		targetIdx <- which(obj$aid$dataType[[dType]] %in% old)
		obj$aid$dataType[[dType]][targetIdx] <- new[sourceIdx]
	}

	return(obj)
}

#' Unfold Column
#' 
#' @description Unfolds a column with categorical variable horizontally into groups of variables.
#' 
#' @template stsfobjParam
#' @param col Character. Column name of the categorical variable to unfold.
#' @param method Character. Defines method to be used for unfolding: "merge" (default) - only cases with all the categories covered will be kept, 
#'   "add" - all cases will be kept with the missing categories filled as \code{NA}.
#' @param key Character. Specifies merging key to be used for unfolding (default: "default").
#' @param dataType Character. Data type of variables that will be used for unfolding. 
#'                 Each variable will spawn a new variable for each category (default: \code{NULL}).
#' @param targetDataType Character. Data type of newly spawned variables. If not specified the same data type will be applied (default: \code{NULL}).
#' @param discardCol Character. Specifies column name to be discarded (default: \code{NULL}). 
#' @param moveCol Character. Specifies column name to be moved before the unfolded columns (default: \code{NULL}). 
#' @param sep Character. Specifies separator used for new column name creation (default: "_").
#' 
#' @return Copy of the semiTable or semiFrame object with new variable groups.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Unfold siteid column to create a group of PC params for each site
#' pcyu <- unfoldCol(pcy, "siteid", method="merge", key="lotWafer", dataType="PC", discardCol=c("siteid", "lotWaferSiteid"))
#' 
#' @export
unfoldCol <- function(obj, col, method="merge", key="default", dataType=NULL, 
						targetDataType=NULL, discardCol=NULL, moveCol=NULL, sep="_") {

	if (is.null(dataType)) {
		dataType <- setdiff(names(dataType(obj)), sfDataType$meta)
	}

	if (is.null(targetDataType)) {
		targetDataType <- dataType[1]
	}

	colFactors <- unique(obj[[col]])
	objUnfold <- NULL

	unfoldedColNamesTotal <- character()
	for (colFactor in colFactors) {

		objFactor <- obj[eval(as.name(col))==colFactor]

		unfoldedColNames <- character()
		for (dType in dataType) {
			colName <- dataType(objFactor)[[dType]]
			unfoldedColName <- paste(colFactor, colName, sep=sep)
			setnames(objFactor$tab, colName, unfoldedColName)
			dataType(objFactor)[[dType]] <- unfoldedColName	
			unfoldedColNames <- c(unfoldedColNames, unfoldedColName)
		}
		unfoldedColNamesTotal <- c(unfoldedColNamesTotal, unfoldedColNames)

		if (is.null(objUnfold)) {
			objUnfold <- objFactor
			next
		}


		key <- findKey(objFactor, objFactor, key=key)
		objFactorMerge <- objFactor[,c(key, unfoldedColNames),with=FALSE]
		objUnfoldMerge <- merge(objUnfold, objFactorMerge, key=key)

		if (method=="add") {
			keyMergeDiff <- setdiff(objUnfold[[key]], objUnfoldMerge[[key]])
			objUnfold <- rbind(objUnfold[eval(as.name(key))%in%keyMergeDiff], objUnfoldMerge, fill=TRUE)
			
			keyMergeDiff <- setdiff(objFactorMerge[[key]], objUnfoldMerge[[key]])
			objUnfold <- rbind(objUnfold, objFactor[eval(as.name(key))%in%keyMergeDiff], fill=TRUE)
			
		} else {

			objUnfold <- objUnfoldMerge
		}
	}

	if (!is.null(discardCol)) {
		objUnfold <- variable(objUnfold, discardCol, lapply(seq_along(discardCol),function(i) return(NULL)))
	}

	if (method=="add") {
	 	objUnfold$aid$dataType[[targetDataType]] <- unfoldedColNamesTotal
	}

	if (!is.null(moveCol)) {
		for (i in seq_along(moveCol)) {
			colShiftRef(objUnfold$tab, moveCol[i], dataType(objUnfold)[[targetDataType]][1])
			colDataType <- names(findDataType(objUnfold, moveCol[i]))[1]
			objUnfold$aid$dataType[[colDataType]] <- setdiff(objUnfold$aid$dataType[[colDataType]], moveCol[i])
			objUnfold$aid$dataType[[targetDataType]] <- c(moveCol[i], objUnfold$aid$dataType[[targetDataType]])
		}
	}

	return(objUnfold)
}

#' Remove Columns with NAs
#'
#' @description Removes columns with NAs using a cutoff ratio.
#' 
#' @template stsfobjParam
#' @param ratio Specifies threshold for column removal with NA values (default: 0.9).
#' @template verbose
#'
#' @return Copy of the \code{semiTable} or \code{semiFrame} object.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' pcy2 <- variable(pcy, "myPARAM", NA ,"PC")
#' pcy2 <- remColNA(pcy2)
#' 
#' @export
remColNA <- function(obj, ratio=0.9, verbose=TRUE) {

	colNA <- 1-sapply(obj$tab, function(col) sum(is.na(col)))/nrow(obj)
	idx <- which(colNA<=ratio)
	if (!length(idx)) {
		ncatn("No columns removed", verbose=verbose)
		return(obj)
	}
	colNA <- names(colNA[idx])
	ncatn(verbose=verbose)
	ncatn("Removing column(s):", verbose=verbose)
	ncatn(paste(colNA, collapse=", "), verbose=verbose)
	ncatn(verbose=verbose)
	ncatn(sprintf("%d column(s) removed.",length(colNA)), verbose=verbose)
	ncatn(verbose=verbose)
	return(variable(obj, colNA, NULL))
}


#' Columns Correlation
#' 
#' @description Returns correlation between two columns with grouping.
#' 
#' @template stsfobjParam
#' @param col1 Character. Specifies the first column name (of a pair) for mutual 
#' correlation assessment.
#' @param col2 Character. Specifies the second column name (of a pair) for mutual 
#' correlation assessment.
#' @param by Character. Defines grouping in the output table (default: "lotWafer"). 
#' @param outputOrder Character. Defines sorting column in the output table:
#' \itemize{ 
#'   \item{ "gcor" }{ Grouped correlation value. }
#'   \item{ "pval" }{ p-value for given specified \code{alternative}. } 
#'   \item{ "cint" }{ Left bound of confidence interval given \code{confLevel} (default). }
#' }
#' @param alternative Character. Indicates the alternative hypothesis and must be one of
#' "two.sided", "greater" or "less" (\code{pval} in the output table) (default: "g").
#' @param confLevel Numeric. Confidence level for the returned confidence interval (\code{cint} in the output table) (default: 0.95).
#' 
#' @return A \code{data.table} with correlation statistics.
#' 
#' @examples
#' 
#' data(pcy)
#' 
#' # Introduce a new variable myPARAM
#' pcy2 <- variable(pcy, "myPARAM", 2*pcy[,PARAM02]-pcy[,PARAM03]/2, "PC")
#' # Find out correlation of the new variable with PARAM10
#' colCor(pcy2, "myPARAM", "PARAM10", by = "lotWafer")
#' 
#' @export
colCor <- function(obj, col1, col2, by="lotWafer", outputOrder="cint", alternative="g", confLevel=0.95) {

	rowCorTab <- obj$tab[,.(gcor=cor(eval(as.name(col1)), eval(as.name(col2))), 
					pval=cor.test(eval(as.name(col1)), eval(as.name(col2)), alternative=alternative)$p.val, 
					cint=cor.test(eval(as.name(col1)), eval(as.name(col2)), conf.level=confLevel)$conf.int[1]),by=by]

	return(rowCorTab[order(-eval(as.name(outputOrder)))])
}

#
# End
# 
