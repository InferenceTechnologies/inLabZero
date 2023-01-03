#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# sfLoad
# 
# 0.82.2
# 

#
# Imports
#

#
# Constants
#

#
# Classes Constructors
#

#
# Generics
#

#
# Methods Functions
#

filePath <- function(...) {
	path <- file.path(...)
	return(gsub(sprintf("[%s]+", .Platform$file.sep), .Platform$file.sep, path))
}

completeFileName <- function(fileName, ext="rda", tech=NULL, repo="dev", ver="latest", 
								subVer=NULL, user=NULL, path=NULL) {

	if (!grepl(paste0("[.]",ext), fileName)) { fileName <- paste0(fileName,".",ext) }
	path <- completePath(tech=tech, repo=repo, ver=ver, subVer=subVer, user=user, path=path, fileName=fileName)
	if (!is.null(path)) { fileName <- filePath(path,fileName)	}
	return(fileName)
} 


completePath <- function(tech, repo, ver, subVer, user, path, fileName) {


	checkPath <- function(dataPath, subPath=NULL, tryLower=TRUE, silent=FALSE) {

		if (is.null(dataPath)) { return(NULL) }
		if (is.null(subPath)) {
			path <- dataPath
			err <- !file.exists(path)
		} else {
			path <- filePath(dataPath, subPath)
			err <- !file.exists(path)
			if (!length(err)) { err <- !logical(1) }
			if (err&tryLower) {
				path <- checkPath(filePath(dataPath, tolower(subPath)), silent=silent)
				err <- logical(1)
			}
		}

		if (!length(err)) { err <- !logical(1) }
		if (err) {
			if (silent) {
				path <- NULL
			} else {
				stop(sprintf("Path %s not found", path))		
			}
		}

		return(path)
	}

	siteUserPath <- function(user, tech=NULL) {


		if (user %in% cfgSite$user) {

			if (is.null(tech)) { stop("Specify technology") }
			return(filePath(cfgSite$data, tolower(filePath(tech, "usr", user))))
		}

		if (user %in% cfgSite$userRStudio) {

			return(checkPath(filePath(cfgSite$dataUserRStudio, user), tech))
		}

		stop("Uknown user")
	}
	
	if (!is.null(path)) {
		path <- checkPath(path)
		return(path)
	}

	if (is.null(user)) {
		
		if (is.null(tech)) { stop("Specify technology") }
		path <- cfgSite$data
		path <- checkPath(path, filePath(tech, repo))
		if (ver=="latest") { 

			vers <- rev(list.dirs(path, full.names=FALSE, recursive=FALSE))
			for (ver in vers) {

				tryPath <- checkPath(path, ver, silent=TRUE)
				tryPath <- checkPath(tryPath, subVer, silent=TRUE)
				tryPath <- checkPath(tryPath, fileName, tryLower=FALSE, silent=TRUE)
				if (is.null(tryPath)) { next }
				break
			}

			if (is.null(tryPath)) { 
				ver <- vers[1]
			}

			ncatn(sprintf("ver = %s", ver))
		}
	} else {

		path <- siteUserPath(user, tech)
		if (ver=="latest") { ver <- NULL }
	}
	path <- checkPath(path, ver)
	path <- checkPath(path, subVer)

	return(path)
}

objectSize <- function(object, msg="size %.3f MB") {

	return(sprintf(msg, unclass(object.size(object))/1048576))
}

#
# Classes Generics Methods
#

#' Load and Save Data
#'
#' @description Loading and saving data.
#' 
#' @param fileName Character. Specifies name of the file to be loaded or saved.
#' @param tech Character. Specifies name of the technology corresponding to the data (default: \code{NULL}).
#' @param repo Character. Defines the name of the data repository (default: "dev").
#' @param ver Character. Data version. Use keyword "latest" for latest version available. (default: "latest").
#' @param subVer Character. Additional information on data version (default: \code{NULL}).
#' @param user Character. Specifies the \code{inLab} account username (default: \code{NULL}).
#' @param path Character. Use to enter complete file path instead of \code{tech}, \code{repo}, \code{ver}, \code{subVer} combination (default: \code{NULL}).
#' @param ext Character. Sets the file extension (default: "rda").
#' @template verbose 
#' 
#' @return Returns the object after loading.
#' 
#' @examples
#' 
#' \dontrun{# Load an object
#' pcy <- loadData("pcy", tech="TECH")}
#' @rdname loadSaveData
#' @export
loadData <- function(fileName, tech=NULL, repo="dev", ver="latest", subVer=NULL, 
							user=NULL, path=NULL, ext="rda", verbose=TRUE) {
	
	ncatn("Loading...", k1=1, verbose=verbose)
	fileName <- completeFileName(fileName=fileName, ext=ext, tech=tech, repo=repo, ver=ver, 
									subVer=subVer, user=user, path=path)
	envir <- environment()
	objName <- load(fileName, envir=envir)
	ncatn("Object ", objName, ", ", objectSize(eval(as.name(objName))), ", loaded.", sep="", k2=2, verbose=verbose)
	return(envir[[objName]])
}

#' @param obj Specifies the object to be saved.
#' @param envir Environment containing the \code{obj}. (default: \code{.GlobalEnv}).
#' @examples
#' \dontrun{# Save an object 
#' data(pcy)
#' saveData(pcy, tech="TECH")}
#' @rdname loadSaveData
#' @export
saveData <- function(obj, tech=NULL, repo="dev", ver="latest", subVer=NULL, user=NULL, 
						path=NULL, fileName=NULL, ext="rda", envir=.GlobalEnv, verbose=TRUE) {

	ncatn("Compressing and saving...", k1=1, verbose=verbose)
	objName <- deparse(substitute(obj))
	if (is.null(fileName)) { fileName <- objName }
	fileName <- completeFileName(fileName=fileName, ext=ext, tech=tech, repo=repo, ver=ver, 
									subVer=subVer, user=user, path=path)
	save(list=objName, file=fileName, envir=envir, ascii=FALSE, compress=cfgSite$compress, compression_level=cfgSite$compressLevel)
	ncatn("Object ", objName, ", ", objectSize(obj), ", saved.", sep="", k2=2, verbose=verbose)
	return(invisible())
}

#
# Standard Generics Methods
#

#' @export
is.srcData <- function(obj) {
	return(inherits(obj, "srcData"))
}

bind.srcData <- function(...){

	args <- list(...) 
	obj <- args[[1]]
	args <- args[-1]

	for (i in seq_along(args)) {

		if (!is.srcData(obj)) { next }
		if (all(obj$dataType==args[[i]]$dataType)) {
			obj$data <- c(obj$data, args[[i]]$data)
		}
	}

	return(obj)
}

#
# Functions
#