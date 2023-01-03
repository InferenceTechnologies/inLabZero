#
# Inference Technologies 
# http://inferencetech.com
#
# pkg inLabZero
#
# Inference Technologies
#
# Utils
# 
# 0.27.2
# 


# function or vector for plotPar
funVec <- function(obj, ...) {

	if (is.function(obj)) { return(do.call(obj, list(...))) }
	return(obj)
}

# numeric table
tableNum <- function(vec, decreasing=FALSE) {

	rleV <- rle(sort(vec,decreasing=decreasing))
	return(data.table(num=rleV$values, n=rleV$length))
}

# renindexing for reorder
reIdx <- function(idx) {
	
	reidx <- integer(length(idx))
	reidx[idx] <- seq_along(idx)
	return(reidx)
}

# search in strings
searchStr <- function(look, str, exclude=NULL, mode=c("&","|")) {

	lookUp <- !logical(1)
	for (i in seq_along(look)) {
		lookUp <- do.call(mode[1], list(lookUp, grepl(look[i],str)))
	}

	lookUp <- which(lookUp)

	if (!is.null(exclude)) {
		lookUpX <- logical(1)
		if (mode[2]=="&") { lookUpX <- !lookUpX }
		for (i in seq_along(exclude)) {
			lookUpX <- do.call(mode[2], list(lookUpX, grepl(exclude[i],str)))
		}
		lookUp <- setdiff(lookUp, which(lookUpX))
	}

	return(lookUp)
}

# returns positive factor of blockIdx list
positiveFactor <- function(idxList) {
	fac <- integer()
	for (i in seq_along(idxList)) {
		fac <- c(fac, rep(i,idxList[[i]][2]-idxList[[i]][1]+1))
	}
	return(fac)
}

# returns block indices of TRUE values
blockIdx <- function(vec, posFactor=FALSE) {

	vecLen <- length(vec)
	idxVec <- which(vec)
	idxNVec <- which(!vec)
	idxList <- list()

	i <- 1
	while (length(idxVec)) {
		blockStart <- idxVec[1]
		idxNVec <- idxNVec[which(idxNVec>=blockStart)]
		if (length(idxNVec)) {
			blockEnd <- idxNVec[1] - 1
		} else {
			blockEnd <- vecLen
		}
		idxVec <- idxVec[-which(idxVec<=blockEnd)]
		idxList[[i]] <- c(blockStart, blockEnd)
		i <- i + 1	
	}

	if (posFactor) {
		return(positiveFactor(idxList))
	}

	return(idxList)
}

# draws halo around text
shadowText <- function(x, y=NULL, labels, col='white', bg='black', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {

    xy <- xy.coords(x,y)
    xo <- r*strwidth('A')
    yo <- r*strheight('A')

    # draw background text with small shift in x and y in background colour
    for (i in theta) {
        text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
    }
    # draw actual text in exact xy position in foreground colour
    text(xy$x, xy$y, labels, col=col, ... )
}

#
# End
#