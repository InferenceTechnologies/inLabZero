library(testthat)
library(inLabZero)

context("fun 'defDie' check")


test_that("first row check", {
	
	data(wbm)    
	data(dem)    

	useStep <- unique(dem[,step])[1]
	lw <- intersect(wbm[,lotWafer], dem[step==useStep,lotWafer])[1]
	binMap <- wbm[lotWafer==lw]
	defMap <- cropMap(dem[lotWafer==lw], dem$aid$demAlign, verbose=FALSE)

	binMapRow <- getRowMat(binMap, 1, 1)[1,]
	defMapRow <- getRowMat(defMap, 1, 1)[1,]
	dieNum <- which(binMapRow>0)

	ded <- defDie(wbm, dem)
	dedRow <- ded[step==useStep&lotWafer==lw&die%in%dieNum, .(die, bin, ALL)][order(+die)]

	expect_identical(dedRow[,bin], binMapRow[dieNum])
	expect_identical(dedRow[,ALL], defMapRow[dieNum])

})
