library(testthat)
library(inLabZero)

context("fun 'cropMap' check")


test_that("dim check", {

	wbmCrop <- cropMap(wbm[1], mapBorder=c(1,-1, 1, -1))

	wbmDim <- as.matrix(matDim(wbm))[,-1]
	wbmCropDim <- as.matrix(matDim(wbmCrop))[,-1]

	expect_identical(wbmDim + c(2, -2), wbmCropDim)

})
