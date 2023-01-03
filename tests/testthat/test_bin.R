library(testthat)
library(inLabZero)

context("fun 'bin' check")

test_that("extract all", {
      
	val <- wbm$mat[[1]][1,,drop=TRUE]
	val[which(val>0)] <- 1
	expect_identical(sum(bin(wbm[1], "all", 1)$mat[[1]]==1), sum(val==1))

})
